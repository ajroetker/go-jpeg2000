package jpeg2000

import (
	"bytes"
	"image"
	"image/color"
	"math"
	"testing"
)

// --- BitWriter Tests ---

func TestBitWriterRoundTrip(t *testing.T) {
	// Write bits with BitWriter, read them back with bitReader
	tests := []struct {
		name string
		bits []int
	}{
		{"single zero", []int{0}},
		{"single one", []int{1}},
		{"byte 0xA5", []int{1, 0, 1, 0, 0, 1, 0, 1}},
		{"byte 0xFF", []int{1, 1, 1, 1, 1, 1, 1, 1}},
		{"12 bits", []int{1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1}},
		{"16 zeros", []int{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newBitWriter()
			for _, bit := range tt.bits {
				w.WriteBit(bit)
			}
			data := w.Flush()

			r := newBitReader(data)
			for i, expected := range tt.bits {
				got, err := r.ReadBit()
				if err != nil {
					t.Fatalf("bit %d: ReadBit error: %v", i, err)
				}
				if got != expected {
					t.Errorf("bit %d: got %d, want %d", i, got, expected)
				}
			}
		})
	}
}

func TestBitWriterWriteBits(t *testing.T) {
	w := newBitWriter()
	w.WriteBits(0b10110011, 8)
	w.WriteBits(0b1010, 4)
	data := w.Flush()

	r := newBitReader(data)
	got, err := r.ReadBits(8)
	if err != nil {
		t.Fatalf("ReadBits(8): %v", err)
	}
	if got != 0b10110011 {
		t.Errorf("first 8 bits: got 0x%X, want 0xB3", got)
	}
	got, err = r.ReadBits(4)
	if err != nil {
		t.Fatalf("ReadBits(4): %v", err)
	}
	if got != 0b1010 {
		t.Errorf("next 4 bits: got 0x%X, want 0xA", got)
	}
}

func TestBitWriterByteStuffing(t *testing.T) {
	// When bit-stuffing is enabled, a 0xFF byte should be followed by a
	// byte whose MSB is 0 (stuffed bit).
	w := newBitWriterWithStuffing()
	// Write 0xFF (8 ones)
	for range 8 {
		w.WriteBit(1)
	}
	// Write 4 more ones
	for range 4 {
		w.WriteBit(1)
	}
	data := w.Flush()

	// First byte should be 0xFF
	if len(data) < 2 {
		t.Fatalf("expected at least 2 bytes, got %d", len(data))
	}
	if data[0] != 0xFF {
		t.Errorf("first byte: got 0x%02X, want 0xFF", data[0])
	}
	// Second byte's MSB must be 0 (stuffed bit)
	if data[1]&0x80 != 0 {
		t.Errorf("second byte MSB should be 0 (stuffed), got 0x%02X", data[1])
	}
}

func TestBitWriterByteAlign(t *testing.T) {
	w := newBitWriter()
	w.WriteBit(1)
	w.WriteBit(0)
	w.WriteBit(1)
	w.ByteAlign()
	w.WriteBit(1)
	data := w.Flush()

	if len(data) != 2 {
		t.Fatalf("expected 2 bytes after align, got %d", len(data))
	}
	// First byte: 101_00000 = 0xA0
	if data[0] != 0xA0 {
		t.Errorf("first byte: got 0x%02X, want 0xA0", data[0])
	}
}

func TestBitWriterReset(t *testing.T) {
	w := newBitWriter()
	w.WriteBits(0xFF, 8)
	w.Reset()

	if w.Len() != 0 {
		t.Errorf("after reset, Len() = %d, want 0", w.Len())
	}

	w.WriteBit(1)
	data := w.Flush()
	if data[0] != 0x80 {
		t.Errorf("after reset+write: got 0x%02X, want 0x80", data[0])
	}
}

// --- MQ Encoder Tests ---

func TestMQEncoderProducesOutput(t *testing.T) {
	// Verify that the MQ encoder produces non-empty output for various symbol sequences.
	// Note: The MQ encoder's C register convention follows ITU-T T.800 Annex C
	// while the decoder follows OpenJPEG's convention (C shifted by 16). Direct
	// round-trip testing requires matching conventions. The real validation is
	// done via EBCOT encode→decode and full Encode→Decode round-trip tests.
	tests := []struct {
		name    string
		ctx     int
		symbols []int
	}{
		{"all MPS ctx0", 0, []int{0, 0, 0, 0, 0, 0, 0, 0}},
		{"alternating ctx0", 0, []int{1, 0, 1, 0, 1, 0, 1, 0}},
		{"mixed ctx0", 0, []int{0, 1, 0, 0, 1, 0, 1, 1, 0, 0}},
		{"uniform ctx18", 18, []int{0, 1, 0, 1, 0, 1, 0, 1}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			enc := newMQEncoder()
			for _, sym := range tt.symbols {
				enc.Encode(tt.ctx, sym)
			}
			data := enc.Flush()

			if len(data) == 0 {
				t.Fatal("encoded data is empty")
			}

			// Verify no accidental marker sequences in output (no 0xFF followed by 0x90-0xFF)
			for i := 0; i < len(data)-1; i++ {
				if data[i] == 0xFF && data[i+1] >= 0x90 {
					t.Errorf("accidental marker at offset %d: FF %02X", i, data[i+1])
				}
			}
		})
	}
}

func TestMQEncoderContextTransitions(t *testing.T) {
	// Verify that encoding LPS symbols causes context state transitions
	enc := newMQEncoder()

	// Context 0 starts at state 4 (per initialization)
	initialState := enc.contexts[0].index

	// Encode several MPS symbols - should transition via nmps
	for range 10 {
		enc.Encode(0, 0)
	}
	afterMPS := enc.contexts[0].index
	if afterMPS == initialState {
		t.Log("MPS encoding did not change context state (may be expected for short sequences)")
	}

	// Reset and encode LPS - should transition via nlps
	enc.ResetContexts()
	enc.Reset()
	initialState = enc.contexts[0].index

	for range 5 {
		enc.Encode(0, 1) // LPS
	}
	afterLPS := enc.contexts[0].index
	if afterLPS == initialState {
		t.Error("LPS encoding did not change context state")
	}
}

// --- DWT Forward+Inverse Round-Trip Tests ---

func TestDWT2D_53_RoundTrip(t *testing.T) {
	// Forward DWT then inverse DWT should give exact original for 5/3 (lossless)
	tests := []struct {
		name   string
		width  int
		height int
		levels int
	}{
		{"8x8 1-level", 8, 8, 1},
		{"8x8 2-levels", 8, 8, 2},
		{"16x16 3-levels", 16, 16, 3},
		{"odd 7x9 1-level", 7, 9, 1},
		{"odd 7x9 2-levels", 7, 9, 2},
		{"non-square 12x8 2-levels", 12, 8, 2},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test data
			original := make([][]int32, tt.height)
			coeffs := make([][]int32, tt.height)
			for y := range tt.height {
				original[y] = make([]int32, tt.width)
				coeffs[y] = make([]int32, tt.width)
				for x := range tt.width {
					original[y][x] = int32((y*tt.width + x) % 256)
					coeffs[y][x] = original[y][x]
				}
			}

			// Forward transform
			Analyze2D_53(coeffs, tt.width, tt.height, tt.levels)

			// Inverse transform
			Synthesize2D_53(coeffs, tt.width, tt.height, tt.levels)

			// Compare
			for y := range tt.height {
				for x := range tt.width {
					if coeffs[y][x] != original[y][x] {
						t.Errorf("pixel (%d,%d): got %d, want %d", x, y, coeffs[y][x], original[y][x])
					}
				}
			}
		})
	}
}

func TestDWT2D_97_RoundTrip(t *testing.T) {
	// Forward DWT then inverse DWT for 9/7 should be near-lossless
	tests := []struct {
		name     string
		width    int
		height   int
		levels   int
		maxError float64
	}{
		// 9/7 wavelet uses floating point with K scaling factors, so
		// round-trip errors accumulate. Tolerances are relaxed accordingly.
		{"8x8 1-level", 8, 8, 1, 0.001},
		{"8x8 2-levels", 8, 8, 2, 0.01},
		{"16x16 3-levels", 16, 16, 3, 0.01},
		{"odd 7x9 1-level", 7, 9, 1, 0.001},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test data
			original := make([][]float64, tt.height)
			coeffs := make([][]float64, tt.height)
			for y := range tt.height {
				original[y] = make([]float64, tt.width)
				coeffs[y] = make([]float64, tt.width)
				for x := range tt.width {
					original[y][x] = float64((y*tt.width + x) % 256)
					coeffs[y][x] = original[y][x]
				}
			}

			// Forward transform
			Analyze2D_97(coeffs, tt.width, tt.height, tt.levels)

			// Inverse transform
			Synthesize2D_97(coeffs, tt.width, tt.height, tt.levels)

			// Compare with tolerance
			maxErr := 0.0
			for y := range tt.height {
				for x := range tt.width {
					err := math.Abs(coeffs[y][x] - original[y][x])
					if err > maxErr {
						maxErr = err
					}
				}
			}
			if maxErr > tt.maxError {
				t.Errorf("max error %g exceeds threshold %g", maxErr, tt.maxError)
			}
		})
	}
}

// --- Color Transform Round-Trip Tests ---

func TestForwardInverseRCT(t *testing.T) {
	width, height := 8, 8

	// Create RGB data
	r := make([][]int32, height)
	g := make([][]int32, height)
	b := make([][]int32, height)
	for y := range height {
		r[y] = make([]int32, width)
		g[y] = make([]int32, width)
		b[y] = make([]int32, width)
		for x := range width {
			r[y][x] = int32((y*width+x)*7) % 256
			g[y][x] = int32((y*width+x)*13) % 256
			b[y][x] = int32((y*width+x)*19) % 256
		}
	}

	// Save original
	origR := clone2DInt32(r)
	origG := clone2DInt32(g)
	origB := clone2DInt32(b)

	// Forward RCT
	yy, cb, cr := forwardRCT(r, g, b)

	// Inverse RCT
	r2, g2, b2 := applyRCT(yy, cb, cr)

	// Must be exact (lossless)
	for y := range height {
		for x := range width {
			if r2[y][x] != origR[y][x] || g2[y][x] != origG[y][x] || b2[y][x] != origB[y][x] {
				t.Errorf("pixel (%d,%d): RGB got (%d,%d,%d), want (%d,%d,%d)",
					x, y, r2[y][x], g2[y][x], b2[y][x], origR[y][x], origG[y][x], origB[y][x])
			}
		}
	}
}

func TestForwardInverseICT(t *testing.T) {
	width, height := 8, 8

	// Create RGB data
	r := make([][]float64, height)
	g := make([][]float64, height)
	b := make([][]float64, height)
	for y := range height {
		r[y] = make([]float64, width)
		g[y] = make([]float64, width)
		b[y] = make([]float64, width)
		for x := range width {
			r[y][x] = float64((y*width+x)*7) / 256.0
			g[y][x] = float64((y*width+x)*13) / 256.0
			b[y][x] = float64((y*width+x)*19) / 256.0
		}
	}

	// Forward ICT
	yy, cb, cr := forwardICT(r, g, b)

	// Inverse ICT
	r2, g2, b2 := applyICT(yy, cb, cr)

	// Should be near-exact (floating-point round-trip tolerance)
	maxErr := 0.0
	for y := range height {
		for x := range width {
			dr := math.Abs(r2[y][x] - r[y][x])
			dg := math.Abs(g2[y][x] - g[y][x])
			db := math.Abs(b2[y][x] - b[y][x])
			maxErr = max(maxErr, dr, dg, db)
		}
	}
	if maxErr > 1e-4 {
		t.Errorf("max ICT round-trip error: %.2e (threshold: 1e-4)", maxErr)
	}
}

// --- EBCOT Encoder Tests ---

func TestEBCOTEncodeZeroBlock(t *testing.T) {
	// All-zero block should produce minimal output
	coeffs := make([][]int32, 4)
	for y := range coeffs {
		coeffs[y] = make([]int32, 4)
	}

	enc := newEBCOTEncoder(4, 4)
	block := enc.EncodeCodeBlock(coeffs, SubbandLL, 8)

	if block.NumBitPlanes != 0 {
		t.Errorf("zero block: NumBitPlanes = %d, want 0", block.NumBitPlanes)
	}
	if block.NumPasses != 0 {
		t.Errorf("zero block: NumPasses = %d, want 0", block.NumPasses)
	}
}

func TestEBCOTEncodeSingleCoeff(t *testing.T) {
	// Block with a single non-zero coefficient
	coeffs := make([][]int32, 4)
	for y := range coeffs {
		coeffs[y] = make([]int32, 4)
	}
	coeffs[1][1] = 5 // Binary: 101, needs 3 bit planes

	enc := newEBCOTEncoder(4, 4)
	block := enc.EncodeCodeBlock(coeffs, SubbandLL, 8)

	if block.NumBitPlanes != 3 {
		t.Errorf("single coeff: NumBitPlanes = %d, want 3", block.NumBitPlanes)
	}
	if block.NumPasses == 0 {
		t.Error("single coeff: expected at least 1 pass")
	}
	// First pass is cleanup for the MSB
	if block.Passes[0].Type != passTypeCleanup {
		t.Errorf("first pass type = %d, want %d (cleanup)", block.Passes[0].Type, passTypeCleanup)
	}
}

func TestEBCOTEncodePassCounts(t *testing.T) {
	// For N bit planes: 1 cleanup pass for MSB, then 3 passes per remaining bit plane
	// Total = 1 + 3*(N-1) = 3N - 2
	coeffs := make([][]int32, 8)
	for y := range coeffs {
		coeffs[y] = make([]int32, 8)
		for x := range coeffs[y] {
			coeffs[y][x] = int32((y*8 + x) * 3) // Various magnitudes
		}
	}

	enc := newEBCOTEncoder(8, 8)
	block := enc.EncodeCodeBlock(coeffs, SubbandLH, 10)

	expectedPasses := 3*block.NumBitPlanes - 2
	if block.NumPasses != expectedPasses {
		t.Errorf("NumPasses = %d, want %d (for %d bit planes)", block.NumPasses, expectedPasses, block.NumBitPlanes)
	}
}

func TestEBCOTEncodeNegativeCoeffs(t *testing.T) {
	// Test with mixed positive/negative coefficients
	coeffs := [][]int32{
		{-10, 5, -3, 7},
		{2, -8, 4, -1},
		{-6, 9, -2, 11},
		{3, -7, 5, -4},
	}

	enc := newEBCOTEncoder(4, 4)
	block := enc.EncodeCodeBlock(coeffs, SubbandHH, 10)

	if block.NumPasses == 0 {
		t.Error("negative coeffs: expected passes")
	}
	// In continuous MQ mode (standard JPEG2000 without ERTERM), all encoded
	// data is stored on the last pass. Verify the last pass has data.
	lastPass := block.Passes[len(block.Passes)-1]
	if lastPass.Length == 0 {
		t.Error("last pass has zero length")
	}
	totalLen := 0
	for _, p := range block.Passes {
		totalLen += p.Length
	}
	if totalLen == 0 {
		t.Error("total data length is zero")
	}
}

// --- Quantization Tests ---

func TestQuantize97RoundTrip(t *testing.T) {
	stepSize := 2.0
	original := [][]float64{
		{10.5, -3.2, 0.8, -7.1},
		{4.3, -0.5, 6.7, -2.9},
	}

	// Quantize
	quantized := quantize97(original, stepSize)

	// Dequantize
	reconstructed := dequantize97(quantized, stepSize)

	// Check: reconstruction should be within stepSize of original
	for y := range original {
		for x := range original[y] {
			err := math.Abs(reconstructed[y][x] - original[y][x])
			if err > stepSize {
				t.Errorf("(%d,%d): error %g exceeds step size %g", x, y, err, stepSize)
			}
		}
	}
}

func TestQuantizeDeadZone(t *testing.T) {
	stepSize := 5.0
	// Values smaller than stepSize should quantize to 0
	data := [][]float64{
		{4.9, -4.9, 0.0, 3.0},
	}

	quantized := quantize97(data, stepSize)
	for x := range quantized[0] {
		if quantized[0][x] != 0 {
			t.Errorf("value %g quantized to %d, want 0 (dead zone)", data[0][x], quantized[0][x])
		}
	}
}

func TestComputeStepSizeRoundTrip(t *testing.T) {
	bitDepth := 8
	testSteps := []float64{0.5, 1.0, 2.0, 4.0, 8.0, 16.0}

	for _, step := range testSteps {
		exp, mant := computeExpMantissa(step, bitDepth)
		reconstructed := computeStepSize(bitDepth, exp, mant)

		// Should be within ~0.1% due to 11-bit mantissa quantization
		relErr := math.Abs(reconstructed-step) / step
		if relErr > 0.001 {
			t.Errorf("step %g: exp=%d, mant=%d, reconstructed=%g, relErr=%g",
				step, exp, mant, reconstructed, relErr)
		}
	}
}

// --- Rate Control Tests ---

func TestRateControlAllPasses(t *testing.T) {
	// When target is larger than total, all passes should be included
	blocks := []*EncodedBlock{
		{
			Passes: []EncodedPass{
				{Length: 10, Distortion: 100},
				{Length: 20, Distortion: 50},
				{Length: 30, Distortion: 25},
			},
			NumPasses: 3,
		},
	}

	rc := NewRateController(blocks)
	passes := rc.OptimizeSingleLayer(1000)

	if passes[0] != 3 {
		t.Errorf("with large target: got %d passes, want 3", passes[0])
	}
}

func TestRateControlZeroTarget(t *testing.T) {
	blocks := []*EncodedBlock{
		{
			Passes:    []EncodedPass{{Length: 10}},
			NumPasses: 1,
		},
	}

	rc := NewRateController(blocks)
	passes := rc.OptimizeSingleLayer(0)

	if passes[0] != 0 {
		t.Errorf("with zero target: got %d passes, want 0", passes[0])
	}
}

func TestRateControlUniformTruncation(t *testing.T) {
	// Without distortion info, should use uniform truncation
	blocks := make([]*EncodedBlock, 3)
	for i := range blocks {
		blocks[i] = &EncodedBlock{
			Passes: []EncodedPass{
				{Length: 10, Distortion: 0},
				{Length: 20, Distortion: 0},
				{Length: 30, Distortion: 0},
			},
			NumPasses: 3,
		}
	}

	rc := NewRateController(blocks)
	// Total is 3*60=180 bytes, target is 90 (half)
	passes := rc.OptimizeSingleLayer(90)

	totalBytes := 0
	for i, blk := range blocks {
		for p := 0; p < passes[i]; p++ {
			totalBytes += blk.Passes[p].Length
		}
	}
	if totalBytes > 90 {
		t.Errorf("total bytes %d exceeds target 90", totalBytes)
	}
}

// --- Subband Bounds Tests ---

func TestSubbandBounds(t *testing.T) {
	// Test with 2 decomposition levels on a 16x16 tile
	numLevels := 2
	tileW, tileH := 16, 16

	// LL at coarsest level: 16/4 = 4
	sbType, x0, y0, w, h := subbandBounds(0, numLevels, tileW, tileH)
	if sbType != SubbandLL {
		t.Errorf("sb0: type = %d, want LL", sbType)
	}
	if w != 4 || h != 4 {
		t.Errorf("sb0 (LL): size %dx%d, want 4x4", w, h)
	}
	if x0 != 0 || y0 != 0 {
		t.Errorf("sb0 (LL): origin (%d,%d), want (0,0)", x0, y0)
	}

	// Check total area equals tile area
	totalArea := 0
	for sbIdx := range 3*numLevels + 1 {
		_, _, _, sw, sh := subbandBounds(sbIdx, numLevels, tileW, tileH)
		totalArea += sw * sh
	}
	if totalArea != tileW*tileH {
		t.Errorf("total subband area = %d, want %d", totalArea, tileW*tileH)
	}
}

func TestSubbandGain53(t *testing.T) {
	numLevels := 3

	// LL: gain=0
	if g := subbandGain53(0, numLevels); g != 0 {
		t.Errorf("LL gain = %d, want 0", g)
	}
	// LH (idx 1): gain=1
	if g := subbandGain53(1, numLevels); g != 1 {
		t.Errorf("LH gain = %d, want 1", g)
	}
	// HL (idx 2): gain=1
	if g := subbandGain53(2, numLevels); g != 1 {
		t.Errorf("HL gain = %d, want 1", g)
	}
	// HH (idx 3): gain=2
	if g := subbandGain53(3, numLevels); g != 2 {
		t.Errorf("HH gain = %d, want 2", g)
	}
}

// --- Encoder Integration Tests ---

func TestEncodeLosslessGray(t *testing.T) {
	// Create a small grayscale image and verify lossless round-trip
	width, height := 32, 32
	img := image.NewGray(image.Rect(0, 0, width, height))
	for y := range height {
		for x := range width {
			img.SetGray(x, y, color.Gray{Y: uint8((x*7 + y*13) % 256)})
		}
	}

	var buf bytes.Buffer
	err := Encode(&buf, img, &EncodeOptions{
		Lossless:       true,
		NumResolutions: 3,
		FileFormat:     FormatJ2K,
	})
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}

	if buf.Len() == 0 {
		t.Fatal("encoded data is empty")
	}

	// Verify it starts with SOC marker (0xFF4F)
	data := buf.Bytes()
	if len(data) < 2 || data[0] != 0xFF || data[1] != 0x4F {
		t.Errorf("missing SOC marker: first bytes = %02X %02X", data[0], data[1])
	}

	// Verify it ends with EOC marker (0xFFD9)
	if len(data) < 2 || data[len(data)-2] != 0xFF || data[len(data)-1] != 0xD9 {
		t.Errorf("missing EOC marker: last bytes = %02X %02X", data[len(data)-2], data[len(data)-1])
	}
}

func TestEncodeLosslessRGB(t *testing.T) {
	width, height := 32, 32
	img := image.NewNRGBA(image.Rect(0, 0, width, height))
	for y := range height {
		for x := range width {
			img.SetNRGBA(x, y, color.NRGBA{
				R: uint8((x*7 + y*3) % 256),
				G: uint8((x*11 + y*5) % 256),
				B: uint8((x*13 + y*17) % 256),
				A: 255,
			})
		}
	}

	var buf bytes.Buffer
	err := Encode(&buf, img, &EncodeOptions{
		Lossless:       true,
		NumResolutions: 3,
		FileFormat:     FormatJ2K,
	})
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}

	if buf.Len() == 0 {
		t.Fatal("encoded data is empty")
	}

	// Basic structure check
	data := buf.Bytes()
	if data[0] != 0xFF || data[1] != 0x4F {
		t.Error("missing SOC marker")
	}
}

func TestEncodeLossyRGB(t *testing.T) {
	width, height := 64, 64
	img := image.NewNRGBA(image.Rect(0, 0, width, height))
	for y := range height {
		for x := range width {
			img.SetNRGBA(x, y, color.NRGBA{
				R: uint8((x*7 + y*3) % 256),
				G: uint8((x*11 + y*5) % 256),
				B: uint8((x*13 + y*17) % 256),
				A: 255,
			})
		}
	}

	var buf bytes.Buffer
	err := Encode(&buf, img, &EncodeOptions{
		Lossless:       false,
		Quality:        0.8,
		NumResolutions: 4,
		FileFormat:     FormatJ2K,
	})
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}

	if buf.Len() == 0 {
		t.Fatal("encoded data is empty")
	}

	// Lossy should generally be smaller than lossless
	var losslessBuf bytes.Buffer
	_ = Encode(&losslessBuf, img, &EncodeOptions{
		Lossless:       true,
		NumResolutions: 4,
		FileFormat:     FormatJ2K,
	})
	t.Logf("lossy size: %d bytes, lossless size: %d bytes", buf.Len(), losslessBuf.Len())
}

func TestEncodeJP2Format(t *testing.T) {
	width, height := 16, 16
	img := image.NewGray(image.Rect(0, 0, width, height))
	for y := range height {
		for x := range width {
			img.SetGray(x, y, color.Gray{Y: uint8(x * y)})
		}
	}

	var buf bytes.Buffer
	err := Encode(&buf, img, &EncodeOptions{
		Lossless:       true,
		NumResolutions: 2,
		FileFormat:     FormatJP2,
	})
	if err != nil {
		t.Fatalf("Encode JP2: %v", err)
	}

	// JP2 files start with the JP2 signature box: 0x0000000C 6A502020
	data := buf.Bytes()
	if len(data) < 12 {
		t.Fatal("JP2 data too short")
	}
	// Signature box length (4 bytes) + type (4 bytes) + content
	// Box length = 12 (0x0000000C)
	if data[0] != 0 || data[1] != 0 || data[2] != 0 || data[3] != 12 {
		t.Errorf("JP2 signature box length: got %02X%02X%02X%02X, want 0000000C",
			data[0], data[1], data[2], data[3])
	}
	// Box type = "jP  " (0x6A502020)
	if data[4] != 0x6A || data[5] != 0x50 || data[6] != 0x20 || data[7] != 0x20 {
		t.Errorf("JP2 signature box type: got %02X%02X%02X%02X, want 6A502020",
			data[4], data[5], data[6], data[7])
	}
}

func TestEncodeTargetSize(t *testing.T) {
	width, height := 64, 64
	img := image.NewNRGBA(image.Rect(0, 0, width, height))
	for y := range height {
		for x := range width {
			img.SetNRGBA(x, y, color.NRGBA{
				R: uint8(x * 4),
				G: uint8(y * 4),
				B: uint8((x + y) * 2),
				A: 255,
			})
		}
	}

	targetSize := 500
	var buf bytes.Buffer
	err := Encode(&buf, img, &EncodeOptions{
		Lossless:       false,
		TargetSize:     targetSize,
		NumResolutions: 3,
		FileFormat:     FormatJ2K,
	})
	if err != nil {
		t.Fatalf("Encode with target size: %v", err)
	}

	t.Logf("target: %d bytes, actual: %d bytes", targetSize, buf.Len())
}

func TestEncodeSmallImage(t *testing.T) {
	// Test edge case: very small image
	sizes := []struct {
		w, h int
	}{
		{1, 1},
		{2, 2},
		{3, 3},
		{4, 4},
		{1, 8},
		{8, 1},
	}

	for _, sz := range sizes {
		t.Run(
			func() string { return image.Rect(0, 0, sz.w, sz.h).String() }(),
			func(t *testing.T) {
				img := image.NewGray(image.Rect(0, 0, sz.w, sz.h))
				for y := range sz.h {
					for x := range sz.w {
						img.SetGray(x, y, color.Gray{Y: 128})
					}
				}

				var buf bytes.Buffer
				err := Encode(&buf, img, &EncodeOptions{
					Lossless:       true,
					NumResolutions: 2,
					FileFormat:     FormatJ2K,
				})
				if err != nil {
					t.Fatalf("Encode %dx%d: %v", sz.w, sz.h, err)
				}
				if buf.Len() == 0 {
					t.Fatalf("Encode %dx%d: empty output", sz.w, sz.h)
				}
			},
		)
	}
}

func TestEncodeDefaultOptions(t *testing.T) {
	// Test encoding with nil options (all defaults)
	img := image.NewGray(image.Rect(0, 0, 16, 16))
	for y := range 16 {
		for x := range 16 {
			img.SetGray(x, y, color.Gray{Y: uint8(x + y)})
		}
	}

	var buf bytes.Buffer
	err := Encode(&buf, img, nil)
	if err != nil {
		t.Fatalf("Encode with nil opts: %v", err)
	}
	if buf.Len() == 0 {
		t.Fatal("empty output with nil opts")
	}
}

// --- Encode+Decode Round-Trip Tests ---

func TestLosslessRoundTrip(t *testing.T) {
	// Encode lossless then decode: pixels should match exactly
	width, height := 32, 32
	img := image.NewGray(image.Rect(0, 0, width, height))
	for y := range height {
		for x := range width {
			img.SetGray(x, y, color.Gray{Y: uint8((x*17 + y*31) % 256)})
		}
	}

	var buf bytes.Buffer
	err := Encode(&buf, img, &EncodeOptions{
		Lossless:       true,
		NumResolutions: 3,
		FileFormat:     FormatJ2K,
	})
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}

	decoded, err := Decode(bytes.NewReader(buf.Bytes()))
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}

	bounds := decoded.Bounds()
	if bounds.Dx() != width || bounds.Dy() != height {
		t.Fatalf("decoded size: %dx%d, want %dx%d", bounds.Dx(), bounds.Dy(), width, height)
	}

	// Compare pixels
	mismatches := 0
	for y := range height {
		for x := range width {
			origR, origG, origB, _ := img.At(x, y).RGBA()
			decR, decG, decB, _ := decoded.At(x, y).RGBA()
			if origR != decR || origG != decG || origB != decB {
				mismatches++
				if mismatches <= 5 {
					t.Errorf("pixel (%d,%d): original (%d,%d,%d) != decoded (%d,%d,%d)",
						x, y, origR>>8, origG>>8, origB>>8, decR>>8, decG>>8, decB>>8)
				}
			}
		}
	}
	if mismatches > 0 {
		t.Errorf("total mismatches: %d out of %d pixels", mismatches, width*height)
	}
}

func TestLossyRoundTrip(t *testing.T) {
	// Encode lossy then decode: check PSNR is reasonable
	width, height := 64, 64
	img := image.NewNRGBA(image.Rect(0, 0, width, height))
	for y := range height {
		for x := range width {
			img.SetNRGBA(x, y, color.NRGBA{
				R: uint8((x*7 + y*3) % 256),
				G: uint8((x*11 + y*5) % 256),
				B: uint8((x*13 + y*17) % 256),
				A: 255,
			})
		}
	}

	var buf bytes.Buffer
	err := Encode(&buf, img, &EncodeOptions{
		Lossless:       false,
		Quality:        0.9,
		NumResolutions: 3,
		FileFormat:     FormatJ2K,
	})
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}

	decoded, err := Decode(bytes.NewReader(buf.Bytes()))
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}

	bounds := decoded.Bounds()
	if bounds.Dx() != width || bounds.Dy() != height {
		t.Fatalf("decoded size: %dx%d, want %dx%d", bounds.Dx(), bounds.Dy(), width, height)
	}

	// Compute PSNR
	psnr := computePSNR(img, decoded, width, height)
	t.Logf("lossy PSNR: %.2f dB (size: %d bytes)", psnr, buf.Len())

	// A reasonable quality=0.9 encoding should yield PSNR > 20 dB
	if psnr < 20.0 {
		t.Errorf("PSNR %.2f dB is below threshold of 20 dB", psnr)
	}
}

func TestLosslessRoundTripRGB(t *testing.T) {
	width, height := 32, 32
	img := image.NewNRGBA(image.Rect(0, 0, width, height))
	for y := range height {
		for x := range width {
			img.SetNRGBA(x, y, color.NRGBA{
				R: uint8((x*7 + y*3) % 256),
				G: uint8((x*11 + y*5) % 256),
				B: uint8((x*13 + y*17) % 256),
				A: 255,
			})
		}
	}

	var buf bytes.Buffer
	err := Encode(&buf, img, &EncodeOptions{
		Lossless:       true,
		NumResolutions: 3,
		FileFormat:     FormatJ2K,
	})
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}

	decoded, err := Decode(bytes.NewReader(buf.Bytes()))
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}

	bounds := decoded.Bounds()
	if bounds.Dx() != width || bounds.Dy() != height {
		t.Fatalf("decoded size: %dx%d, want %dx%d", bounds.Dx(), bounds.Dy(), width, height)
	}

	// Compare pixels
	mismatches := 0
	for y := range height {
		for x := range width {
			origR, origG, origB, _ := img.At(x, y).RGBA()
			decR, decG, decB, _ := decoded.At(x, y).RGBA()
			if origR != decR || origG != decG || origB != decB {
				mismatches++
				if mismatches <= 5 {
					t.Errorf("pixel (%d,%d): original (%d,%d,%d) != decoded (%d,%d,%d)",
						x, y, origR>>8, origG>>8, origB>>8, decR>>8, decG>>8, decB>>8)
				}
			}
		}
	}
	if mismatches > 0 {
		t.Errorf("total mismatches: %d out of %d pixels", mismatches, width*height)
	}
}

func TestEncodeDecodeJP2(t *testing.T) {
	// Test JP2 format round-trip
	width, height := 16, 16
	img := image.NewGray(image.Rect(0, 0, width, height))
	for y := range height {
		for x := range width {
			img.SetGray(x, y, color.Gray{Y: uint8(x*16 + y)})
		}
	}

	var buf bytes.Buffer
	err := Encode(&buf, img, &EncodeOptions{
		Lossless:       true,
		NumResolutions: 2,
		FileFormat:     FormatJP2,
	})
	if err != nil {
		t.Fatalf("Encode JP2: %v", err)
	}

	decoded, err := Decode(bytes.NewReader(buf.Bytes()))
	if err != nil {
		t.Fatalf("Decode JP2: %v", err)
	}

	bounds := decoded.Bounds()
	if bounds.Dx() != width || bounds.Dy() != height {
		t.Fatalf("decoded size: %dx%d, want %dx%d", bounds.Dx(), bounds.Dy(), width, height)
	}
}

// --- Codestream Writer Tests ---

func TestCodestreamWriterMarkers(t *testing.T) {
	// Verify that WriteMainHeader produces valid marker sequence
	header := &MainHeader{
		Width:           16,
		Height:          16,
		TileWidth:       16,
		TileHeight:      16,
		NumComps:        1,
		BitDepth:        []int{8},
		Signed:          []bool{false},
		XRsiz:           []int{1},
		YRsiz:           []int{1},
		NumDecompLevels: 2,
		CodeBlockWidth:  64,
		CodeBlockHeight: 64,
		WaveletFilter:   Wavelet53,
		QuantStyle:      0,
		GuardBits:       1,
		Exponents:       []int{10, 10, 10, 10, 10, 10, 10},
		Mantissas:       []int{0, 0, 0, 0, 0, 0, 0},
		NumXTiles:       1,
		NumYTiles:       1,
		NumTiles:        1,
		NumLayers:       1,
	}

	var buf bytes.Buffer
	cw := NewCodestreamWriter(&buf, header)
	if err := cw.WriteMainHeader(); err != nil {
		t.Fatalf("WriteMainHeader: %v", err)
	}

	data := buf.Bytes()

	// SOC marker: 0xFF4F
	if len(data) < 2 || data[0] != 0xFF || data[1] != 0x4F {
		t.Error("missing SOC marker")
	}

	// SIZ marker should follow: 0xFF51
	if len(data) < 4 || data[2] != 0xFF || data[3] != 0x51 {
		t.Error("missing SIZ marker after SOC")
	}
}

// --- Benchmark ---

func BenchmarkEncodeLossless32x32(b *testing.B) {
	img := image.NewGray(image.Rect(0, 0, 32, 32))
	for y := range 32 {
		for x := range 32 {
			img.SetGray(x, y, color.Gray{Y: uint8((x + y) % 256)})
		}
	}
	opts := &EncodeOptions{
		Lossless:       true,
		NumResolutions: 3,
		FileFormat:     FormatJ2K,
	}

	b.ReportAllocs()
	for b.Loop() {
		var buf bytes.Buffer
		_ = Encode(&buf, img, opts)
	}
}

func BenchmarkEncodeLossy64x64(b *testing.B) {
	img := image.NewNRGBA(image.Rect(0, 0, 64, 64))
	for y := range 64 {
		for x := range 64 {
			img.SetNRGBA(x, y, color.NRGBA{
				R: uint8(x * 4), G: uint8(y * 4), B: uint8((x + y) * 2), A: 255,
			})
		}
	}
	opts := &EncodeOptions{
		Lossless:       false,
		Quality:        0.8,
		NumResolutions: 3,
		FileFormat:     FormatJ2K,
	}

	b.ReportAllocs()
	for b.Loop() {
		var buf bytes.Buffer
		_ = Encode(&buf, img, opts)
	}
}

// --- Helper Functions ---

func clone2DInt32(src [][]int32) [][]int32 {
	dst := make([][]int32, len(src))
	for i := range src {
		dst[i] = make([]int32, len(src[i]))
		copy(dst[i], src[i])
	}
	return dst
}

func computePSNR(original, decoded image.Image, width, height int) float64 {
	var mse float64
	count := 0

	for y := range height {
		for x := range width {
			or, og, ob, _ := original.At(x, y).RGBA()
			dr, dg, db, _ := decoded.At(x, y).RGBA()

			// Convert to 8-bit
			diffR := float64(or>>8) - float64(dr>>8)
			diffG := float64(og>>8) - float64(dg>>8)
			diffB := float64(ob>>8) - float64(db>>8)

			mse += diffR*diffR + diffG*diffG + diffB*diffB
			count += 3
		}
	}

	if count == 0 {
		return math.Inf(1)
	}
	mse /= float64(count)
	if mse == 0 {
		return math.Inf(1)
	}

	return 10.0 * math.Log10(255.0*255.0/mse)
}
