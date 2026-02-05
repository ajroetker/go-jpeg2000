package jpeg2000

import (
	"testing"
)

// TestEBCOTContextCalculation tests context determination functions
// Note: hasSignificantNeighbor() uses flagNeighborSig which is set by setSignificant()
// when a coefficient becomes significant. Tests must use setSignificant() or manually
// set flagNeighborSig on the target coefficient.
func TestEBCOTContextCalculation(t *testing.T) {
	e := newEBCOTDecoder(8, 8)

	// Helper to set a neighbor as significant and propagate flagNeighborSig
	setNeighborSig := func(nx, ny int) {
		e.setSignificant(nx, ny, 0, false) // bit plane doesn't matter for this test
	}

	tests := []struct {
		name         string
		setup        func()
		x, y         int
		subbandType  SubbandType
		wantSigCtx   int
		wantHasSigNb bool
	}{
		{
			name: "no neighbors",
			setup: func() {
				e.resetState()
			},
			x:            4,
			y:            4,
			subbandType:  SubbandHL,
			wantSigCtx:   0,
			wantHasSigNb: false,
		},
		{
			name: "one horizontal neighbor",
			setup: func() {
				e.resetState()
				setNeighborSig(3, 4) // left neighbor - propagates flagNeighborSig to (4,4)
			},
			x:           4,
			y:           4,
			subbandType: SubbandHL,
			// HL swaps h,v: h=1,v=0 -> h=0,v=1 -> context 3
			wantSigCtx:   3,
			wantHasSigNb: true,
		},
		{
			name: "two horizontal neighbors",
			setup: func() {
				e.resetState()
				setNeighborSig(3, 4) // left
				setNeighborSig(5, 4) // right
			},
			x:           4,
			y:           4,
			subbandType: SubbandHL,
			// HL swaps h,v: h=2,v=0 -> h=0,v=2 -> context 4
			wantSigCtx:   4,
			wantHasSigNb: true,
		},
		{
			name: "one vertical neighbor",
			setup: func() {
				e.resetState()
				setNeighborSig(4, 3) // top
			},
			x:           4,
			y:           4,
			subbandType: SubbandHL,
			// HL swaps h,v: h=0,v=1 -> h=1,v=0 -> context 5
			wantSigCtx:   5,
			wantHasSigNb: true,
		},
		{
			name: "mixed neighbors",
			setup: func() {
				e.resetState()
				setNeighborSig(3, 4) // left (h)
				setNeighborSig(4, 3) // top (v)
			},
			x:           4,
			y:           4,
			subbandType: SubbandHL,
			// HL swaps h,v: h=1,v=1 -> h=1,v=1 -> context 7
			wantSigCtx:   7,
			wantHasSigNb: true,
		},
		{
			name: "diagonal HH subband",
			setup: func() {
				e.resetState()
				setNeighborSig(3, 3) // diagonal - propagates flagNeighborSig
			},
			x:           4,
			y:           4,
			subbandType: SubbandHH,
			// HH: d=1, hv=0 -> context 3
			wantSigCtx:   3,
			wantHasSigNb: true, // diagonal neighbors DO count for flagNeighborSig
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tt.setup()

			// Test significance context
			gotCtx := e.getSigContext(tt.x, tt.y, tt.subbandType, false)
			if gotCtx != tt.wantSigCtx {
				t.Errorf("getSigContext() = %d, want %d", gotCtx, tt.wantSigCtx)
			}

			// Test has significant neighbor
			gotHas := e.hasSignificantNeighbor(tt.x, tt.y, false)
			if gotHas != tt.wantHasSigNb {
				t.Errorf("hasSignificantNeighbor() = %v, want %v", gotHas, tt.wantHasSigNb)
			}
		})
	}
}

// TestEBCOTSignContext tests sign context and XOR prediction
// Uses OpenJPEG's LUT tables (lutCtxnoSC and lutSPB) for exact bit-compatible decoding.
// The LUT index is built from neighbor significance and sign bits.
func TestEBCOTSignContext(t *testing.T) {
	e := newEBCOTDecoder(8, 8)

	tests := []struct {
		name       string
		setup      func()
		x, y       int
		wantCtx    int
		wantXorBit int
	}{
		{
			name: "no neighbors",
			setup: func() {
				e.resetState()
			},
			x:          4,
			y:          4,
			wantCtx:    ctxSign_0, // LUT[0x00] = 9
			wantXorBit: 0,         // no bias
		},
		{
			name: "positive horizontal neighbor (west)",
			setup: func() {
				e.resetState()
				// West neighbor at (3,4): significant, positive
				// LUT index: lutSigW (bit 3) = 0x08
				e.state[4][3] = flagSignificant // positive (no sign flag)
			},
			x:          4,
			y:          4,
			wantCtx:    12, // LUT[0x08] = 0xc = 12 (ctxSign_3)
			wantXorBit: 0,  // LUT_SPB[0x08] = 0
		},
		{
			name: "negative horizontal neighbor (west)",
			setup: func() {
				e.resetState()
				// West neighbor: significant, negative
				// LUT index: lutSigW (bit 3) | lutSgnW (bit 0) = 0x09
				e.state[4][3] = flagSignificant | flagSign
			},
			x:          4,
			y:          4,
			wantCtx:    12, // LUT[0x09] = 0xc = 12 (ctxSign_3)
			wantXorBit: 1,  // LUT_SPB[0x09] = 1 (predict negative)
		},
		{
			name: "mixed neighbors - H+ H+ V-",
			setup: func() {
				e.resetState()
				// West positive: lutSigW (0x08)
				e.state[4][3] = flagSignificant
				// East positive: lutSigE (0x20)
				e.state[4][5] = flagSignificant
				// North negative: lutSigN (0x02) | lutSgnN (0x10)
				e.state[3][4] = flagSignificant | flagSign
			},
			x:          4,
			y:          4,
			wantCtx:    11, // LUT[0x3a] = 0xb = 11
			wantXorBit: 0,  // LUT_SPB[0x3a] = 0
		},
		{
			name: "mixed neighbors - H- H- V+",
			setup: func() {
				e.resetState()
				// West negative: lutSigW (0x08) | lutSgnW (0x01) = 0x09
				e.state[4][3] = flagSignificant | flagSign
				// East negative: lutSigE (0x20) | lutSgnE (0x04) = 0x24
				e.state[4][5] = flagSignificant | flagSign
				// North positive: lutSigN (0x02)
				e.state[3][4] = flagSignificant
			},
			x:          4,
			y:          4,
			wantCtx:    11, // LUT[0x2f] = 0xb = 11
			wantXorBit: 1,  // LUT_SPB[0x2f] = 1
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tt.setup()

			gotCtx, gotXor := e.getSignContext(tt.x, tt.y, false)
			if gotCtx != tt.wantCtx {
				t.Errorf("getSignContext() ctx = %d, want %d", gotCtx, tt.wantCtx)
			}
			if gotXor != tt.wantXorBit {
				t.Errorf("getSignContext() xor = %d, want %d", gotXor, tt.wantXorBit)
			}
		})
	}
}

// TestEBCOTMagnitudeContext tests magnitude refinement context
// Per ITU-T T.800 Table D.3:
// - Context 14: First refinement AND no significant neighbors
// - Context 15: Subsequent refinement OR has significant neighbors
func TestEBCOTMagnitudeContext(t *testing.T) {
	e := newEBCOTDecoder(8, 8)

	tests := []struct {
		name    string
		setup   func()
		x, y    int
		wantCtx int
	}{
		{
			name: "no neighbors - first refinement",
			setup: func() {
				e.resetState()
			},
			x:       4,
			y:       4,
			wantCtx: ctxMagFirst, // 14
		},
		{
			name: "has neighbors - other refinement",
			setup: func() {
				e.resetState()
				// Use setSignificant to properly propagate flagNeighborSig to (4,4)
				e.setSignificant(3, 4, 0, false) // west neighbor
			},
			x:       4,
			y:       4,
			wantCtx: ctxMagOther, // 15
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tt.setup()

			gotCtx := e.getMagContext(tt.x, tt.y, false)
			if gotCtx != tt.wantCtx {
				t.Errorf("getMagContext() = %d, want %d", gotCtx, tt.wantCtx)
			}
		})
	}
}

// TestEBCOTCountNeighbors tests neighbor counting
func TestEBCOTCountNeighbors(t *testing.T) {
	e := newEBCOTDecoder(8, 8)

	tests := []struct {
		name  string
		setup func()
		x, y  int
		wantH int
		wantV int
		wantD int
	}{
		{
			name: "no neighbors",
			setup: func() {
				e.resetState()
			},
			x:     4,
			y:     4,
			wantH: 0,
			wantV: 0,
			wantD: 0,
		},
		{
			name: "horizontal only",
			setup: func() {
				e.resetState()
				e.state[4][3] = flagSignificant // left
				e.state[4][5] = flagSignificant // right
			},
			x:     4,
			y:     4,
			wantH: 2,
			wantV: 0,
			wantD: 0,
		},
		{
			name: "vertical only",
			setup: func() {
				e.resetState()
				e.state[3][4] = flagSignificant // top
				e.state[5][4] = flagSignificant // bottom
			},
			x:     4,
			y:     4,
			wantH: 0,
			wantV: 2,
			wantD: 0,
		},
		{
			name: "diagonal only",
			setup: func() {
				e.resetState()
				e.state[3][3] = flagSignificant // top-left
				e.state[3][5] = flagSignificant // top-right
				e.state[5][3] = flagSignificant // bottom-left
				e.state[5][5] = flagSignificant // bottom-right
			},
			x:     4,
			y:     4,
			wantH: 0,
			wantV: 0,
			wantD: 4,
		},
		{
			name: "all neighbors",
			setup: func() {
				e.resetState()
				// Set all 8 neighbors
				e.state[3][3] = flagSignificant // top-left
				e.state[3][4] = flagSignificant // top
				e.state[3][5] = flagSignificant // top-right
				e.state[4][3] = flagSignificant // left
				e.state[4][5] = flagSignificant // right
				e.state[5][3] = flagSignificant // bottom-left
				e.state[5][4] = flagSignificant // bottom
				e.state[5][5] = flagSignificant // bottom-right
			},
			x:     4,
			y:     4,
			wantH: 2,
			wantV: 2,
			wantD: 4,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tt.setup()

			h, v, d := e.countSigNeighbors(tt.x, tt.y, false)
			if h != tt.wantH || v != tt.wantV || d != tt.wantD {
				t.Errorf("countSigNeighbors() = (%d, %d, %d), want (%d, %d, %d)",
					h, v, d, tt.wantH, tt.wantV, tt.wantD)
			}
		})
	}
}

// TestEBCOTRunMode tests run-length mode detection
func TestEBCOTRunMode(t *testing.T) {
	e := newEBCOTDecoder(8, 8)

	tests := []struct {
		name  string
		setup func()
		x     int
		y0    int
		count int
		want  bool
	}{
		{
			name: "can use run mode - all clear",
			setup: func() {
				e.resetState()
			},
			x:     4,
			y0:    1,
			count: 4,
			want:  true,
		},
		{
			name: "cannot use - one visited",
			setup: func() {
				e.resetState()
				e.state[2][4] = flagVisited
			},
			x:     4,
			y0:    1,
			count: 4,
			want:  false,
		},
		{
			name: "cannot use - one significant",
			setup: func() {
				e.resetState()
				e.state[3][4] = flagSignificant
			},
			x:     4,
			y0:    1,
			count: 4,
			want:  false,
		},
		{
			name: "cannot use - has neighbor",
			setup: func() {
				e.resetState()
				// Use setSignificant to properly propagate flagNeighborSig
				// Neighbor at (3,1) will set flagNeighborSig on (4,1)
				e.setSignificant(3, 1, 0, false)
			},
			x:     4,
			y0:    1,
			count: 4,
			want:  false,
		},
		{
			name: "cannot use - count too small",
			setup: func() {
				e.resetState()
			},
			x:     4,
			y0:    1,
			count: 3,
			want:  false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tt.setup()

			// Capture full state snapshot as the real cleanup pass does
			stateSnapshot := [4]uint8{}
			for i := 0; i < tt.count && i < 4; i++ {
				y := tt.y0 + i
				stateSnapshot[i] = e.state[y][tt.x]
			}

			got := e.canUseRunModeFromSnapshot(tt.count, stateSnapshot[:])
			if got != tt.want {
				t.Errorf("canUseRunModeFromSnapshot() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestEBCOTSimplePattern tests decoding with a simple known pattern
func TestEBCOTSimplePattern(t *testing.T) {
	// Create a simple code block with known output
	// This is a minimal test - real JPEG2000 data would be more complex

	// For this test, we'll create a very simple MQ-coded bitstream
	// representing a 4x4 block with a few significant coefficients

	// This is a synthetic test case - in practice, you'd need actual
	// JPEG2000 encoded data, which requires a full encoder or test vectors

	e := newEBCOTDecoder(4, 4)

	// Create minimal bitstream (this would normally come from encoder)
	// For now, just test that decoding doesn't crash
	data := []byte{
		0x00, 0x00, 0x00, 0x00, // Placeholder - would be real MQ data
	}

	cb := &CodeBlock{
		Data:          data,
		Width:         4,
		Height:        4,
		ZeroBitPlanes: 0,
		NumPasses:     3, // One full bit plane (3 passes)
	}

	result, err := e.DecodeCodeBlock(cb, SubbandHL)
	if err != nil {
		t.Fatalf("DecodeCodeBlock() error = %v", err)
	}

	// Verify dimensions
	if len(result) != 4 {
		t.Errorf("result height = %d, want 4", len(result))
	}
	if len(result) > 0 && len(result[0]) != 4 {
		t.Errorf("result width = %d, want 4", len(result[0]))
	}

	// With zero MQ data, all coefficients should be zero or small
	// (depends on MQ decoder initialization)
	// This is just checking that decoding completes
}

// TestEBCOTSetSignificant tests setting coefficient as significant
// Per OpenJPEG, setSignificant always uses oneplushalf midpoint reconstruction
// with an extra precision bit. The internal value is 2x what the final coefficient
// will be - the divide by 2 happens post-EBCOT in packet.go.
func TestEBCOTSetSignificant(t *testing.T) {
	e := newEBCOTDecoder(4, 4)
	e.resetState()

	// Set coefficient at (2, 2) significant at bit plane 5
	x, y := 2, 2
	bp := 5

	e.setSignificant(x, y, bp, false)

	// Check flag is set
	if e.state[y][x]&flagSignificant == 0 {
		t.Error("flagSignificant not set")
	}

	// Per OpenJPEG: magnitude uses extra precision bit (bp+1)
	// one = 1 << (5+1) = 64, half = 32, so one | half = 96
	// This is 2x the final coefficient value; divide by 2 happens in dequant
	one := int32(1 << uint(bp+1))
	half := one >> 1
	expectedMag := one | half // oneplushalf with extra precision
	if e.data[y][x] != expectedMag {
		t.Errorf("magnitude = %d, want %d", e.data[y][x], expectedMag)
	}

	// Also verify neighbor propagation occurred
	// Neighbors of (2,2) should have flagNeighborSig set
	if e.state[y][x-1]&flagNeighborSig == 0 {
		t.Error("west neighbor missing flagNeighborSig")
	}
	if e.state[y][x+1]&flagNeighborSig == 0 {
		t.Error("east neighbor missing flagNeighborSig")
	}
	if e.state[y-1][x]&flagNeighborSig == 0 {
		t.Error("north neighbor missing flagNeighborSig")
	}
	if e.state[y+1][x]&flagNeighborSig == 0 {
		t.Error("south neighbor missing flagNeighborSig")
	}
}

// TestEBCOTBitPlaneDecoding tests multi-bit-plane decoding
// Note: EBCOT uses 2x internal precision (extra bit). Final values are divided by 2
// during dequantization in packet.go.
func TestEBCOTBitPlaneDecoding(t *testing.T) {
	e := newEBCOTDecoder(4, 4)

	// Simulate decoding multiple bit planes
	// Set up a coefficient that becomes significant at bit plane 3
	// and gets refined at bit planes 2, 1, 0

	e.resetState()

	// Bit plane 3: becomes significant
	// With extra precision: one = 1 << (3+1) = 16, half = 8, one|half = 24
	e.setSignificant(2, 2, 3, false)
	initialMag := e.data[2][2] // should be 24 (one | half at bp=3 with extra precision)
	if initialMag != 24 {
		t.Errorf("initial magnitude = %d, want 24", initialMag)
	}

	e.state[2][2] |= flagRefined // mark as having been through first refinement

	// Bit plane 2: refinement adds bit (bit=1)
	// With extra precision, we add/subtract at (bp+1), so 1 << (2+1) >> 1 = 4
	e.data[2][2] |= 1 << 3 // poshalf at bp 2 with extra precision

	// Bit plane 1: refinement adds bit (bit=1)
	// 1 << (1+1) >> 1 = 2
	e.data[2][2] |= 1 << 2

	// Bit plane 0: refinement adds bit (bit=1)
	// 1 << (0+1) >> 1 = 1
	e.data[2][2] |= 1 << 1

	// Expected magnitude: 24 | 8 | 4 | 2 = 30 (internal value with 2x precision)
	// After divide by 2 in dequant, this would give 15
	expectedMag := int32(30)

	if e.data[2][2] != expectedMag {
		t.Errorf("final magnitude = %d, want %d", e.data[2][2], expectedMag)
	}

	// Test with negative sign
	e.state[2][2] |= flagSign

	// When extracting result, sign should be applied
	result := make([][]int32, 4)
	for y := range 4 {
		result[y] = make([]int32, 4)
		for x := range 4 {
			val := e.data[y+1][x+1]
			if e.state[y+1][x+1]&flagSign != 0 {
				val = -val
			}
			result[y][x] = val
		}
	}

	// Internal value is -30, which after divide by 2 gives -15
	if result[1][1] != -30 {
		t.Errorf("signed coefficient = %d, want -30", result[1][1])
	}
}

// TestEBCOTBorderHandling tests that border padding works correctly
func TestEBCOTBorderHandling(t *testing.T) {
	e := newEBCOTDecoder(4, 4)
	e.resetState()

	// Border cells should remain zero initially
	// Check all border cells
	for x := range 6 {
		if e.state[0][x] != 0 {
			t.Errorf("top border at x=%d not zero", x)
		}
		if e.state[5][x] != 0 {
			t.Errorf("bottom border at x=%d not zero", x)
		}
	}

	for y := range 6 {
		if e.state[y][0] != 0 {
			t.Errorf("left border at y=%d not zero", y)
		}
		if e.state[y][5] != 0 {
			t.Errorf("right border at y=%d not zero", y)
		}
	}

	// Set coefficient at corner and check neighbors
	e.setSignificant(1, 1, 0, false) // Top-left actual coefficient (border coords)

	// (2,2) IS a diagonal neighbor of (1,1), so flagNeighborSig should be set
	// Note: The new implementation propagates to all 8 neighbors including diagonals
	if !e.hasSignificantNeighbor(2, 2, false) {
		t.Error("(2,2) should have flagNeighborSig from diagonal (1,1)")
	}

	// (1,2) is a vertical neighbor of (1,1)
	if !e.hasSignificantNeighbor(1, 2, false) {
		t.Error("(1,2) should detect (1,1) as vertical neighbor")
	}

	// (2,1) is a horizontal neighbor of (1,1)
	if !e.hasSignificantNeighbor(2, 1, false) {
		t.Error("(2,1) should detect (1,1) as horizontal neighbor")
	}

	// (3,3) should NOT have flagNeighborSig (too far from (1,1))
	if e.hasSignificantNeighbor(3, 3, false) {
		t.Error("(3,3) should not be a neighbor of (1,1)")
	}
}

// BenchmarkEBCOTDecode benchmarks EBCOT decoding
func BenchmarkEBCOTDecode(b *testing.B) {
	// Create a synthetic code block
	data := make([]byte, 256)
	for i := range data {
		data[i] = byte(i % 256)
	}

	cb := &CodeBlock{
		Data:          data,
		Width:         32,
		Height:        32,
		ZeroBitPlanes: 2,
		NumPasses:     9, // 3 bit planes
	}

	e := newEBCOTDecoder(32, 32)

	b.ReportAllocs()

	for b.Loop() {
		_, _ = e.DecodeCodeBlock(cb, SubbandHL)
	}
}

// BenchmarkEBCOTContextLookup benchmarks context calculation
func BenchmarkEBCOTContextLookup(b *testing.B) {
	e := newEBCOTDecoder(32, 32)

	// Set up some significant coefficients
	for y := 1; y <= 32; y += 4 {
		for x := 1; x <= 32; x += 4 {
			e.state[y][x] = flagSignificant
		}
	}

	b.ReportAllocs()

	for b.Loop() {
		_ = e.getSigContext(16, 16, SubbandHL, false)
	}
}
