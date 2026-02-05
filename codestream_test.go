package jpeg2000

import (
	"encoding/binary"
	"testing"
)

// buildMinimalCodestream creates a minimal valid JPEG2000 codestream for testing
func buildMinimalCodestream() []byte {
	buf := make([]byte, 0, 256)

	// SOC marker
	buf = appendUint16(buf, markerSOC)

	// SIZ marker
	buf = appendUint16(buf, markerSIZ)
	sizLen := uint16(38 + 3*1) // 1 component
	buf = appendUint16(buf, sizLen)
	buf = appendUint16(buf, 0)   // Rsiz (capabilities)
	buf = appendUint32(buf, 256) // Xsiz (width)
	buf = appendUint32(buf, 256) // Ysiz (height)
	buf = appendUint32(buf, 0)   // XOsiz
	buf = appendUint32(buf, 0)   // YOsiz
	buf = appendUint32(buf, 256) // XTsiz (tile width)
	buf = appendUint32(buf, 256) // YTsiz (tile height)
	buf = appendUint32(buf, 0)   // XTOsiz
	buf = appendUint32(buf, 0)   // YTOsiz
	buf = appendUint16(buf, 1)   // Csiz (num components)
	// Component 0: 8-bit unsigned
	buf = append(buf, 7) // Ssiz: (7 & 0x7F) + 1 = 8 bits
	buf = append(buf, 1) // XRsiz
	buf = append(buf, 1) // YRsiz

	// COD marker
	buf = appendUint16(buf, markerCOD)
	buf = appendUint16(buf, 12) // Lcod
	buf = append(buf, 0)        // Scod
	buf = append(buf, 0)        // SGcod progression order (LRCP)
	buf = appendUint16(buf, 1)  // SGcod num layers
	buf = append(buf, 0)        // SGcod MCT (no transform)
	buf = append(buf, 5)        // SPcod num decomp levels
	buf = append(buf, 2)        // SPcod code block width exp (2 -> 2^4 = 16)
	buf = append(buf, 2)        // SPcod code block height exp (2 -> 2^4 = 16)
	buf = append(buf, 0)        // SPcod code block style
	buf = append(buf, 1)        // SPcod wavelet (1 = 5/3)

	// QCD marker (no quantization)
	buf = appendUint16(buf, markerQCD)
	buf = appendUint16(buf, 3+16) // Lqcd: 3 header + 16 exponents
	buf = append(buf, 0)          // Sqcd: style 0 (no quant), 0 guard bits
	// Exponents for subbands (5 levels -> 16 subbands: 1 LL + 3*5 = 16)
	for range 16 {
		buf = append(buf, byte(8<<3)) // exponent = 8
	}

	// EOC marker
	buf = appendUint16(buf, markerEOC)

	return buf
}

func appendUint16(buf []byte, val uint16) []byte {
	b := make([]byte, 2)
	binary.BigEndian.PutUint16(b, val)
	return append(buf, b...)
}

func appendUint32(buf []byte, val uint32) []byte {
	b := make([]byte, 4)
	binary.BigEndian.PutUint32(b, val)
	return append(buf, b...)
}

func TestMarkerDetection(t *testing.T) {
	tests := []struct {
		name    string
		data    []byte
		wantErr bool
	}{
		{
			name:    "valid SOC marker",
			data:    buildMinimalCodestream(),
			wantErr: false,
		},
		{
			name:    "invalid SOC marker",
			data:    []byte{0xFF, 0x00, 0xFF, 0x51},
			wantErr: true,
		},
		{
			name:    "truncated data",
			data:    []byte{0xFF},
			wantErr: true,
		},
		{
			name:    "empty data",
			data:    []byte{},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, _, err := parseCodestream(tt.data)
			if (err != nil) != tt.wantErr {
				t.Errorf("parseCodestream() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestParseSIZ(t *testing.T) {
	data := buildMinimalCodestream()
	header, _, err := parseCodestream(data)
	if err != nil {
		t.Fatalf("parseCodestream() failed: %v", err)
	}

	// Verify SIZ marker parsed correctly
	if header.Width != 256 {
		t.Errorf("Width = %d, want 256", header.Width)
	}
	if header.Height != 256 {
		t.Errorf("Height = %d, want 256", header.Height)
	}
	if header.NumComps != 1 {
		t.Errorf("NumComps = %d, want 1", header.NumComps)
	}
	if len(header.BitDepth) != 1 || header.BitDepth[0] != 8 {
		t.Errorf("BitDepth = %v, want [8]", header.BitDepth)
	}
	if len(header.Signed) != 1 || header.Signed[0] != false {
		t.Errorf("Signed = %v, want [false]", header.Signed)
	}
	if header.TileWidth != 256 {
		t.Errorf("TileWidth = %d, want 256", header.TileWidth)
	}
	if header.TileHeight != 256 {
		t.Errorf("TileHeight = %d, want 256", header.TileHeight)
	}
	if header.NumXTiles != 1 {
		t.Errorf("NumXTiles = %d, want 1", header.NumXTiles)
	}
	if header.NumYTiles != 1 {
		t.Errorf("NumYTiles = %d, want 1", header.NumYTiles)
	}
	if header.NumTiles != 1 {
		t.Errorf("NumTiles = %d, want 1", header.NumTiles)
	}
}

func TestParseCOD(t *testing.T) {
	data := buildMinimalCodestream()
	header, _, err := parseCodestream(data)
	if err != nil {
		t.Fatalf("parseCodestream() failed: %v", err)
	}

	// Verify COD marker parsed correctly
	if header.ProgressionOrder != 0 {
		t.Errorf("ProgressionOrder = %d, want 0 (LRCP)", header.ProgressionOrder)
	}
	if header.NumLayers != 1 {
		t.Errorf("NumLayers = %d, want 1", header.NumLayers)
	}
	if header.MCT != false {
		t.Errorf("MCT = %v, want false", header.MCT)
	}
	if header.NumDecompLevels != 5 {
		t.Errorf("NumDecompLevels = %d, want 5", header.NumDecompLevels)
	}
	if header.CodeBlockWidth != 16 {
		t.Errorf("CodeBlockWidth = %d, want 16", header.CodeBlockWidth)
	}
	if header.CodeBlockHeight != 16 {
		t.Errorf("CodeBlockHeight = %d, want 16", header.CodeBlockHeight)
	}
	if header.WaveletFilter != Wavelet53 {
		t.Errorf("WaveletFilter = %v, want Wavelet53", header.WaveletFilter)
	}
}

func TestParseQCD(t *testing.T) {
	data := buildMinimalCodestream()
	header, _, err := parseCodestream(data)
	if err != nil {
		t.Fatalf("parseCodestream() failed: %v", err)
	}

	// Verify QCD marker parsed correctly
	if header.QuantStyle != 0 {
		t.Errorf("QuantStyle = %d, want 0 (no quantization)", header.QuantStyle)
	}
	if header.GuardBits != 0 {
		t.Errorf("GuardBits = %d, want 0", header.GuardBits)
	}
	if len(header.Exponents) != 16 {
		t.Errorf("len(Exponents) = %d, want 16", len(header.Exponents))
	}
	// Verify all exponents are 8
	for i, exp := range header.Exponents {
		if exp != 8 {
			t.Errorf("Exponents[%d] = %d, want 8", i, exp)
		}
	}
}

func TestParseQCDScalarDerived(t *testing.T) {
	buf := make([]byte, 0, 256)

	// SOC marker
	buf = appendUint16(buf, markerSOC)

	// Minimal SIZ
	buf = appendUint16(buf, markerSIZ)
	sizLen := uint16(38 + 3)
	buf = appendUint16(buf, sizLen)
	buf = appendUint16(buf, 0)   // Rsiz
	buf = appendUint32(buf, 256) // Xsiz
	buf = appendUint32(buf, 256) // Ysiz
	buf = appendUint32(buf, 0)   // XOsiz
	buf = appendUint32(buf, 0)   // YOsiz
	buf = appendUint32(buf, 256) // XTsiz
	buf = appendUint32(buf, 256) // YTsiz
	buf = appendUint32(buf, 0)   // XTOsiz
	buf = appendUint32(buf, 0)   // YTOsiz
	buf = appendUint16(buf, 1)   // Csiz
	buf = append(buf, 7, 1, 1)   // Component info

	// QCD marker (scalar derived)
	buf = appendUint16(buf, markerQCD)
	buf = appendUint16(buf, 5) // Lqcd: 3 header + 2 bytes for LL band
	buf = append(buf, 1)       // Sqcd: style 1 (scalar derived), 0 guard bits

	// Step size for LL band: exp=10, mant=1024
	// val = (10 << 11) | 1024 = 20480 + 1024 = 21504
	buf = appendUint16(buf, (10<<11)|1024)

	// EOC marker
	buf = appendUint16(buf, markerEOC)

	header, _, err := parseCodestream(buf)
	if err != nil {
		t.Fatalf("parseCodestream() failed: %v", err)
	}

	if header.QuantStyle != 1 {
		t.Errorf("QuantStyle = %d, want 1 (scalar derived)", header.QuantStyle)
	}
	if len(header.Exponents) != 1 || header.Exponents[0] != 10 {
		t.Errorf("Exponents = %v, want [10]", header.Exponents)
	}
	if len(header.Mantissas) != 1 || header.Mantissas[0] != 1024 {
		t.Errorf("Mantissas = %v, want [1024]", header.Mantissas)
	}
	if len(header.StepSizes) != 1 {
		t.Fatalf("len(StepSizes) = %d, want 1", len(header.StepSizes))
	}
	// Step size per ITU-T T.800 equation E-2:
	// Δ_b = 2^(R_b - ε_b) × (1 + μ_b / 2^11)
	// With R_b=8 (bit depth), ε_b=10, μ_b=1024:
	// stepSize = 2^(8-10) × (1 + 1024/2048) = 0.25 × 1.5 = 0.375
	expectedStepSize := 0.375
	if header.StepSizes[0] != expectedStepSize {
		t.Errorf("StepSizes[0] = %f, want %f", header.StepSizes[0], expectedStepSize)
	}
}

func TestParseQCDScalarExpounded(t *testing.T) {
	buf := make([]byte, 0, 256)

	// SOC marker
	buf = appendUint16(buf, markerSOC)

	// Minimal SIZ
	buf = appendUint16(buf, markerSIZ)
	sizLen := uint16(38 + 3)
	buf = appendUint16(buf, sizLen)
	buf = appendUint16(buf, 0)   // Rsiz
	buf = appendUint32(buf, 256) // Xsiz
	buf = appendUint32(buf, 256) // Ysiz
	buf = appendUint32(buf, 0)   // XOsiz
	buf = appendUint32(buf, 0)   // YOsiz
	buf = appendUint32(buf, 256) // XTsiz
	buf = appendUint32(buf, 256) // YTsiz
	buf = appendUint32(buf, 0)   // XTOsiz
	buf = appendUint32(buf, 0)   // YTOsiz
	buf = appendUint16(buf, 1)   // Csiz
	buf = append(buf, 7, 1, 1)   // Component info

	// QCD marker (scalar expounded)
	buf = appendUint16(buf, markerQCD)
	buf = appendUint16(buf, 3+4) // Lqcd: 3 header + 4 bytes (2 subbands)
	buf = append(buf, 2)         // Sqcd: style 2 (scalar expounded), 0 guard bits

	// Two step sizes
	buf = appendUint16(buf, (11<<11)|512) // exp=11, mant=512
	buf = appendUint16(buf, (10<<11)|256) // exp=10, mant=256

	// EOC marker
	buf = appendUint16(buf, markerEOC)

	header, _, err := parseCodestream(buf)
	if err != nil {
		t.Fatalf("parseCodestream() failed: %v", err)
	}

	if header.QuantStyle != 2 {
		t.Errorf("QuantStyle = %d, want 2 (scalar expounded)", header.QuantStyle)
	}
	if len(header.Exponents) != 2 {
		t.Fatalf("len(Exponents) = %d, want 2", len(header.Exponents))
	}
	if header.Exponents[0] != 11 {
		t.Errorf("Exponents[0] = %d, want 11", header.Exponents[0])
	}
	if header.Exponents[1] != 10 {
		t.Errorf("Exponents[1] = %d, want 10", header.Exponents[1])
	}
	if len(header.Mantissas) != 2 {
		t.Fatalf("len(Mantissas) = %d, want 2", len(header.Mantissas))
	}
	if header.Mantissas[0] != 512 {
		t.Errorf("Mantissas[0] = %d, want 512", header.Mantissas[0])
	}
	if header.Mantissas[1] != 256 {
		t.Errorf("Mantissas[1] = %d, want 256", header.Mantissas[1])
	}
}

func TestParseSOTBasic(t *testing.T) {
	// Test SOT marker parsing directly
	buf := make([]byte, 0, 32)

	// SOT segment
	buf = appendUint16(buf, 10)  // Lsot
	buf = appendUint16(buf, 0)   // Isot (tile index)
	buf = appendUint32(buf, 100) // Psot (tile-part length)
	buf = append(buf, 0)         // TPsot (tile-part index)
	buf = append(buf, 1)         // TNsot (number of tile-parts)

	tileIndex, tilePartIndex, length, _, err := parseSOT(buf, 0)
	if err != nil {
		t.Fatalf("parseSOT() failed: %v", err)
	}

	if tileIndex != 0 {
		t.Errorf("tileIndex = %d, want 0", tileIndex)
	}
	if tilePartIndex != 0 {
		t.Errorf("tilePartIndex = %d, want 0", tilePartIndex)
	}
	if length != 100 {
		t.Errorf("length = %d, want 100", length)
	}
}

func TestParseInvalidProgression(t *testing.T) {
	buf := make([]byte, 0, 256)

	// SOC marker
	buf = appendUint16(buf, markerSOC)

	// Minimal SIZ
	buf = appendUint16(buf, markerSIZ)
	sizLen := uint16(38 + 3)
	buf = appendUint16(buf, sizLen)
	buf = appendUint16(buf, 0)   // Rsiz
	buf = appendUint32(buf, 256) // Xsiz
	buf = appendUint32(buf, 256) // Ysiz
	buf = appendUint32(buf, 0)   // XOsiz
	buf = appendUint32(buf, 0)   // YOsiz
	buf = appendUint32(buf, 256) // XTsiz
	buf = appendUint32(buf, 256) // YTsiz
	buf = appendUint32(buf, 0)   // XTOsiz
	buf = appendUint32(buf, 0)   // YTOsiz
	buf = appendUint16(buf, 1)   // Csiz
	buf = append(buf, 7, 1, 1)   // Component info

	// COD marker with invalid progression order
	buf = appendUint16(buf, markerCOD)
	buf = appendUint16(buf, 12) // Lcod
	buf = append(buf, 0)        // Scod
	buf = append(buf, 99)       // SGcod progression order (invalid - > 4)
	buf = appendUint16(buf, 1)  // SGcod num layers
	buf = append(buf, 0)        // SGcod MCT
	buf = append(buf, 5)        // SPcod num decomp levels
	buf = append(buf, 2)        // SPcod code block width exp
	buf = append(buf, 2)        // SPcod code block height exp
	buf = append(buf, 0)        // SPcod code block style
	buf = append(buf, 1)        // SPcod wavelet

	// EOC marker
	buf = appendUint16(buf, markerEOC)

	_, _, err := parseCodestream(buf)
	if err == nil {
		t.Error("parseCodestream() expected error for invalid progression order, got nil")
	}
}

func TestMultiComponentImage(t *testing.T) {
	buf := make([]byte, 0, 256)

	// SOC marker
	buf = appendUint16(buf, markerSOC)

	// SIZ marker with 3 components (RGB)
	buf = appendUint16(buf, markerSIZ)
	sizLen := uint16(38 + 3*3) // 3 components
	buf = appendUint16(buf, sizLen)
	buf = appendUint16(buf, 0)   // Rsiz
	buf = appendUint32(buf, 512) // Xsiz
	buf = appendUint32(buf, 512) // Ysiz
	buf = appendUint32(buf, 0)   // XOsiz
	buf = appendUint32(buf, 0)   // YOsiz
	buf = appendUint32(buf, 512) // XTsiz
	buf = appendUint32(buf, 512) // YTsiz
	buf = appendUint32(buf, 0)   // XTOsiz
	buf = appendUint32(buf, 0)   // YTOsiz
	buf = appendUint16(buf, 3)   // Csiz (3 components)
	// Component 0: R
	buf = append(buf, 7, 1, 1)
	// Component 1: G
	buf = append(buf, 7, 1, 1)
	// Component 2: B
	buf = append(buf, 7, 1, 1)

	// EOC marker
	buf = appendUint16(buf, markerEOC)

	header, _, err := parseCodestream(buf)
	if err != nil {
		t.Fatalf("parseCodestream() failed: %v", err)
	}

	if header.NumComps != 3 {
		t.Errorf("NumComps = %d, want 3", header.NumComps)
	}
	if len(header.BitDepth) != 3 {
		t.Errorf("len(BitDepth) = %d, want 3", len(header.BitDepth))
	}
	for i, depth := range header.BitDepth {
		if depth != 8 {
			t.Errorf("BitDepth[%d] = %d, want 8", i, depth)
		}
	}
}

func TestParseTLM(t *testing.T) {
	tests := []struct {
		name           string
		stlm           byte // Stlm value: bits 5-4 = ST, bit 6 = SP
		entries        []TLMEntry
		wantLen        int
	}{
		{
			name: "16-bit tile index, 32-bit length",
			stlm: 0x60, // ST=2 (16-bit), SP=1 (32-bit)
			entries: []TLMEntry{
				{TileIndex: 0, TilePartLen: 1000},
				{TileIndex: 1, TilePartLen: 2000},
			},
			wantLen: 2,
		},
		{
			name: "8-bit tile index, 16-bit length",
			stlm: 0x10, // ST=1 (8-bit), SP=0 (16-bit)
			entries: []TLMEntry{
				{TileIndex: 0, TilePartLen: 500},
				{TileIndex: 1, TilePartLen: 600},
				{TileIndex: 2, TilePartLen: 700},
			},
			wantLen: 3,
		},
		{
			name: "no tile index, 16-bit length (implied order)",
			stlm: 0x00, // ST=0 (none), SP=0 (16-bit)
			entries: []TLMEntry{
				{TileIndex: 0, TilePartLen: 100},
				{TileIndex: 1, TilePartLen: 200},
			},
			wantLen: 2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Build TLM segment
			buf := make([]byte, 0, 128)

			// Calculate entry size
			st := (tt.stlm >> 4) & 0x03
			sp := (tt.stlm >> 6) & 0x01
			ttlmSize := 0
			switch st {
			case 1:
				ttlmSize = 1
			case 2:
				ttlmSize = 2
			}
			ptlmSize := 2
			if sp == 1 {
				ptlmSize = 4
			}
			entrySize := ttlmSize + ptlmSize

			// Ltlm = 2 (itself) + 1 (Ztlm) + 1 (Stlm) + entries
			segLen := uint16(4 + len(tt.entries)*entrySize)
			buf = appendUint16(buf, segLen)
			buf = append(buf, 0)       // Ztlm
			buf = append(buf, tt.stlm) // Stlm

			// Add entries
			for _, e := range tt.entries {
				switch st {
				case 1:
					buf = append(buf, byte(e.TileIndex))
				case 2:
					buf = appendUint16(buf, uint16(e.TileIndex))
				}
				if sp == 0 {
					buf = appendUint16(buf, uint16(e.TilePartLen))
				} else {
					buf = appendUint32(buf, uint32(e.TilePartLen))
				}
			}

			// Parse
			header := &MainHeader{}
			_, err := parseTLM(buf, 0, header)
			if err != nil {
				t.Fatalf("parseTLM() error: %v", err)
			}

			if len(header.TLMEntries) != tt.wantLen {
				t.Fatalf("got %d entries, want %d", len(header.TLMEntries), tt.wantLen)
			}

			for i, got := range header.TLMEntries {
				want := tt.entries[i]
				if st == 0 {
					// Implied order - tile index is sequential
					want.TileIndex = i
				}
				if got.TileIndex != want.TileIndex {
					t.Errorf("entry[%d].TileIndex = %d, want %d", i, got.TileIndex, want.TileIndex)
				}
				if got.TilePartLen != want.TilePartLen {
					t.Errorf("entry[%d].TilePartLen = %d, want %d", i, got.TilePartLen, want.TilePartLen)
				}
			}
		})
	}
}
