package jpeg2000

import (
	"math"
	"testing"
)

func TestApplyRCT(t *testing.T) {
	tests := []struct {
		name                string
		y, cb, cr           [][]int32
		wantR, wantG, wantB [][]int32
	}{
		{
			name:  "simple values",
			y:     [][]int32{{128}},
			cb:    [][]int32{{0}},
			cr:    [][]int32{{0}},
			wantR: [][]int32{{128}},
			wantG: [][]int32{{128}},
			wantB: [][]int32{{128}},
		},
		{
			name:  "red pixel",
			y:     [][]int32{{191}},  // From forward: (255 + 2*128 + 0) / 4 = 511 / 4 = 127... actually need to recalculate
			cb:    [][]int32{{-128}}, // B - G = 0 - 128 = -128
			cr:    [][]int32{{127}},  // R - G = 255 - 128 = 127
			wantR: [][]int32{{319}},  // Cr + G = 127 + 192 = 319 (will be clamped to 255 later)
			wantG: [][]int32{{192}},  // Y - (Cb + Cr) >> 2 = 191 - ((-128 + 127) >> 2) = 191 - (-1 >> 2) = 191 - 0 = 191 + 1 = 192
			wantB: [][]int32{{64}},   // Cb + G = -128 + 192 = 64
		},
		{
			name: "2x2 block",
			y: [][]int32{
				{100, 150},
				{120, 140},
			},
			cb: [][]int32{
				{10, -10},
				{20, -20},
			},
			cr: [][]int32{
				{5, -5},
				{15, -15},
			},
			wantR: [][]int32{
				{102, 149}, // cr + g: 5 + 97 = 102, -5 + 154 = 149
				{127, 134}, // 15 + 112 = 127, -15 + 149 = 134
			},
			wantG: [][]int32{
				{97, 154},  // y - (cb + cr) >> 2: 100 - (10+5)>>2 = 100 - 3 = 97, 150 - (-10-5)>>2 = 150 - (-4) = 154
				{112, 149}, // 120 - (20+15)>>2 = 120 - 8 = 112, 140 - (-20-15)>>2 = 140 - (-9) = 149
			},
			wantB: [][]int32{
				{107, 144}, // cb + g: 10 + 97 = 107, -10 + 154 = 144
				{132, 129}, // 20 + 112 = 132, -20 + 149 = 129
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r, g, b := applyRCT(tt.y, tt.cb, tt.cr)

			if !equal2D(r, tt.wantR) {
				t.Errorf("R mismatch:\ngot:  %v\nwant: %v", r, tt.wantR)
			}
			if !equal2D(g, tt.wantG) {
				t.Errorf("G mismatch:\ngot:  %v\nwant: %v", g, tt.wantG)
			}
			if !equal2D(b, tt.wantB) {
				t.Errorf("B mismatch:\ngot:  %v\nwant: %v", b, tt.wantB)
			}
		})
	}
}

func TestApplyICT(t *testing.T) {
	tests := []struct {
		name                string
		y, cb, cr           [][]float64
		wantR, wantG, wantB [][]float64
		tolerance           float64
	}{
		{
			name:      "neutral gray",
			y:         [][]float64{{128.0}},
			cb:        [][]float64{{0.0}},
			cr:        [][]float64{{0.0}},
			wantR:     [][]float64{{128.0}},
			wantG:     [][]float64{{128.0}},
			wantB:     [][]float64{{128.0}},
			tolerance: 0.01,
		},
		{
			name:      "red-ish pixel",
			y:         [][]float64{{150.0}},
			cb:        [][]float64{{-50.0}},
			cr:        [][]float64{{50.0}},
			wantR:     [][]float64{{220.1}}, // 150 + 1.402 * 50 = 220.1
			wantG:     [][]float64{{131.5}}, // 150 - 0.344136 * (-50) - 0.714136 * 50 = 150 + 17.2068 - 35.7068 = 131.5
			wantB:     [][]float64{{61.4}},  // 150 + 1.772 * (-50) = 61.4
			tolerance: 0.5,
		},
		{
			name: "2x2 block",
			y: [][]float64{
				{100.0, 150.0},
				{120.0, 140.0},
			},
			cb: [][]float64{
				{10.0, -10.0},
				{20.0, -20.0},
			},
			cr: [][]float64{
				{5.0, -5.0},
				{15.0, -15.0},
			},
			wantR: [][]float64{
				{107.01, 142.99}, // Y + 1.402 * Cr: 100 + 1.402*5 = 107.01, 150 + 1.402*(-5) = 142.99
				{141.03, 118.97}, // 120 + 1.402*15 = 141.03, 140 + 1.402*(-15) = 118.97
			},
			wantG: [][]float64{
				{92.98796, 157.01204},  // Y - 0.344136*Cb - 0.714136*Cr: 100 - 0.344136*10 - 0.714136*5 = 92.98796
				{102.40524, 157.59476}, // 120 - 0.344136*20 - 0.714136*15 = 102.40524, 140 - 0.344136*(-20) - 0.714136*(-15) = 157.59476
			},
			wantB: [][]float64{
				{117.72, 132.28}, // Y + 1.772 * Cb: 100 + 1.772*10 = 117.72, 150 + 1.772*(-10) = 132.28
				{155.44, 104.56}, // 120 + 1.772*20 = 155.44, 140 + 1.772*(-20) = 104.56
			},
			tolerance: 0.01,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r, g, b := applyICT(tt.y, tt.cb, tt.cr)

			if !approxEqual2D(r, tt.wantR, tt.tolerance) {
				t.Errorf("R mismatch:\ngot:  %v\nwant: %v", r, tt.wantR)
			}
			if !approxEqual2D(g, tt.wantG, tt.tolerance) {
				t.Errorf("G mismatch:\ngot:  %v\nwant: %v", g, tt.wantG)
			}
			if !approxEqual2D(b, tt.wantB, tt.tolerance) {
				t.Errorf("B mismatch:\ngot:  %v\nwant: %v", b, tt.wantB)
			}
		})
	}
}

func TestClampToUint8(t *testing.T) {
	tests := []struct {
		input int32
		want  uint8
	}{
		{-100, 0},
		{-1, 0},
		{0, 0},
		{127, 127},
		{255, 255},
		{256, 255},
		{1000, 255},
	}

	for _, tt := range tests {
		got := clampToUint8(tt.input)
		if got != tt.want {
			t.Errorf("clampToUint8(%d) = %d, want %d", tt.input, got, tt.want)
		}
	}
}

func TestClampFloat(t *testing.T) {
	tests := []struct {
		input float64
		want  uint8
	}{
		{-100.5, 0},
		{-0.1, 0},
		{0.0, 0},
		{0.4, 0},
		{0.5, 1},
		{127.3, 127},
		{127.6, 128},
		{255.0, 255},
		{255.4, 255},
		{255.6, 255},
		{1000.0, 255},
	}

	for _, tt := range tests {
		got := clampFloat(tt.input)
		if got != tt.want {
			t.Errorf("clampFloat(%f) = %d, want %d", tt.input, got, tt.want)
		}
	}
}

func TestConvertToRGBA_Grayscale(t *testing.T) {
	// Input coefficients are signed (DC level shift already applied during encoding)
	// To get pixel value P, coefficient = P - 128 (for 8-bit unsigned)
	components := [][][]int32{
		{
			{-128, 0},  // pixel 0, pixel 128
			{127, -64}, // pixel 255, pixel 64
		},
	}

	img := convertToRGBA(components, 2, 2, []int{8}, []bool{false}, true)

	// Check dimensions
	if img.Bounds().Dx() != 2 || img.Bounds().Dy() != 2 {
		t.Errorf("wrong dimensions: got %v", img.Bounds())
	}

	// Check pixels (grayscale should have R=G=B)
	tests := []struct {
		x, y int
		want uint8
	}{
		{0, 0, 0},
		{1, 0, 128},
		{0, 1, 255},
		{1, 1, 64},
	}

	for _, tt := range tests {
		r, g, b, a := img.At(tt.x, tt.y).RGBA()
		// RGBA() returns uint32 values in [0, 65535], so convert back
		rr, gg, bb, aa := uint8(r>>8), uint8(g>>8), uint8(b>>8), uint8(a>>8)

		if rr != tt.want || gg != tt.want || bb != tt.want {
			t.Errorf("pixel (%d,%d): got RGB(%d,%d,%d), want (%d,%d,%d)",
				tt.x, tt.y, rr, gg, bb, tt.want, tt.want, tt.want)
		}
		if aa != 255 {
			t.Errorf("pixel (%d,%d): alpha = %d, want 255", tt.x, tt.y, aa)
		}
	}
}

func TestConvertToRGBA_RGB_NoTransform(t *testing.T) {
	// Input coefficients are signed (DC level shift already applied during encoding)
	// To get pixel value P, coefficient = P - 128 (for 8-bit unsigned)
	components := [][][]int32{
		{{127, -128}},  // R: pixel 255, pixel 0
		{{-128, 127}},  // G: pixel 0, pixel 255
		{{-128, -128}}, // B: pixel 0, pixel 0
	}

	img := convertToRGBA(components, 2, 1, []int{8, 8, 8}, []bool{false, false, false}, false)

	tests := []struct {
		x       int
		r, g, b uint8
	}{
		{0, 255, 0, 0}, // Red
		{1, 0, 255, 0}, // Green
	}

	for _, tt := range tests {
		r, g, b, a := img.At(tt.x, 0).RGBA()
		rr, gg, bb, aa := uint8(r>>8), uint8(g>>8), uint8(b>>8), uint8(a>>8)

		if rr != tt.r || gg != tt.g || bb != tt.b {
			t.Errorf("pixel %d: got RGB(%d,%d,%d), want RGB(%d,%d,%d)",
				tt.x, rr, gg, bb, tt.r, tt.g, tt.b)
		}
		if aa != 255 {
			t.Errorf("pixel %d: alpha = %d, want 255", tt.x, aa)
		}
	}
}

func TestConvertToRGBA_SignedValues(t *testing.T) {
	// Signed 8-bit: range is -128 to 127
	// For display, signed values must be mapped to unsigned [0, 255] by adding 128.
	// This is the same offset used for unsigned data (inverse DC level shift).
	components := [][][]int32{
		{{-128, 0, 127}}, // Grayscale
	}

	img := convertToRGBA(components, 3, 1, []int{8}, []bool{true}, true)

	tests := []struct {
		x    int
		want uint8
	}{
		{0, 0},   // -128 + 128 = 0
		{1, 128}, // 0 + 128 = 128
		{2, 255}, // 127 + 128 = 255
	}

	for _, tt := range tests {
		r, g, b, _ := img.At(tt.x, 0).RGBA()
		rr, gg, bb := uint8(r>>8), uint8(g>>8), uint8(b>>8)

		if rr != tt.want || gg != tt.want || bb != tt.want {
			t.Errorf("pixel %d: got RGB(%d,%d,%d), want (%d,%d,%d)",
				tt.x, rr, gg, bb, tt.want, tt.want, tt.want)
		}
	}
}

func TestConvertToRGBAFloat_Grayscale(t *testing.T) {
	// Input coefficients are signed (DC level shift already applied during encoding)
	// To get pixel value P, coefficient = P - 128 (for 8-bit unsigned)
	components := [][][]float64{
		{
			{-128.0, 0.5},   // pixel 0, pixel 128.5 → 129
			{127.0, -63.7},  // pixel 255, pixel 64.3 → 64
		},
	}

	img := convertToRGBAFloat(components, 2, 2, []int{8}, []bool{false})

	tests := []struct {
		x, y int
		want uint8
	}{
		{0, 0, 0},
		{1, 0, 129}, // 0.5 + 128 = 128.5 → 129
		{0, 1, 255}, // 127 + 128 = 255
		{1, 1, 64},  // -63.7 + 128 = 64.3 → 64
	}

	for _, tt := range tests {
		r, g, b, a := img.At(tt.x, tt.y).RGBA()
		rr, gg, bb, aa := uint8(r>>8), uint8(g>>8), uint8(b>>8), uint8(a>>8)

		if rr != tt.want || gg != tt.want || bb != tt.want {
			t.Errorf("pixel (%d,%d): got RGB(%d,%d,%d), want (%d,%d,%d)",
				tt.x, tt.y, rr, gg, bb, tt.want, tt.want, tt.want)
		}
		if aa != 255 {
			t.Errorf("pixel (%d,%d): alpha = %d, want 255", tt.x, tt.y, aa)
		}
	}
}

func TestConvertToRGBAFloat_ICT(t *testing.T) {
	// Create simple YCbCr values and convert with ICT
	// Input coefficients are signed: to get pixel 128, coefficient = 128 - 128 = 0
	components := [][][]float64{
		{{0.0}},   // Y: pixel 128
		{{0.0}},   // Cb: neutral
		{{0.0}},   // Cr: neutral
	}

	img := convertToRGBAFloat(components, 1, 1, []int{8, 8, 8}, []bool{false, false, false})

	r, g, b, a := img.At(0, 0).RGBA()
	rr, gg, bb, aa := uint8(r>>8), uint8(g>>8), uint8(b>>8), uint8(a>>8)

	// Neutral gray should stay neutral (Y=0 → pixel 128 after offset)
	if rr != 128 || gg != 128 || bb != 128 {
		t.Errorf("neutral gray: got RGB(%d,%d,%d), want RGB(128,128,128)", rr, gg, bb)
	}
	if aa != 255 {
		t.Errorf("alpha = %d, want 255", aa)
	}
}

func TestRCT_RoundTrip(t *testing.T) {
	// Test that forward + inverse RCT is approximately identity
	// Forward RCT:
	// Y = (R + 2G + B) / 4
	// Cb = B - G
	// Cr = R - G

	r := [][]int32{{255, 128, 0}}
	g := [][]int32{{0, 128, 255}}
	b := [][]int32{{0, 0, 128}}

	// Forward transform
	y := make([][]int32, 1)
	cb := make([][]int32, 1)
	cr := make([][]int32, 1)
	y[0] = make([]int32, 3)
	cb[0] = make([]int32, 3)
	cr[0] = make([]int32, 3)

	for i := range 3 {
		y[0][i] = (r[0][i] + 2*g[0][i] + b[0][i]) >> 2
		cb[0][i] = b[0][i] - g[0][i]
		cr[0][i] = r[0][i] - g[0][i]
	}

	// Inverse transform
	rOut, gOut, bOut := applyRCT(y, cb, cr)

	// Check round-trip
	for i := range 3 {
		if rOut[0][i] != r[0][i] {
			t.Errorf("R[%d]: got %d, want %d", i, rOut[0][i], r[0][i])
		}
		if gOut[0][i] != g[0][i] {
			t.Errorf("G[%d]: got %d, want %d", i, gOut[0][i], g[0][i])
		}
		if bOut[0][i] != b[0][i] {
			t.Errorf("B[%d]: got %d, want %d", i, bOut[0][i], b[0][i])
		}
	}
}

// Helper functions

func equal2D(a, b [][]int32) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if len(a[i]) != len(b[i]) {
			return false
		}
		for j := range a[i] {
			if a[i][j] != b[i][j] {
				return false
			}
		}
	}
	return true
}

func approxEqual2D(a, b [][]float64, tolerance float64) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if len(a[i]) != len(b[i]) {
			return false
		}
		for j := range a[i] {
			if math.Abs(a[i][j]-b[i][j]) > tolerance {
				return false
			}
		}
	}
	return true
}
