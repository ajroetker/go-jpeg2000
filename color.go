package jpeg2000

import "image"

// applyRCT applies inverse Reversible Color Transform (lossless)
// Converts from YCbCr to RGB using integer arithmetic
// Y = (R + 2G + B) / 4  =>  G = Y - (Cb + Cr) / 4
// Cb = B - G            =>  B = Cb + G
// Cr = R - G            =>  R = Cr + G
func applyRCT(y, cb, cr [][]int32) (r, g, b [][]int32) {
	height := len(y)
	if height == 0 {
		return nil, nil, nil
	}
	width := len(y[0])

	r = make([][]int32, height)
	g = make([][]int32, height)
	b = make([][]int32, height)

	for i := range height {
		r[i] = make([]int32, width)
		g[i] = make([]int32, width)
		b[i] = make([]int32, width)

		for j := range width {
			// G = Y - floor((Cb + Cr) / 4)
			g[i][j] = y[i][j] - ((cb[i][j] + cr[i][j]) >> 2)
			// R = Cr + G
			r[i][j] = cr[i][j] + g[i][j]
			// B = Cb + G
			b[i][j] = cb[i][j] + g[i][j]
		}
	}

	return r, g, b
}

// applyICT applies inverse Irreversible Color Transform (lossy)
// Standard YCbCr to RGB conversion with JPEG2000 coefficients
// R = Y + 1.402 * Cr
// G = Y - 0.344136 * Cb - 0.714136 * Cr
// B = Y + 1.772 * Cb
func applyICT(y, cb, cr [][]float64) (r, g, b [][]float64) {
	height := len(y)
	if height == 0 {
		return nil, nil, nil
	}
	width := len(y[0])

	r = make([][]float64, height)
	g = make([][]float64, height)
	b = make([][]float64, height)

	const (
		crToR = 1.402
		cbToG = 0.344136
		crToG = 0.714136
		cbToB = 1.772
	)

	for i := range height {
		r[i] = make([]float64, width)
		g[i] = make([]float64, width)
		b[i] = make([]float64, width)

		for j := range width {
			// R = Y + 1.402 * Cr
			r[i][j] = y[i][j] + crToR*cr[i][j]
			// G = Y - 0.344136 * Cb - 0.714136 * Cr
			g[i][j] = y[i][j] - cbToG*cb[i][j] - crToG*cr[i][j]
			// B = Y + 1.772 * Cb
			b[i][j] = y[i][j] + cbToB*cb[i][j]
		}
	}

	return r, g, b
}

// forwardRCT applies the forward Reversible Color Transform (lossless).
// Converts from RGB to YCbCr using integer arithmetic per ITU-T T.800 G.2:
//
//	Y  = floor((R + 2G + B) / 4)
//	Cb = B - G
//	Cr = R - G
//
// This is the exact inverse of applyRCT: forwardRCT followed by applyRCT
// yields the original RGB values (lossless round-trip).
func forwardRCT(r, g, b [][]int32) (y, cb, cr [][]int32) {
	height := len(r)
	if height == 0 {
		return nil, nil, nil
	}
	width := len(r[0])

	y = make([][]int32, height)
	cb = make([][]int32, height)
	cr = make([][]int32, height)

	for i := range height {
		y[i] = make([]int32, width)
		cb[i] = make([]int32, width)
		cr[i] = make([]int32, width)

		for j := range width {
			// Y = floor((R + 2G + B) / 4)
			y[i][j] = (r[i][j] + 2*g[i][j] + b[i][j]) >> 2
			// Cb = B - G
			cb[i][j] = b[i][j] - g[i][j]
			// Cr = R - G
			cr[i][j] = r[i][j] - g[i][j]
		}
	}

	return y, cb, cr
}

// forwardICT applies the forward Irreversible Color Transform (lossy).
// Converts from RGB to YCbCr using the standard JPEG2000 coefficients
// per ITU-T T.800 G.2:
//
//	Y  =  0.299   * R + 0.587   * G + 0.114   * B
//	Cb = -0.16875 * R - 0.33126 * G + 0.5     * B
//	Cr =  0.5     * R - 0.41869 * G - 0.08131 * B
//
// The inverse of this transform is applyICT. Note that because this uses
// floating-point arithmetic, the round-trip is not perfectly lossless.
func forwardICT(r, g, b [][]float64) (y, cb, cr [][]float64) {
	height := len(r)
	if height == 0 {
		return nil, nil, nil
	}
	width := len(r[0])

	y = make([][]float64, height)
	cb = make([][]float64, height)
	cr = make([][]float64, height)

	const (
		// Forward ICT coefficients (ITU-T T.800 Table G.2)
		rToY  = 0.299
		gToY  = 0.587
		bToY  = 0.114
		rToCb = -0.16875
		gToCb = -0.33126
		bToCb = 0.5
		rToCr = 0.5
		gToCr = -0.41869
		bToCr = -0.08131
	)

	for i := range height {
		y[i] = make([]float64, width)
		cb[i] = make([]float64, width)
		cr[i] = make([]float64, width)

		for j := range width {
			rv := r[i][j]
			gv := g[i][j]
			bv := b[i][j]

			// Y = 0.299*R + 0.587*G + 0.114*B
			y[i][j] = rToY*rv + gToY*gv + bToY*bv
			// Cb = -0.16875*R - 0.33126*G + 0.5*B
			cb[i][j] = rToCb*rv + gToCb*gv + bToCb*bv
			// Cr = 0.5*R - 0.41869*G - 0.08131*B
			cr[i][j] = rToCr*rv + gToCr*gv + bToCr*bv
		}
	}

	return y, cb, cr
}

// clampToUint8 clamps a value to [0, 255] range
func clampToUint8(v int32) uint8 {
	if v < 0 {
		return 0
	}
	if v > 255 {
		return 255
	}
	return uint8(v)
}

// clampFloat clamps a float to [0, 255] and converts to uint8
func clampFloat(v float64) uint8 {
	if v < 0 {
		return 0
	}
	if v > 255 {
		return 255
	}
	return uint8(v + 0.5) // Round to nearest
}

// getComponentSample safely gets a sample from a component, handling subsampling.
// If the component is smaller than the output dimensions, uses nearest-neighbor upsampling.
func getComponentSample(comp [][]int32, x, y, outWidth, outHeight int) int32 {
	compHeight := len(comp)
	if compHeight == 0 {
		return 0
	}
	compWidth := len(comp[0])
	if compWidth == 0 {
		return 0
	}

	// Scale coordinates if component is subsampled
	// Use nearest-neighbor interpolation
	compY := y
	compX := x
	if compHeight < outHeight {
		compY = y * compHeight / outHeight
	}
	if compWidth < outWidth {
		compX = x * compWidth / outWidth
	}

	// Bounds check
	if compY >= compHeight {
		compY = compHeight - 1
	}
	if compX >= compWidth {
		compX = compWidth - 1
	}

	return comp[compY][compX]
}

// convertToRGBA converts decoded components to image.RGBA
// components: slice of 2D coefficient arrays (1 for grayscale, 3+ for RGB/RGBA)
// bitDepths: per-component bits per sample (typically 8)
// signed: per-component signedness
// reversible: true for RCT, false for ICT
// Note: Components may have different dimensions due to subsampling (e.g., YCbCr 4:2:0)
func convertToRGBA(components [][][]int32, width, height int,
	bitDepths []int, signed []bool, reversible bool) *image.RGBA {

	img := image.NewRGBA(image.Rect(0, 0, width, height))

	// Helper to get bit depth for a component (defaults to 8)
	getBitDepth := func(c int) int {
		if c < len(bitDepths) && bitDepths[c] > 0 {
			return bitDepths[c]
		}
		return 8
	}

	// Display mapping offset: always add 2^(bitDepth-1) to convert to unsigned range.
	// For unsigned data: undoes the encoder's DC level shift (ITU-T T.800 G.1.2).
	// For signed data: converts from signed range [-2^(B-1), 2^(B-1)-1] to
	// unsigned range [0, 2^B-1] for display. OpenJPEG's opj_decompress does the
	// same when outputting to unsigned formats (PGM/PPM/PNG).
	getOffset := func(c int) int32 {
		bitDepth := getBitDepth(c)
		return int32(1 << (bitDepth - 1))
	}

	// Grayscale
	if len(components) == 1 {
		gray := components[0]
		bitDepth := getBitDepth(0)
		offset := getOffset(0)
		// Calculate proper scaling factor
		// For n-bit to 8-bit: scale by 255 / (2^n - 1)
		// This maps [0, 2^n-1] to [0, 255] correctly
		maxVal := int32((1 << bitDepth) - 1)
		for y := range height {
			for x := range width {
				val := getComponentSample(gray, x, y, width, height) + offset
				// Scale to 8-bit using proper factor
				// For example: 4-bit uses * 255 / 15 = * 17
				if bitDepth != 8 {
					val = (val*255 + maxVal/2) / maxVal
				}
				g := clampToUint8(val)
				idx := img.PixOffset(x, y)
				img.Pix[idx+0] = g   // R
				img.Pix[idx+1] = g   // G
				img.Pix[idx+2] = g   // B
				img.Pix[idx+3] = 255 // A
			}
		}
		return img
	}

	// 2-component (grayscale + alpha or similar) - use first component as grayscale
	// This matches OpenJPEG's behavior of outputting only the first component for PGM
	if len(components) == 2 {
		gray := components[0]
		bitDepth := getBitDepth(0)
		offset := getOffset(0)
		maxVal := int32((1 << bitDepth) - 1)
		for y := range height {
			for x := range width {
				val := getComponentSample(gray, x, y, width, height) + offset
				if bitDepth != 8 {
					val = (val*255 + maxVal/2) / maxVal
				}
				g := clampToUint8(val)
				idx := img.PixOffset(x, y)
				img.Pix[idx+0] = g   // R
				img.Pix[idx+1] = g   // G
				img.Pix[idx+2] = g   // B
				img.Pix[idx+3] = 255 // A
			}
		}
		return img
	}

	// RGB/RGBA with optional color transform
	if len(components) >= 3 {
		var rComp, gComp, bComp [][]int32

		if reversible {
			// Apply RCT - requires all components to have same dimensions
			// Check if components have matching dimensions
			if len(components[0]) == len(components[1]) && len(components[0]) == len(components[2]) &&
				len(components[0]) > 0 && len(components[0][0]) == len(components[1][0]) &&
				len(components[0][0]) == len(components[2][0]) {
				rComp, gComp, bComp = applyRCT(components[0], components[1], components[2])
			} else {
				// Components have different sizes, skip RCT
				rComp = components[0]
				gComp = components[1]
				bComp = components[2]
			}
		} else {
			// No color transform for integer components in ICT mode
			// (ICT uses float components, handled by convertToRGBAFloat)
			rComp = components[0]
			gComp = components[1]
			bComp = components[2]
		}

		// Calculate per-component offsets, bit depths, and max values
		rBitDepth := getBitDepth(0)
		gBitDepth := getBitDepth(1)
		bBitDepth := getBitDepth(2)
		rOffset := getOffset(0)
		gOffset := getOffset(1)
		bOffset := getOffset(2)
		rMaxVal := int32((1 << rBitDepth) - 1)
		gMaxVal := int32((1 << gBitDepth) - 1)
		bMaxVal := int32((1 << bBitDepth) - 1)

		for y := range height {
			for x := range width {
				// Use safe accessor that handles subsampling
				// Apply per-component DC offset
				rVal := getComponentSample(rComp, x, y, width, height) + rOffset
				gVal := getComponentSample(gComp, x, y, width, height) + gOffset
				bVal := getComponentSample(bComp, x, y, width, height) + bOffset

				// Scale each component to 8-bit using proper linear scaling
				// For n-bit to 8-bit: output = (val * 255 + maxVal/2) / maxVal
				// This maps [0, 2^n-1] to [0, 255] correctly
				if rBitDepth != 8 {
					rVal = (rVal * 255 + rMaxVal/2) / rMaxVal
				}
				if gBitDepth != 8 {
					gVal = (gVal * 255 + gMaxVal/2) / gMaxVal
				}
				if bBitDepth != 8 {
					bVal = (bVal * 255 + bMaxVal/2) / bMaxVal
				}

				idx := img.PixOffset(x, y)
				img.Pix[idx+0] = clampToUint8(rVal)
				img.Pix[idx+1] = clampToUint8(gVal)
				img.Pix[idx+2] = clampToUint8(bVal)
				img.Pix[idx+3] = 255
			}
		}
		return img
	}

	return img
}

// convertToRGBAFloat converts decoded float components to image.RGBA
// Used for lossy (ICT) decoding
func convertToRGBAFloat(components [][][]float64, width, height int,
	bitDepths []int, signed []bool) *image.RGBA {

	img := image.NewRGBA(image.Rect(0, 0, width, height))

	// Helper to get bit depth for a component (defaults to 8)
	getBitDepth := func(c int) int {
		if c < len(bitDepths) && bitDepths[c] > 0 {
			return bitDepths[c]
		}
		return 8
	}

	// Calculate display offset and scale for a component.
	// Always add 2^(bitDepth-1) to convert to unsigned range for display.
	getOffsetAndScale := func(c int) (float64, float64) {
		bitDepth := getBitDepth(c)
		offset := float64(uint(1) << (bitDepth - 1))
		scale := 1.0
		if bitDepth != 8 {
			scale = 255.0 / float64((uint(1)<<bitDepth)-1)
		}
		return offset, scale
	}

	// Grayscale
	if len(components) == 1 {
		gray := components[0]
		offset, scale := getOffsetAndScale(0)
		for y := range height {
			for x := range width {
				val := (gray[y][x] + offset) * scale
				g := clampFloat(val)
				idx := img.PixOffset(x, y)
				img.Pix[idx+0] = g
				img.Pix[idx+1] = g
				img.Pix[idx+2] = g
				img.Pix[idx+3] = 255
			}
		}
		return img
	}

	// RGB with ICT
	if len(components) >= 3 {
		// For ICT (irreversible color transform), the DC level shift was applied to the
		// Y component BEFORE forward ICT in the encoder. Per ITU-T T.800:
		// - Forward: RGB -> YCbCr, then Y -= 2^(B-1) for DC level shift (unsigned only)
		// - Inverse: Y += 2^(B-1), then YCbCr -> RGB (unsigned only)
		// The Y component must have DC offset added BEFORE inverse ICT, not after.
		// Cb and Cr are already centered at 0 and don't need offset.
		yComp := components[0]
		cbComp := components[1]
		crComp := components[2]

		// Add offset to Y component before ICT.
		// For unsigned: undoes encoder DC level shift. For signed: converts to unsigned range.
		bitDepth := getBitDepth(0)
		yOffset := float64(uint(1) << (bitDepth - 1)) // 2^(bitDepth-1) = 128 for 8-bit

		yShifted := make([][]float64, len(yComp))
		for i := range yComp {
			yShifted[i] = make([]float64, len(yComp[i]))
			for j := range yComp[i] {
				yShifted[i][j] = yComp[i][j] + yOffset
			}
		}

		// Now apply inverse ICT with properly shifted Y
		rComp, gComp, bComp := applyICT(yShifted, cbComp, crComp)

		// Calculate scale for non-8-bit depths
		rScale := 1.0
		gScale := 1.0
		bScale := 1.0
		if getBitDepth(0) != 8 {
			rScale = 255.0 / float64((uint(1)<<getBitDepth(0))-1)
		}
		if getBitDepth(1) != 8 {
			gScale = 255.0 / float64((uint(1)<<getBitDepth(1))-1)
		}
		if getBitDepth(2) != 8 {
			bScale = 255.0 / float64((uint(1)<<getBitDepth(2))-1)
		}

		for y := range height {
			for x := range width {
				// RGB values from ICT are already in [0, 2^B - 1] range
				// Only scale if bit depth != 8
				rVal := rComp[y][x] * rScale
				gVal := gComp[y][x] * gScale
				bVal := bComp[y][x] * bScale

				idx := img.PixOffset(x, y)
				img.Pix[idx+0] = clampFloat(rVal)
				img.Pix[idx+1] = clampFloat(gVal)
				img.Pix[idx+2] = clampFloat(bVal)
				img.Pix[idx+3] = 255
			}
		}
		return img
	}

	return img
}
