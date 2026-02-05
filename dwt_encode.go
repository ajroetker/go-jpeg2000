package jpeg2000

// Analyze2D_53 performs forward 2D 5/3 wavelet transform (lossless).
// Input: image samples in coeffs[y][x].
// Output: subbands arranged as [LL, LH; HL, HH] per level.
// width, height: dimensions of the full image.
// levels: number of decomposition levels.
//
// Per JPEG2000 standard (ITU-T T.800): forward transform applies vertical
// analysis first (columns), then horizontal analysis (rows). This is the
// opposite of the inverse transform which does horizontal then vertical.
func Analyze2D_53(coeffs [][]int32, width, height, levels int) {
	if levels < 1 {
		return
	}

	// Process from finest to coarsest level
	for level := 1; level <= levels; level++ {
		// At level L (1-indexed), the working region is the LL subband
		// from the previous decomposition, with dimensions ceil(size / 2^(L-1)).
		// This matches the dimension formula used in Synthesize2D_53.
		levelWidth := (width + (1 << (level - 1)) - 1) >> (level - 1)
		levelHeight := (height + (1 << (level - 1)) - 1) >> (level - 1)

		// Vertical analysis first (process columns)
		for x := range levelWidth {
			col := make([]int32, levelHeight)
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}
			analyze1D_53(col)
			for y := range levelHeight {
				coeffs[y][x] = col[y]
			}
		}

		// Horizontal analysis second (process rows)
		for y := range levelHeight {
			analyze1D_53(coeffs[y][:levelWidth])
		}
	}
}

// Analyze2D_97 performs forward 2D 9/7 wavelet transform (lossy).
// Input: image samples in coeffs[y][x].
// Output: subbands arranged as [LL, LH; HL, HH] per level.
// width, height: dimensions of the full image.
// levels: number of decomposition levels.
//
// Per JPEG2000 standard (ITU-T T.800): forward transform applies vertical
// analysis first (columns), then horizontal analysis (rows). This is the
// opposite of the inverse transform which does horizontal then vertical.
func Analyze2D_97(coeffs [][]float64, width, height, levels int) {
	if levels < 1 {
		return
	}

	// Process from finest to coarsest level
	for level := 1; level <= levels; level++ {
		// At level L (1-indexed), the working region is the LL subband
		// from the previous decomposition, with dimensions ceil(size / 2^(L-1)).
		// This matches the dimension formula used in Synthesize2D_97.
		levelWidth := (width + (1 << (level - 1)) - 1) >> (level - 1)
		levelHeight := (height + (1 << (level - 1)) - 1) >> (level - 1)

		// Vertical analysis first (process columns)
		for x := range levelWidth {
			col := make([]float64, levelHeight)
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}
			analyze1D_97(col)
			for y := range levelHeight {
				coeffs[y][x] = col[y]
			}
		}

		// Horizontal analysis second (process rows)
		for y := range levelHeight {
			analyze1D_97(coeffs[y][:levelWidth])
		}
	}
}
