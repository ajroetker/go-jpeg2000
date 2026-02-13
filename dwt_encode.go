package jpeg2000

import (
	"github.com/ajroetker/go-highway/hwy/contrib/wavelet"
)

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

	// Pre-allocate reusable buffers for the largest dimension.
	maxDim := max(width, height)
	maxHalf := (maxDim + 1) / 2
	low53 := make([]int32, maxHalf)
	high53 := make([]int32, maxHalf)
	col := make([]int32, height)

	// Process from finest to coarsest level
	for level := 1; level <= levels; level++ {
		levelWidth := (width + (1 << (level - 1)) - 1) >> (level - 1)
		levelHeight := (height + (1 << (level - 1)) - 1) >> (level - 1)

		// Vertical analysis first (process columns)
		for x := range levelWidth {
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}
			wavelet.Analyze53(col[:levelHeight], 0, low53, high53)
			for y := range levelHeight {
				coeffs[y][x] = col[y]
			}
		}

		// Horizontal analysis second (process rows)
		for y := range levelHeight {
			wavelet.Analyze53(coeffs[y][:levelWidth], 0, low53, high53)
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

	// Pre-allocate column buffer outside the loop.
	col := make([]float64, height)

	// Process from finest to coarsest level
	for level := 1; level <= levels; level++ {
		levelWidth := (width + (1 << (level - 1)) - 1) >> (level - 1)
		levelHeight := (height + (1 << (level - 1)) - 1) >> (level - 1)

		// Vertical analysis first (process columns)
		for x := range levelWidth {
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}
			analyze1D_97(col[:levelHeight])
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
