package jpeg2000

import (
	"math"

	"github.com/ajroetker/go-highway/hwy/contrib/wavelet"
)

// WaveletType indicates the wavelet filter used
type WaveletType int

const (
	Wavelet53 WaveletType = iota // 5/3 reversible (lossless)
	Wavelet97                    // 9/7 irreversible (lossy)
)

// Lifting coefficients for 9/7 irreversible filter (ITU-T T.800 Table F.4)
const (
	lift97Alpha float64 = -1.586134342059924
	lift97Beta  float64 = -0.052980118572961
	lift97Gamma float64 = 0.882911075530934
	lift97Delta float64 = 0.443506852043971
	lift97K     float64 = 1.230174104914001
	// BUG_WEIRD_TWO_INVK: OpenJPEG uses 2/K instead of 1/K for high-pass scaling,
	// compensated by using gain=0 for all subbands in 9/7 decode (see tcd.c).
	// This avoids numerical issues when n=1 DWT levels cause no-op pass-throughs
	// that would leave standard gain factors uncompensated.
	lift97TwoInvK float64 = 2.0 / 1.230174104914001
)

// dwtBufs97 holds reusable working buffers for 9/7 wavelet transforms.
// Allocated once per 2D function call, reused across all rows/columns.
type dwtBufs97 struct {
	low, high []float64
}

// ensure grows the internal buffers to accommodate a signal of length n.
func (b *dwtBufs97) ensure(n int) {
	sn := (n + 1) / 2
	dn := n - sn
	if cap(b.low) < sn {
		b.low = make([]float64, sn)
	}
	if cap(b.high) < dn {
		b.high = make([]float64, dn)
	}
}

// synthesize1D_53 performs in-place 1D inverse 5/3 wavelet transform
// Input: [L0, L1, ..., H0, H1, ...] where first half is low-pass, second half is high-pass
// Output: reconstructed signal [even0, odd0, even1, odd1, ...]
func synthesize1D_53(data []int32) {
	wavelet.Synthesize53(data, 0)
}

// synthesize1D_53_cas performs 1D inverse 5/3 transform with parity control
// cas=0: first output sample is at even position (standard case)
// cas=1: first output sample is at odd position (for tiles starting at odd coordinates)
func synthesize1D_53_cas(data []int32, cas int) {
	wavelet.Synthesize53(data, cas)
}

// synthesize1D_53_bufs performs the 5/3 inverse transform using pre-allocated buffers.
func synthesize1D_53_bufs(data []int32, low, high []int32, cas int) {
	wavelet.Synthesize53Bufs(data, cas, low, high)
}

// synthesize1D_97 performs in-place 1D inverse 9/7 wavelet transform
// Input: coefficients [L0, L1, ..., H0, H1, ...] where first half is low-pass, second half is high-pass
// Output: reconstructed signal in same array
func synthesize1D_97(data []float64) {
	synthesize1D_97_cas(data, 0)
}

// synthesize1D_97_cas performs 1D inverse 9/7 transform with parity control
// cas=0: first output sample is at even position (standard case)
// cas=1: first output sample is at odd position (for tiles starting at odd coordinates)
// Uses float64 SIMD primitives for lifting, scaling, and interleaving while
// preserving JPEG2000's non-standard 2/K high-pass scaling (BUG_WEIRD_TWO_INVK).
func synthesize1D_97_cas(data []float64, cas int) {
	n := len(data)
	if n <= 1 {
		return
	}
	sn := (n + 1) / 2
	dn := n / 2
	if cas != 0 {
		sn, dn = dn, sn
	}
	low := make([]float64, sn)
	high := make([]float64, dn)
	doSynthesize97(data, low, high, cas)
}

// synthesize1D_97_bufs performs the 9/7 inverse transform using pre-allocated buffers.
// The caller must ensure bufs has been sized via bufs.ensure(len(data)).
func synthesize1D_97_bufs(data []float64, bufs *dwtBufs97, cas int) {
	n := len(data)
	if n <= 1 {
		return
	}
	sn := (n + 1) / 2
	dn := n / 2
	if cas != 0 {
		sn, dn = dn, sn
	}
	doSynthesize97(data, bufs.low[:sn], bufs.high[:dn], cas)
}

// doSynthesize97 is the core 9/7 inverse transform implementation.
// Operates entirely on float64, using SIMD-accelerated lifting and interleaving.
// low and high are separate buffers (not aliases of data), so Interleave can
// write directly into data without an intermediate out buffer.
func doSynthesize97(data, low, high []float64, cas int) {
	sn := len(low)
	dn := len(high)

	copy(low, data[:sn])
	copy(high, data[sn:sn+dn])

	// Inverse scaling per OpenJPEG's BUG_WEIRD_TWO_INVK approach:
	// Low-pass *= K, High-pass *= 2/K (instead of standard 1/K).
	wavelet.ScaleSlice(low, sn, lift97K)
	wavelet.ScaleSlice(high, dn, lift97TwoInvK)

	// Inverse lifting steps (SIMD-accelerated on float64)
	// For update steps (target=low): phase = 1-cas
	// For predict steps (target=high): phase = cas
	wavelet.LiftStep97(low, sn, high, dn, lift97Delta, 1-cas)
	wavelet.LiftStep97(high, dn, low, sn, lift97Gamma, cas)
	wavelet.LiftStep97(low, sn, high, dn, lift97Beta, 1-cas)
	wavelet.LiftStep97(high, dn, low, sn, lift97Alpha, cas)

	// Interleave directly into data (safe: low/high are separate copies)
	wavelet.Interleave(data, low, sn, high, dn, cas)
}

// Synthesize2D_53 performs full 2D inverse 5/3 wavelet transform
// coeffs: coefficient array organized as [LL, LH; HL, HH] subbands per level
// JPEG2000 standard: forward is V→H (columns then rows), inverse is H→V (rows then columns)
// width, height: dimensions of the full image
// levels: number of decomposition levels
func Synthesize2D_53(coeffs [][]int32, width, height, levels int) {
	if levels < 1 {
		return
	}

	// Pre-allocate reusable buffers for the largest dimension.
	maxDim := max(width, height)
	maxHalf := (maxDim + 1) / 2
	low53 := make([]int32, maxHalf)
	high53 := make([]int32, maxHalf)
	col := make([]int32, height)

	// Process from coarsest to finest level
	for level := levels; level >= 1; level-- {
		levelWidth := (width + (1 << (level - 1)) - 1) >> (level - 1)
		levelHeight := (height + (1 << (level - 1)) - 1) >> (level - 1)

		// Horizontal synthesis first (process rows)
		for y := range levelHeight {
			synthesize1D_53_bufs(coeffs[y][:levelWidth], low53, high53, 0)
		}

		// Vertical synthesis second (process columns)
		for x := range levelWidth {
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}
			synthesize1D_53_bufs(col[:levelHeight], low53, high53, 0)
			for y := range levelHeight {
				coeffs[y][x] = col[y]
			}
		}
	}
}

// ResBounds holds resolution-level bounds for DWT synthesis
// X0, Y0 are the image-space origin (trX0, trY0) used for parity calculation
type ResBounds struct {
	Width, Height int
	X0, Y0        int
}

// Synthesize2D_53_WithDims performs 2D inverse 5/3 wavelet transform
// using explicit resolution dimensions instead of computing them.
// resDims[i] contains bounds for resolution level i (0=coarsest)
// The X0/Y0 values are used to compute DWT parity (cas) for tiles at odd positions.
func Synthesize2D_53_WithDims(coeffs [][]int32, resDims []ResBounds) {
	levels := len(resDims) - 1
	if levels < 1 {
		return
	}

	// Pre-allocate reusable buffers for the largest dimension.
	maxDim := 0
	for _, rd := range resDims {
		if rd.Width > maxDim {
			maxDim = rd.Width
		}
		if rd.Height > maxDim {
			maxDim = rd.Height
		}
	}
	maxHalf := (maxDim + 1) / 2
	low53 := make([]int32, maxHalf)
	high53 := make([]int32, maxHalf)
	col := make([]int32, maxDim)

	// Process from coarsest to finest level
	for level := levels; level >= 1; level-- {
		resIdx := levels - level + 1
		levelWidth := resDims[resIdx].Width
		levelHeight := resDims[resIdx].Height

		casH := resDims[resIdx].X0 % 2
		casV := resDims[resIdx].Y0 % 2

		// Horizontal synthesis first (process rows)
		for y := range levelHeight {
			synthesize1D_53_bufs(coeffs[y][:levelWidth], low53, high53, casH)
		}

		// Vertical synthesis second (process columns)
		for x := range levelWidth {
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}
			synthesize1D_53_bufs(col[:levelHeight], low53, high53, casV)
			for y := range levelHeight {
				coeffs[y][x] = col[y]
			}
		}
	}
}

// Synthesize2D_97 performs full 2D inverse 9/7 wavelet transform
// coeffs: coefficient array organized as [LL, LH; HL, HH] subbands per level
// JPEG2000 standard: forward is V→H (columns then rows), inverse is H→V (rows then columns)
// width, height: dimensions of the full image
// levels: number of decomposition levels
func Synthesize2D_97(coeffs [][]float64, width, height, levels int) {
	if levels < 1 {
		return
	}

	// Pre-allocate reusable buffers sized for the largest level (level=1 = full image).
	var bufs dwtBufs97
	maxDim := max(width, height)
	bufs.ensure(maxDim)
	col := make([]float64, height)

	// Process from coarsest to finest level
	for level := levels; level >= 1; level-- {
		// Compute dimensions at this level
		levelWidth := (width + (1 << (level - 1)) - 1) >> (level - 1)
		levelHeight := (height + (1 << (level - 1)) - 1) >> (level - 1)

		// Horizontal synthesis first (process rows) - matches OpenJPEG inverse order
		// Undoes the last step of forward transform (which did horizontal second)
		for y := range levelHeight {
			synthesize1D_97_bufs(coeffs[y][:levelWidth], &bufs, 0)
		}

		// Vertical synthesis second (process columns)
		// Undoes the first step of forward transform (which did vertical first)
		for x := range levelWidth {
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}
			synthesize1D_97_bufs(col[:levelHeight], &bufs, 0)
			for y := range levelHeight {
				coeffs[y][x] = col[y]
			}
		}
	}
}

// Synthesize2D_97_WithDims performs 2D inverse 9/7 wavelet transform
// using explicit resolution dimensions instead of computing them.
// resDims[i] contains bounds for resolution level i (0=coarsest)
// The X0/Y0 values are used to compute DWT parity (cas) for tiles at odd positions.
func Synthesize2D_97_WithDims(coeffs [][]float64, resDims []ResBounds) {
	levels := len(resDims) - 1
	if levels < 1 {
		return
	}

	// Pre-allocate reusable buffers sized for the largest resolution level.
	maxDim := 0
	for _, rd := range resDims {
		if rd.Width > maxDim {
			maxDim = rd.Width
		}
		if rd.Height > maxDim {
			maxDim = rd.Height
		}
	}
	var bufs dwtBufs97
	bufs.ensure(maxDim)
	col := make([]float64, maxDim)

	for level := levels; level >= 1; level-- {
		resIdx := levels - level + 1
		levelWidth := resDims[resIdx].Width
		levelHeight := resDims[resIdx].Height

		// Compute parity (cas) based on resolution origin
		casH := resDims[resIdx].X0 % 2
		casV := resDims[resIdx].Y0 % 2

		for y := range levelHeight {
			synthesize1D_97_bufs(coeffs[y][:levelWidth], &bufs, casH)
		}

		for x := range levelWidth {
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}
			synthesize1D_97_bufs(col[:levelHeight], &bufs, casV)
			for y := range levelHeight {
				coeffs[y][x] = col[y]
			}
		}
	}
}

// analyze1D_53 performs forward 1D 5/3 wavelet transform (for testing)
// Input: signal [s0, s1, s2, s3, ...]
// Output: [L0, L1, ..., H0, H1, ...] (low-pass first, then high-pass)
func analyze1D_53(data []int32) {
	wavelet.Analyze53(data, 0)
}

// analyze1D_97 performs forward 1D 9/7 wavelet transform (for testing)
// Uses float64 SIMD primitives while preserving JPEG2000's non-standard scaling.
func analyze1D_97(data []float64) {
	n := len(data)
	if n <= 1 {
		return
	}

	half := (n + 1) / 2

	low := make([]float64, half)
	high := make([]float64, n-half)
	wavelet.Deinterleave(data, low, half, high, n-half, 0)

	// Forward lifting steps (SIMD-accelerated on float64)
	// Forward uses += coeff*(neighbors), LiftStep97 does -= coeff*(neighbors),
	// so negate coefficients for forward direction.
	wavelet.LiftStep97(high, n-half, low, half, -lift97Alpha, 0)
	wavelet.LiftStep97(low, half, high, n-half, -lift97Beta, 1)
	wavelet.LiftStep97(high, n-half, low, half, -lift97Gamma, 0)
	wavelet.LiftStep97(low, half, high, n-half, -lift97Delta, 1)

	// JPEG2000 scaling: low /= K, high *= K/2
	wavelet.ScaleSlice(low, half, 1.0/lift97K)
	wavelet.ScaleSlice(high, n-half, lift97K/2.0)

	// Pack [low | high] back
	copy(data[:half], low)
	copy(data[half:], high)
}

// roundTripError computes max absolute error after forward+inverse transform
func roundTripError_53(original []int32) int32 {
	data := make([]int32, len(original))
	copy(data, original)

	analyze1D_53(data)
	synthesize1D_53(data)

	var maxErr int32
	for i := range original {
		err := data[i] - original[i]
		if err < 0 {
			err = -err
		}
		if err > maxErr {
			maxErr = err
		}
	}
	return maxErr
}

// roundTripError_97 computes max absolute error after forward+inverse transform
func roundTripError_97(original []float64) float64 {
	data := make([]float64, len(original))
	copy(data, original)

	analyze1D_97(data)
	synthesize1D_97(data)

	var maxErr float64
	for i := range original {
		err := math.Abs(data[i] - original[i])
		if err > maxErr {
			maxErr = err
		}
	}
	return maxErr
}
