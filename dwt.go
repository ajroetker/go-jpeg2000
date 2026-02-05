package jpeg2000

import (
	"math"
)

// WaveletType indicates the wavelet filter used
type WaveletType int

const (
	Wavelet53 WaveletType = iota // 5/3 reversible (lossless)
	Wavelet97                    // 9/7 irreversible (lossy)
)

// Lifting coefficients for 5/3 reversible filter
const (
	lift53Alpha = -0.5 // Prediction step
	lift53Beta  = 0.25 // Update step
)

// Lifting coefficients for 9/7 irreversible filter (ITU-T T.800 Table F.4)
const (
	lift97Alpha = -1.586134342059924
	lift97Beta  = -0.052980118572961
	lift97Gamma = 0.882911075530934
	lift97Delta = 0.443506852043971
	lift97K     = 1.230174104914001 // Normalization constant K
)

// float32 lifting coefficients to match OpenJPEG's single-precision arithmetic
const (
	lift97Alpha32 float32 = -1.586134342059924
	lift97Beta32  float32 = -0.052980118572961
	lift97Gamma32 float32 = 0.882911075530934
	lift97Delta32 float32 = 0.443506852043971
	lift97K32     float32 = 1.230174104914001
	// BUG_WEIRD_TWO_INVK: OpenJPEG uses 2/K instead of 1/K for high-pass scaling,
	// compensated by using gain=0 for all subbands in 9/7 decode (see tcd.c).
	// This avoids numerical issues when n=1 DWT levels cause no-op pass-throughs
	// that would leave standard gain factors uncompensated.
	lift97TwoInvK32 float32 = 1.625732422
)

// synthesize1D_53 performs in-place 1D inverse 5/3 wavelet transform
// Input: [L0, L1, ..., H0, H1, ...] where first half is low-pass, second half is high-pass
// Output: reconstructed signal [even0, odd0, even1, odd1, ...]
func synthesize1D_53(data []int32) {
	synthesize1D_53_cas(data, 0)
}

// synthesize1D_53_cas performs 1D inverse 5/3 transform with parity control
// cas=0: first output sample is at even position (standard case)
// cas=1: first output sample is at odd position (for tiles starting at odd coordinates)
func synthesize1D_53_cas(data []int32, cas int) {
	n := len(data)
	if n <= 1 {
		if n == 1 && cas == 1 {
			data[0] /= 2
		}
		return
	}

	// Number of low and high frequency samples depends on parity
	// cas=0: low has ceil(n/2), high has floor(n/2)
	// cas=1: high has ceil(n/2), low has floor(n/2)
	var sn, dn int // sn = number of low-pass, dn = number of high-pass
	if cas == 0 {
		sn = (n + 1) / 2
		dn = n / 2
	} else {
		dn = (n + 1) / 2
		sn = n / 2
	}

	low := make([]int32, sn)
	high := make([]int32, dn)

	// Extract low and high bands from input
	// Layout in memory: [low0, low1, ..., high0, high1, ...]
	copy(low, data[:sn])
	copy(high, data[sn:])

	if cas == 0 {
		// Even case: standard lifting
		// Inverse lifting step 1: undo update
		for i := 0; i < sn; i++ {
			var h1, h2 int32
			if i > 0 {
				h1 = high[i-1]
			} else {
				h1 = high[0]
			}
			if i < dn {
				h2 = high[i]
			} else {
				h2 = high[dn-1]
			}
			low[i] -= (h1 + h2 + 2) >> 2
		}
		// Inverse lifting step 2: undo predict
		for i := 0; i < dn; i++ {
			var e1, e2 int32
			e1 = low[i]
			if i+1 < sn {
				e2 = low[i+1]
			} else {
				e2 = low[sn-1]
			}
			high[i] += (e1 + e2) >> 1
		}
		// Interleave: even=low, odd=high
		for i := 0; i < dn; i++ {
			data[2*i] = low[i]
			data[2*i+1] = high[i]
		}
		if sn > dn {
			data[n-1] = low[sn-1]
		}
	} else {
		// Odd case (cas=1): first output sample is at odd global position
		//
		// For cas=1, the interleaved output has high-pass at even positions,
		// low-pass at odd positions. In OpenJPEG:
		//   OPJ_S(i) = a[2*i]   = even positions = high-pass for cas=1
		//   OPJ_D(i) = a[1+2*i] = odd positions = low-pass for cas=1
		//
		// OpenJPEG's STANDARD_SLOW cas=1 lifting steps:
		//   Step 1: D[i] -= (SS_(i) + SS_(i+1) + 2) >> 2   for i=0..sn-1
		//   Step 2: S[i] += (DD_(i) + DD_(i-1)) >> 1        for i=0..dn-1
		//
		// Since D=low-pass and S=high-pass for cas=1:
		//   Step 1: low[i]  -= (high[i] + high[i+1] + 2) >> 2   for i=0..sn-1
		//   Step 2: high[i] += (low[i] + low[i-1]) >> 1          for i=0..dn-1
		//
		// Boundary: SS_ clamps to [0, dn-1] = high array bounds
		//           DD_ clamps to [0, sn-1] = low array bounds

		// Edge case: single high-pass sample with no low-pass (OpenJPEG compatibility)
		if sn == 0 && dn == 1 {
			data[0] /= 2
			return
		}

		// getHigh accesses high-pass array with boundary clamping to [0, dn-1]
		getHigh := func(i int) int32 {
			if i < 0 {
				return high[0]
			}
			if i >= dn {
				return high[dn-1]
			}
			return high[i]
		}

		// getLow accesses low-pass array with boundary clamping to [0, sn-1]
		getLow := func(i int) int32 {
			if i < 0 {
				return low[0]
			}
			if i >= sn {
				return low[sn-1]
			}
			return low[i]
		}

		// Inverse lifting step 1: low[i] -= (high[i] + high[i+1] + 2) >> 2
		for i := 0; i < sn; i++ {
			h1 := getHigh(i)
			h2 := getHigh(i + 1)
			low[i] -= (h1 + h2 + 2) >> 2
		}
		// Inverse lifting step 2: high[i] += (low[i] + low[i-1]) >> 1
		for i := 0; i < dn; i++ {
			l1 := getLow(i)
			l2 := getLow(i - 1)
			high[i] += (l1 + l2) >> 1
		}
		// Interleave: even=high, odd=low (for cas=1)
		for i := 0; i < dn; i++ {
			data[2*i] = high[i]
		}
		for i := 0; i < sn; i++ {
			data[2*i+1] = low[i]
		}
	}
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
// Internally uses float32 arithmetic to match OpenJPEG's single-precision behavior.
func synthesize1D_97_cas(data []float64, cas int) {
	n := len(data)
	if n <= 1 {
		// Per OpenJPEG opj_v8dwt_decode: for n<=1, no scaling is applied.
		// cas=0 (sn=1,dn=0): !((dn>0)||(sn>1)) → return
		// cas=1 (sn=0,dn=1): !((sn>0)||(dn>1)) → return
		// The coefficient passes through unchanged. This is consistent with
		// the encoder's forward transform which also doesn't scale n=1 cases.
		return
	}

	// Number of low and high frequency samples depends on parity
	// cas=0: low has ceil(n/2), high has floor(n/2)
	// cas=1: high has ceil(n/2), low has floor(n/2)
	var sn, dn int // sn = number of low-pass, dn = number of high-pass
	if cas == 0 {
		sn = (n + 1) / 2
		dn = n / 2
	} else {
		dn = (n + 1) / 2
		sn = n / 2
	}

	// Use float32 arrays internally to match OpenJPEG's single-precision arithmetic
	low := make([]float32, sn)
	high := make([]float32, dn)

	// Extract low and high bands from input, converting to float32
	for i := 0; i < sn; i++ {
		low[i] = float32(data[i])
	}
	for i := 0; i < dn; i++ {
		high[i] = float32(data[sn+i])
	}

	// Inverse scaling per OpenJPEG's BUG_WEIRD_TWO_INVK approach:
	// Low-pass *= K, High-pass *= 2/K (instead of standard 1/K).
	// This requires using gain=0 for all subbands in dequantization.
	// The 2/K approach avoids errors when n=1 DWT levels are no-ops,
	// which would leave standard gain factors (0/1/2) uncompensated.
	for i := range low {
		low[i] *= lift97K32
	}
	for i := range high {
		high[i] *= lift97TwoInvK32
	}

	// Inverse lifting steps per ITU-T T.800 Annex F (CDF 9/7 wavelet)
	// cas=0: standard case, cas=1: high-pass comes first in output
	// Boundary conditions change based on cas

	if cas == 0 {
		// cas=0: standard lifting (low=even, high=odd)
		// Inverse step 4: S[k] -= δ*(D[k-1] + D[k])
		for i := 0; i < sn; i++ {
			var h1, h2 float32
			if i > 0 {
				h1 = high[i-1]
			} else {
				h1 = high[0]
			}
			if i < dn {
				h2 = high[i]
			} else {
				h2 = high[dn-1]
			}
			low[i] -= lift97Delta32 * (h1 + h2)
		}
		// Inverse step 3: D[k] -= γ*(S[k] + S[k+1])
		for i := 0; i < dn; i++ {
			e1 := low[i]
			var e2 float32
			if i+1 < sn {
				e2 = low[i+1]
			} else {
				e2 = low[sn-1]
			}
			high[i] -= lift97Gamma32 * (e1 + e2)
		}
		// Inverse step 2: S[k] -= β*(D[k-1] + D[k])
		for i := 0; i < sn; i++ {
			var h1, h2 float32
			if i > 0 {
				h1 = high[i-1]
			} else {
				h1 = high[0]
			}
			if i < dn {
				h2 = high[i]
			} else {
				h2 = high[dn-1]
			}
			low[i] -= lift97Beta32 * (h1 + h2)
		}
		// Inverse step 1: D[k] -= α*(S[k] + S[k+1])
		for i := 0; i < dn; i++ {
			e1 := low[i]
			var e2 float32
			if i+1 < sn {
				e2 = low[i+1]
			} else {
				e2 = low[sn-1]
			}
			high[i] -= lift97Alpha32 * (e1 + e2)
		}
		// Interleave: even=low, odd=high (convert back to float64)
		for i := 0; i < dn; i++ {
			data[2*i] = float64(low[i])
			data[2*i+1] = float64(high[i])
		}
		if sn > dn {
			data[n-1] = float64(low[sn-1])
		}
	} else {
		// cas=1: high-pass comes first in output
		//
		// For cas=1, interleaved output: even=high-pass, odd=low-pass
		// In OpenJPEG: S(i)=a[2*i]=even=high, D(i)=a[1+2*i]=odd=low
		//
		// OpenJPEG STANDARD_SLOW cas=1 lifting:
		//   Step 4: D[k] -= δ*(SS_(k) + SS_(k+1))    for k=0..sn-1
		//   Step 3: S[k] -= γ*(DD_(k-1) + DD_(k))    for k=0..dn-1
		//   Step 2: D[k] -= β*(SS_(k) + SS_(k+1))    for k=0..sn-1
		//   Step 1: S[k] -= α*(DD_(k-1) + DD_(k))    for k=0..dn-1
		//
		// Since D=low and S=high for cas=1:
		//   Step 4: low[k]  -= δ*(high[k] + high[k+1])   for k=0..sn-1
		//   Step 3: high[k] -= γ*(low[k-1] + low[k])     for k=0..dn-1
		//   Step 2: low[k]  -= β*(high[k] + high[k+1])   for k=0..sn-1
		//   Step 1: high[k] -= α*(low[k-1] + low[k])     for k=0..dn-1
		//
		// Boundary: SS_ clamps to [0, dn-1] = high array bounds
		//           DD_ clamps to [0, sn-1] = low array bounds

		// getHigh accesses high-pass array with boundary clamping to [0, dn-1]
		getHigh := func(i int) float32 {
			if i < 0 {
				return high[0]
			}
			if i >= dn {
				return high[dn-1]
			}
			return high[i]
		}

		// getLow accesses low-pass array with boundary clamping to [0, sn-1]
		getLow := func(i int) float32 {
			if i < 0 {
				return low[0]
			}
			if i >= sn {
				return low[sn-1]
			}
			return low[i]
		}

		// Inverse step 4: low[k] -= δ*(high[k] + high[k+1])
		for i := 0; i < sn; i++ {
			h1 := getHigh(i)
			h2 := getHigh(i + 1)
			low[i] -= lift97Delta32 * (h1 + h2)
		}
		// Inverse step 3: high[k] -= γ*(low[k-1] + low[k])
		for i := 0; i < dn; i++ {
			l1 := getLow(i - 1)
			l2 := getLow(i)
			high[i] -= lift97Gamma32 * (l1 + l2)
		}
		// Inverse step 2: low[k] -= β*(high[k] + high[k+1])
		for i := 0; i < sn; i++ {
			h1 := getHigh(i)
			h2 := getHigh(i + 1)
			low[i] -= lift97Beta32 * (h1 + h2)
		}
		// Inverse step 1: high[k] -= α*(low[k-1] + low[k])
		for i := 0; i < dn; i++ {
			l1 := getLow(i - 1)
			l2 := getLow(i)
			high[i] -= lift97Alpha32 * (l1 + l2)
		}
		// Interleave: even=high, odd=low (for cas=1, convert back to float64)
		for i := 0; i < dn; i++ {
			data[2*i] = float64(high[i])
		}
		for i := 0; i < sn; i++ {
			data[2*i+1] = float64(low[i])
		}
	}
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

	// Process from coarsest to finest level
	for level := levels; level >= 1; level-- {
		// Compute dimensions at this level
		// At level L, dimensions are ceil(size / 2^(L-1))
		// This is the size of the LL band after level L-1 synthesis
		// For level=1, this is the full image size
		// For level=levels, this is ceil(size / 2^(levels-1)) which includes LL + detail subbands
		levelWidth := (width + (1 << (level - 1)) - 1) >> (level - 1)
		levelHeight := (height + (1 << (level - 1)) - 1) >> (level - 1)

		// Horizontal synthesis first (process rows) - matches OpenJPEG inverse order
		// Undoes the last step of forward transform (which did horizontal second)
		for y := range levelHeight {
			// Synthesize row in-place
			synthesize1D_53(coeffs[y][:levelWidth])
		}

		// Vertical synthesis second (process columns)
		// Undoes the first step of forward transform (which did vertical first)
		for x := range levelWidth {
			// Extract column
			col := make([]int32, levelHeight)
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}

			// Synthesize
			synthesize1D_53(col)

			// Write back
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

	// Process from coarsest to finest level
	// level=levels is the coarsest (res=0), level=1 is finest (res=levels)
	for level := levels; level >= 1; level-- {
		// Use the resolution dimensions for this level
		// resDims[levels-level+1] corresponds to the level we're synthesizing TO
		resIdx := levels - level + 1
		levelWidth := resDims[resIdx].Width
		levelHeight := resDims[resIdx].Height

		// Compute parity (cas) based on resolution origin
		// Per OpenJPEG: cas = tr->x0 % 2
		// When cas=1, the first coefficient is high-pass, not low-pass
		casH := resDims[resIdx].X0 % 2 // horizontal parity
		casV := resDims[resIdx].Y0 % 2 // vertical parity

		// Horizontal synthesis first (process rows)
		for y := range levelHeight {
			synthesize1D_53_cas(coeffs[y][:levelWidth], casH)
		}

		// Vertical synthesis second (process columns)
		for x := range levelWidth {
			col := make([]int32, levelHeight)
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}
			synthesize1D_53_cas(col, casV)
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

	// Process from coarsest to finest level
	for level := levels; level >= 1; level-- {
		// Compute dimensions at this level
		levelWidth := (width + (1 << (level - 1)) - 1) >> (level - 1)
		levelHeight := (height + (1 << (level - 1)) - 1) >> (level - 1)

		// Horizontal synthesis first (process rows) - matches OpenJPEG inverse order
		// Undoes the last step of forward transform (which did horizontal second)
		for y := range levelHeight {
			// Synthesize row in-place
			synthesize1D_97(coeffs[y][:levelWidth])
		}

		// Vertical synthesis second (process columns)
		// Undoes the first step of forward transform (which did vertical first)
		for x := range levelWidth {
			// Extract column
			col := make([]float64, levelHeight)
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}
			// Synthesize
			synthesize1D_97(col)
			// Write back
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

	for level := levels; level >= 1; level-- {
		resIdx := levels - level + 1
		levelWidth := resDims[resIdx].Width
		levelHeight := resDims[resIdx].Height

		// Compute parity (cas) based on resolution origin
		casH := resDims[resIdx].X0 % 2
		casV := resDims[resIdx].Y0 % 2

		for y := range levelHeight {
			synthesize1D_97_cas(coeffs[y][:levelWidth], casH)
		}

		for x := range levelWidth {
			col := make([]float64, levelHeight)
			for y := range levelHeight {
				col[y] = coeffs[y][x]
			}
			synthesize1D_97_cas(col, casV)
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
	n := len(data)
	if n <= 1 {
		return
	}

	temp := make([]int32, n)
	copy(temp, data)

	half := (n + 1) / 2
	high := make([]int32, n-half)

	// Step 1: Predict - compute high-pass coefficients
	// high[i] = odd[i] - floor((even[i] + even[i+1]) / 2)
	// odd[i] = temp[2*i+1], even[i] = temp[2*i], even[i+1] = temp[2*i+2]
	for i := 0; i < n/2; i++ {
		var e1, e2 int32
		e1 = temp[2*i] // even[i]
		if 2*i+2 < n {
			e2 = temp[2*i+2] // even[i+1]
		} else {
			// Symmetric extension: reflect around the last even sample
			// For n even: last even is temp[n-2], so temp[n] mirrors to temp[n-2]
			// For n odd: last even is temp[n-1], so this branch isn't taken
			e2 = temp[2*i] // Use same even sample (whole-sample symmetric)
		}
		high[i] = temp[2*i+1] - (e1+e2)>>1
	}

	// Step 2: Update - compute low-pass coefficients
	// low[i] = even[i] + floor((high[i-1] + high[i] + 2) / 4)
	for i := range half {
		var h1, h2 int32
		// h1 is high[i-1]
		if i > 0 {
			h1 = high[i-1]
		} else {
			h1 = high[0] // symmetric extension
		}
		// h2 is high[i]
		if i < len(high) {
			h2 = high[i]
		} else {
			h2 = high[len(high)-1] // symmetric extension
		}
		data[i] = temp[2*i] + (h1+h2+2)>>2
	}

	// Copy high-pass coefficients to second half
	copy(data[half:], high)
}

// analyze1D_97 performs forward 1D 9/7 wavelet transform (for testing)
func analyze1D_97(data []float64) {
	n := len(data)
	if n <= 1 {
		// Per OpenJPEG: no scaling for single element (consistent with inverse)
		return
	}

	temp := make([]float64, n)
	copy(temp, data)

	half := (n + 1) / 2
	low := make([]float64, half)
	high := make([]float64, n-half)

	// Copy even samples to low, odd to high
	for i := range half {
		low[i] = temp[2*i]
	}
	for i := 0; i < n/2; i++ {
		high[i] = temp[2*i+1]
	}

	// Forward lifting steps per ITU-T T.800 Annex F (CDF 9/7 wavelet)
	// The forward transform alternates between updating D (odd/high) and S (even/low):
	//   Step 1: D[k] += α*(S[k] + S[k+1])     (D uses S neighbors [k, k+1])
	//   Step 2: S[k] += β*(D[k-1] + D[k])     (S uses D neighbors [k-1, k])
	//   Step 3: D[k] += γ*(S[k] + S[k+1])     (D uses S neighbors [k, k+1])
	//   Step 4: S[k] += δ*(D[k-1] + D[k])     (S uses D neighbors [k-1, k])
	//   Then scale: S /= K, D *= K
	// where low=S=even samples, high=D=odd samples

	// Step 1 (alpha): D[k] += α*(S[k] + S[k+1])
	// high uses low neighbors at [i, i+1]
	for i := range high {
		curr := low[i]
		var next float64
		if i+1 < len(low) {
			next = low[i+1]
		} else {
			next = low[len(low)-1]
		}
		high[i] += lift97Alpha * (curr + next)
	}

	// Step 2 (beta): S[k] += β*(D[k-1] + D[k])
	// low uses high neighbors at [i-1, i]
	for i := range low {
		var prev, curr float64
		if i > 0 {
			prev = high[i-1]
		} else {
			prev = high[0]
		}
		if i < len(high) {
			curr = high[i]
		} else {
			curr = high[len(high)-1]
		}
		low[i] += lift97Beta * (prev + curr)
	}

	// Step 3 (gamma): D[k] += γ*(S[k] + S[k+1])
	// high uses low neighbors at [i, i+1]
	for i := range high {
		curr := low[i]
		var next float64
		if i+1 < len(low) {
			next = low[i+1]
		} else {
			next = low[len(low)-1]
		}
		high[i] += lift97Gamma * (curr + next)
	}

	// Step 4 (delta): S[k] += δ*(D[k-1] + D[k])
	// low uses high neighbors at [i-1, i]
	for i := range low {
		var prev, curr float64
		if i > 0 {
			prev = high[i-1]
		} else {
			prev = high[0]
		}
		if i < len(high) {
			curr = high[i]
		} else {
			curr = high[len(high)-1]
		}
		low[i] += lift97Delta * (prev + curr)
	}

	// Scaling: even /= K, odd *= K/2 (matches inverse's 2/K high-pass scaling)
	for i := range low {
		low[i] /= lift97K
	}
	for i := range high {
		high[i] *= lift97K / 2.0
	}

	// Write back: [L0, L1, ..., H0, H1, ...]
	for i := range half {
		data[i] = low[i]
	}
	for i := 0; i < n-half; i++ {
		data[half+i] = high[i]
	}
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
