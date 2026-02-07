package jpeg2000

import "math"

// quantize97 performs forward dead-zone quantization for lossy mode.
// Per ITU-T T.800 equation E.1:
//
//	q_b = sign(a_b) * floor(|a_b| / Δ_b)
//
// where Δ_b is the step size for subband b. The "dead zone" means that
// coefficients with magnitude less than Δ_b are quantized to 0, and the
// dead zone width is 2*Δ_b (symmetric around zero).
//
// Returns quantized integer coefficients.
func quantize97(coeffs [][]float64, stepSize float64) [][]int32 {
	height := len(coeffs)
	if height == 0 {
		return nil
	}
	width := len(coeffs[0])

	result := make([][]int32, height)
	for y := range height {
		result[y] = make([]int32, width)
		for x := range width {
			val := coeffs[y][x]
			if val >= 0 {
				result[y][x] = int32(math.Floor(val / stepSize))
			} else {
				result[y][x] = -int32(math.Floor(-val / stepSize))
			}
		}
	}

	return result
}

// dequantize97 performs inverse quantization for lossy mode.
// Per ITU-T T.800 equation E.2, for reconstruction the midpoint
// of the quantization bin is used:
//
//	a_b = sign(q_b) * (|q_b| + 0.5) * Δ_b   (for q_b != 0)
//	a_b = 0                                    (for q_b == 0)
//
// This places the reconstructed value at the center of each quantization
// interval, which minimizes mean squared error. The dead-zone bin [-Δ_b, Δ_b)
// always reconstructs to exactly 0.
func dequantize97(coeffs [][]int32, stepSize float64) [][]float64 {
	height := len(coeffs)
	if height == 0 {
		return nil
	}
	width := len(coeffs[0])

	result := make([][]float64, height)
	for y := range height {
		result[y] = make([]float64, width)
		for x := range width {
			q := coeffs[y][x]
			if q == 0 {
				result[y][x] = 0
			} else if q > 0 {
				result[y][x] = (float64(q) + 0.5) * stepSize
			} else {
				result[y][x] = -(float64(-q) + 0.5) * stepSize
			}
		}
	}

	return result
}

// computeStepSize computes the quantization step size from exponent and mantissa.
// Per ITU-T T.800 equation E.3:
//
//	Δ_b = 2^(R_b - ε_b) × (1 + μ_b / 2^11)
//
// where R_b is the nominal dynamic range (bit depth + subband gain for 5/3,
// or just bit depth for 9/7 with gain=0), ε_b is the exponent, and μ_b is
// the mantissa (0..2047).
func computeStepSize(bitDepth, exponent, mantissa int) float64 {
	return (1.0 + float64(mantissa)/2048.0) * math.Pow(2, float64(bitDepth-exponent))
}

// computeExpMantissa computes the exponent and mantissa for a given step size.
// This is the inverse of computeStepSize: given a desired step size Δ and
// bit depth R_b, it finds the (exponent, mantissa) pair that best represents
// the step size in the QCD/QCC marker format.
//
// From Δ_b = 2^(R_b - ε_b) × (1 + μ_b / 2^11):
//
//	ε_b = R_b - floor(log2(Δ_b))
//	μ_b = round((Δ_b / 2^(R_b - ε_b) - 1) * 2^11)
//
// The exponent is clamped to [0, 31] (5 bits) and the mantissa to [0, 2047]
// (11 bits) per the QCD marker specification.
func computeExpMantissa(stepSize float64, bitDepth int) (int, int) {
	if stepSize <= 0 {
		return bitDepth, 0
	}

	// ε_b = R_b - floor(log2(Δ_b))
	log2Step := math.Floor(math.Log2(stepSize))
	exponent := max(
		// Clamp exponent to 5-bit range [0, 31]
		bitDepth-int(log2Step), 0)
	if exponent > 31 {
		exponent = 31
	}

	// μ_b = round((Δ_b / 2^(R_b - ε_b) - 1) * 2^11)
	normalized := stepSize / math.Pow(2, float64(bitDepth-exponent))
	mantissa := max(
		// Clamp mantissa to 11-bit range [0, 2047]
		int(math.Round((normalized-1.0)*2048.0)), 0)
	if mantissa > 2047 {
		mantissa = 2047
	}

	return exponent, mantissa
}

// defaultStepSizes returns default quantization step sizes for each subband
// at the given number of decomposition levels, based on the desired quality.
//
// quality: 0.0 (lowest quality, highest compression) to 1.0 (highest quality,
// lowest compression). A quality of 1.0 uses very small step sizes, while 0.0
// uses large step sizes that discard most detail.
//
// Returns step sizes indexed by subband order matching the QCD marker:
//
//	[0]=LL_N, [1]=LH_N, [2]=HL_N, [3]=HH_N, [4]=LH_{N-1}, [5]=HL_{N-1}, [6]=HH_{N-1}, ...
//
// where N is the coarsest decomposition level. There are 3*numLevels + 1
// entries total.
//
// The step sizes are derived from the subband's nominal gain and decomposition
// level. Higher decomposition levels (coarser subbands) and subbands with
// higher gain (HH > HL/LH > LL) get larger step sizes, reflecting the
// visual weighting of the wavelet coefficients.
func defaultStepSizes(quality float64, numLevels, bitDepth int) []float64 {
	// Clamp quality to [0, 1]
	if quality < 0 {
		quality = 0
	}
	if quality > 1 {
		quality = 1
	}

	// Base step size: maps quality to a reasonable range.
	// quality=1.0 -> baseStep ~= 0.0001 (near-lossless)
	// quality=0.5 -> baseStep ~= 1.0
	// quality=0.0 -> baseStep ~= 1024 (heavy compression)
	// Using exponential mapping for perceptually uniform quality control.
	maxDynRange := float64(uint(1) << bitDepth)
	baseStep := maxDynRange * math.Pow(10, -4.0*quality)

	// Total subbands: 1 (LL) + 3 per level (LH, HL, HH)
	numSubbands := 3*numLevels + 1
	stepSizes := make([]float64, numSubbands)

	// Subband gains for 9/7 wavelet (ITU-T T.800 Table E.1):
	// For the irreversible 9/7 wavelet, all subbands use gain_b = 0,
	// meaning R_b = bitDepth for all subbands. The step sizes vary
	// by level and orientation based on the energy distribution of
	// the wavelet basis functions.
	//
	// Gain factors per orientation (relative energy weighting):
	//   LL: 1.0 (lowest gain, smallest step)
	//   LH, HL: 2.0 (one high-pass filtering)
	//   HH: 4.0 (two high-pass filterings)
	//
	// Level scaling: each coarser level doubles the effective step size
	// because the subband covers a larger spatial extent.

	// LL subband at coarsest level (index 0)
	// LL has the lowest gain and is at the coarsest level
	llGain := 1.0
	levelScale := math.Pow(2, float64(numLevels-1))
	stepSizes[0] = baseStep * llGain / levelScale

	// Detail subbands from coarsest to finest level
	// Index layout: [LL, LH_N, HL_N, HH_N, LH_{N-1}, HL_{N-1}, HH_{N-1}, ...]
	idx := 1
	for level := numLevels; level >= 1; level-- {
		// Level scale: coarser levels have smaller coefficients relative
		// to image dynamic range, so they need proportionally smaller step sizes
		// to preserve the same relative precision.
		levelFactor := math.Pow(2, float64(level-1))

		// LH subband (vertical high-pass, horizontal low-pass)
		stepSizes[idx] = baseStep * 2.0 / levelFactor
		idx++

		// HL subband (vertical low-pass, horizontal high-pass)
		stepSizes[idx] = baseStep * 2.0 / levelFactor
		idx++

		// HH subband (both high-pass)
		stepSizes[idx] = baseStep * 4.0 / levelFactor
		idx++
	}

	return stepSizes
}
