package jpeg2000

// PCRD-opt (Post-Compression Rate-Distortion optimization) for JPEG2000
//
// This implements the rate control algorithm described in ITU-T T.800 Annex J.
// After EBCOT Tier-1 encoding produces per-pass data for each code block,
// the rate controller decides how to optimally truncate these passes across
// all code blocks to achieve a target bitrate or quality level.
//
// The core idea: each coding pass has a rate-distortion slope (ΔD/ΔR),
// measuring how much distortion is reduced per byte spent. By including
// passes in decreasing slope order, we get the best quality for any given
// byte budget. A bisection search on the slope threshold (lambda) efficiently
// finds the optimal truncation point for a target byte count.

import (
	"math"
	"sort"
)

// TruncationPoint represents a truncation point for a code block pass.
type TruncationPoint struct {
	BlockIndex int     // Index into the blocks array
	PassIndex  int     // Number of passes to include (1-based: pass 0 means include first pass)
	Bytes      int     // Cumulative bytes up to this pass
	Distortion float64 // Cumulative distortion reduction up to this pass
	Slope      float64 // R-D slope = ΔD/ΔR (distortion reduction per byte)
}

// LayerAllocation specifies which passes of each block go into a quality layer.
type LayerAllocation struct {
	// NumPasses[i] = number of passes for block i in this layer
	NumPasses []int
}

// RateController performs PCRD-opt rate-distortion optimization.
type RateController struct {
	blocks []*EncodedBlock
}

// NewRateController creates a rate controller for the given encoded blocks.
func NewRateController(blocks []*EncodedBlock) *RateController {
	return &RateController{
		blocks: blocks,
	}
}

// OptimizeSingleLayer finds optimal truncation for a single quality layer
// to achieve the target byte count.
// Returns the number of passes to include for each block.
func (rc *RateController) OptimizeSingleLayer(targetBytes int) []int {
	numBlocks := len(rc.blocks)
	result := make([]int, numBlocks)

	if targetBytes <= 0 || numBlocks == 0 {
		return result
	}

	// Check if distortion values are available. If all are zero,
	// fall back to uniform truncation.
	if !rc.hasDistortionInfo() {
		return rc.uniformTruncation(targetBytes)
	}

	// Compute R-D slopes for all truncation points.
	slopes := rc.computeRDSlopes()
	if len(slopes) == 0 {
		return result
	}

	// Check total bytes if we include everything.
	totalAvailable := 0
	for _, blk := range rc.blocks {
		for _, p := range blk.Passes {
			totalAvailable += p.Length
		}
	}
	if targetBytes >= totalAvailable {
		// Include all passes
		for i, blk := range rc.blocks {
			result[i] = len(blk.Passes)
		}
		return result
	}

	// Binary search on the slope threshold (lambda).
	// For a given lambda, include all passes with slope >= lambda.
	// Find lambda such that total bytes is closest to targetBytes without exceeding it.
	lambda := rc.bisectLambda(slopes, targetBytes)

	// Apply the threshold: include passes with slope >= lambda
	result = rc.applyThreshold(lambda, slopes)

	return result
}

// OptimizeLayers performs multi-layer PCRD-opt.
// targetBytesPerLayer[i] = cumulative target bytes for layer i.
// Returns LayerAllocation for each layer.
func (rc *RateController) OptimizeLayers(targetBytesPerLayer []int) []LayerAllocation {
	numLayers := len(targetBytesPerLayer)
	numBlocks := len(rc.blocks)

	if numLayers == 0 || numBlocks == 0 {
		return nil
	}

	allocations := make([]LayerAllocation, numLayers)

	// Track how many passes have been allocated so far per block
	// across all previous layers.
	allocatedPasses := make([]int, numBlocks)

	for layer := range numLayers {
		allocations[layer] = LayerAllocation{
			NumPasses: make([]int, numBlocks),
		}

		target := targetBytesPerLayer[layer]
		if target <= 0 {
			continue
		}

		// Compute how many bytes have already been allocated in previous layers.
		totalAllocatedBytes := 0
		for i, blk := range rc.blocks {
			for p := 0; p < allocatedPasses[i] && p < len(blk.Passes); p++ {
				totalAllocatedBytes += blk.Passes[p].Length
			}
		}

		// The remaining budget for this layer's cumulative target.
		remainingTarget := target - totalAllocatedBytes
		if remainingTarget <= 0 {
			// Already exceeded this layer's budget with previous layers.
			// Each block keeps its previous allocation (0 new passes).
			continue
		}

		// Build truncation points from only the remaining (unallocated) passes.
		remainingSlopes := rc.computeRemainingSlopes(allocatedPasses)

		if len(remainingSlopes) == 0 {
			continue
		}

		// Check if remaining budget covers all remaining passes.
		totalRemaining := 0
		for _, tp := range remainingSlopes {
			totalRemaining += tp.Bytes
		}

		if remainingTarget >= totalRemaining {
			// Include all remaining passes
			for i, blk := range rc.blocks {
				allocations[layer].NumPasses[i] = len(blk.Passes) - allocatedPasses[i]
				allocatedPasses[i] = len(blk.Passes)
			}
			continue
		}

		if !rc.hasDistortionInfo() {
			// Uniform truncation for remaining passes
			layerPasses := rc.uniformTruncationRemaining(remainingTarget, allocatedPasses)
			for i := range layerPasses {
				allocations[layer].NumPasses[i] = layerPasses[i]
				allocatedPasses[i] += layerPasses[i]
			}
			continue
		}

		// Bisect on lambda for this layer's remaining passes.
		lambda := rc.bisectLambda(remainingSlopes, remainingTarget)

		// Apply threshold to get per-block pass counts for this layer.
		totalPasses := rc.applyThreshold(lambda, remainingSlopes)

		for i := range totalPasses {
			// totalPasses[i] is the number of remaining passes to include for block i.
			newPasses := totalPasses[i]
			allocations[layer].NumPasses[i] = newPasses
			allocatedPasses[i] += newPasses
		}
	}

	return allocations
}

// computeRDSlopes computes the rate-distortion slope for each truncation point
// across all blocks. The slope is ΔD/ΔR (change in distortion per change in bytes).
func (rc *RateController) computeRDSlopes() []TruncationPoint {
	var points []TruncationPoint

	for bi, blk := range rc.blocks {
		cumBytes := 0
		cumDist := 0.0

		for pi, pass := range blk.Passes {
			if pass.Length == 0 {
				// Skip zero-length passes; they contribute no bytes.
				continue
			}

			deltaR := pass.Length
			deltaD := pass.Distortion

			cumBytes += deltaR
			cumDist += deltaD

			slope := 0.0
			if deltaR > 0 {
				slope = deltaD / float64(deltaR)
			}

			points = append(points, TruncationPoint{
				BlockIndex: bi,
				PassIndex:  pi + 1, // 1-based: "include pi+1 passes"
				Bytes:      deltaR,
				Distortion: cumDist,
				Slope:      slope,
			})
		}
	}

	return points
}

// computeRemainingSlopes computes R-D slopes for passes not yet allocated.
// allocatedPasses[i] = number of passes already allocated for block i.
func (rc *RateController) computeRemainingSlopes(allocatedPasses []int) []TruncationPoint {
	var points []TruncationPoint

	for bi, blk := range rc.blocks {
		startPass := allocatedPasses[bi]

		for pi := startPass; pi < len(blk.Passes); pi++ {
			pass := blk.Passes[pi]
			if pass.Length == 0 {
				continue
			}

			deltaR := pass.Length
			deltaD := pass.Distortion

			slope := 0.0
			if deltaR > 0 {
				slope = deltaD / float64(deltaR)
			}

			points = append(points, TruncationPoint{
				BlockIndex: bi,
				PassIndex:  pi + 1, // absolute pass index (1-based)
				Bytes:      deltaR,
				Distortion: deltaD,
				Slope:      slope,
			})
		}
	}

	return points
}

// bisectLambda performs binary search on the slope threshold to find the
// lambda value that yields total bytes closest to targetBytes without
// exceeding it.
func (rc *RateController) bisectLambda(slopes []TruncationPoint, targetBytes int) float64 {
	if len(slopes) == 0 {
		return math.MaxFloat64
	}

	// Find the range of slopes.
	minSlope := math.MaxFloat64
	maxSlope := -math.MaxFloat64
	for _, tp := range slopes {
		if tp.Slope < minSlope {
			minSlope = tp.Slope
		}
		if tp.Slope > maxSlope {
			maxSlope = tp.Slope
		}
	}

	// Edge case: all slopes are the same.
	if maxSlope-minSlope < 1e-15 {
		// Check if including everything fits.
		total := 0
		for _, tp := range slopes {
			total += tp.Bytes
		}
		if total <= targetBytes {
			return minSlope
		}
		// Cannot fit anything meaningfully; return a very high threshold.
		return maxSlope + 1.0
	}

	// Binary search: find the largest lambda such that
	// sum of bytes for passes with slope >= lambda <= targetBytes.
	lo := minSlope
	hi := maxSlope

	// Perform enough iterations for convergence (50 iterations gives
	// precision well beyond what floating-point slopes require).
	for range 50 {
		mid := (lo + hi) / 2.0

		totalBytes := 0
		for _, tp := range slopes {
			if tp.Slope >= mid {
				totalBytes += tp.Bytes
			}
		}

		if totalBytes <= targetBytes {
			// We can afford this threshold or lower; try lower to include more.
			hi = mid
		} else {
			// Too many bytes; raise the threshold.
			lo = mid
		}
	}

	// Use the upper bound (hi) as the final lambda to ensure we do not
	// exceed the target. Then verify and adjust.
	lambda := hi

	// Final check: ensure we don't exceed target.
	totalBytes := 0
	for _, tp := range slopes {
		if tp.Slope >= lambda {
			totalBytes += tp.Bytes
		}
	}

	if totalBytes > targetBytes {
		// Nudge lambda up slightly. This can happen due to floating point.
		// Sort slopes descending and find the exact cutoff.
		sorted := make([]TruncationPoint, len(slopes))
		copy(sorted, slopes)
		sort.Slice(sorted, func(i, j int) bool {
			return sorted[i].Slope > sorted[j].Slope
		})

		cumBytes := 0
		for _, tp := range sorted {
			if cumBytes+tp.Bytes > targetBytes {
				// The threshold should be just above this slope.
				lambda = tp.Slope + 1e-15
				break
			}
			cumBytes += tp.Bytes
		}
	}

	return lambda
}

// applyThreshold applies a slope threshold and returns the number of
// passes to include for each block. For each block, includes the maximal
// prefix of passes such that every included pass has slope >= lambda.
//
// The key constraint: passes must form a prefix (you cannot skip an earlier
// pass and include a later one from the same block). We include passes
// sequentially until we encounter one below the threshold.
func (rc *RateController) applyThreshold(lambda float64, slopes []TruncationPoint) []int {
	numBlocks := len(rc.blocks)
	result := make([]int, numBlocks)

	// Build a lookup: for each block, determine which passes meet the threshold.
	// Since passes must be included as a prefix, find the longest prefix where
	// all passes have slope >= lambda.
	//
	// First, index slopes by block.
	type passSlope struct {
		absPassIndex int // 1-based absolute pass index
		slope        float64
	}
	blockSlopes := make(map[int][]passSlope)
	for _, tp := range slopes {
		blockSlopes[tp.BlockIndex] = append(blockSlopes[tp.BlockIndex], passSlope{
			absPassIndex: tp.PassIndex,
			slope:        tp.Slope,
		})
	}

	for bi := range numBlocks {
		passes, ok := blockSlopes[bi]
		if !ok {
			continue
		}

		// Sort by absolute pass index to ensure prefix ordering.
		sort.Slice(passes, func(i, j int) bool {
			return passes[i].absPassIndex < passes[j].absPassIndex
		})

		// Include the longest prefix of passes with slope >= lambda.
		count := 0
		for _, ps := range passes {
			if ps.slope >= lambda {
				count = ps.absPassIndex
			} else {
				break
			}
		}
		result[bi] = count
	}

	return result
}

// hasDistortionInfo checks whether any block has non-zero distortion values.
// If distortion was not computed during encoding, we fall back to uniform truncation.
func (rc *RateController) hasDistortionInfo() bool {
	for _, blk := range rc.blocks {
		for _, pass := range blk.Passes {
			if pass.Distortion != 0 {
				return true
			}
		}
	}
	return false
}

// uniformTruncation distributes the byte budget proportionally across all
// blocks when distortion information is unavailable. Each block gets a
// share of the budget proportional to its total encoded size.
func (rc *RateController) uniformTruncation(targetBytes int) []int {
	numBlocks := len(rc.blocks)
	result := make([]int, numBlocks)

	// Compute total bytes across all blocks.
	totalBytes := 0
	blockTotals := make([]int, numBlocks)
	for i, blk := range rc.blocks {
		for _, pass := range blk.Passes {
			blockTotals[i] += pass.Length
		}
		totalBytes += blockTotals[i]
	}

	if totalBytes == 0 {
		return result
	}

	if targetBytes >= totalBytes {
		for i, blk := range rc.blocks {
			result[i] = len(blk.Passes)
		}
		return result
	}

	// Each block gets a proportional share.
	for i, blk := range rc.blocks {
		if blockTotals[i] == 0 {
			continue
		}

		blockBudget := int(float64(targetBytes) * float64(blockTotals[i]) / float64(totalBytes))

		// Find the maximum number of passes that fit in the budget.
		cumBytes := 0
		for pi, pass := range blk.Passes {
			cumBytes += pass.Length
			if cumBytes > blockBudget {
				break
			}
			result[i] = pi + 1
		}
	}

	return result
}

// uniformTruncationRemaining distributes the byte budget proportionally
// across remaining (unallocated) passes when distortion information is unavailable.
func (rc *RateController) uniformTruncationRemaining(targetBytes int, allocatedPasses []int) []int {
	numBlocks := len(rc.blocks)
	result := make([]int, numBlocks)

	// Compute total remaining bytes across all blocks.
	totalRemaining := 0
	blockRemaining := make([]int, numBlocks)
	for i, blk := range rc.blocks {
		for pi := allocatedPasses[i]; pi < len(blk.Passes); pi++ {
			blockRemaining[i] += blk.Passes[pi].Length
		}
		totalRemaining += blockRemaining[i]
	}

	if totalRemaining == 0 {
		return result
	}

	if targetBytes >= totalRemaining {
		for i, blk := range rc.blocks {
			result[i] = len(blk.Passes) - allocatedPasses[i]
		}
		return result
	}

	// Each block gets a proportional share.
	for i, blk := range rc.blocks {
		if blockRemaining[i] == 0 {
			continue
		}

		blockBudget := int(float64(targetBytes) * float64(blockRemaining[i]) / float64(totalRemaining))

		// Find the maximum number of remaining passes that fit.
		cumBytes := 0
		for pi := allocatedPasses[i]; pi < len(blk.Passes); pi++ {
			cumBytes += blk.Passes[pi].Length
			if cumBytes > blockBudget {
				break
			}
			result[i]++
		}
	}

	return result
}
