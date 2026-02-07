package jpeg2000

// Tag Tree Decoder for JPEG2000 Packet Headers
//
// Tag trees are used in JPEG2000 to efficiently encode 2D arrays of non-negative
// integers. They are hierarchical structures where each level represents a
// quadrant reduction of the previous level. Values are encoded incrementally,
// allowing early termination when a threshold is reached.
//
// Used for:
// - Code block inclusion (which blocks have data in this layer)
// - Zero bit-plane information (number of leading zero bit-planes)
//
// Reference: ITU-T T.800 Annex B.10

// tagTree implements a JPEG2000 tag tree
type tagTree struct {
	width, height int         // Dimensions of the base level
	levels        int         // Number of levels (including base)
	nodes         [][][]int32 // Node values: [level][y][x]
	states        [][][]int32 // Current state: [level][y][x] (known lower bound)
}

// newTagTree creates a tag tree for the given dimensions
func newTagTree(width, height int) *tagTree {
	if width <= 0 || height <= 0 {
		return &tagTree{width: 0, height: 0, levels: 0}
	}

	// Calculate number of levels
	// Each level reduces dimensions by factor of 2 until we reach 1x1
	levels := 1
	w, h := width, height
	for w > 1 || h > 1 {
		w = (w + 1) >> 1
		h = (h + 1) >> 1
		levels++
	}

	// Allocate nodes and states for each level
	nodes := make([][][]int32, levels)
	states := make([][][]int32, levels)

	w, h = width, height
	for lvl := 0; lvl < levels; lvl++ {
		nodes[lvl] = make([][]int32, h)
		states[lvl] = make([][]int32, h)
		for y := 0; y < h; y++ {
			nodes[lvl][y] = make([]int32, w)
			states[lvl][y] = make([]int32, w)
			for x := 0; x < w; x++ {
				// Initialize with large value (not yet decoded)
				nodes[lvl][y][x] = -1
			}
		}
		w = (w + 1) >> 1
		h = (h + 1) >> 1
	}

	return &tagTree{
		width:  width,
		height: height,
		levels: levels,
		nodes:  nodes,
		states: states,
	}
}

// decodeInclusion checks if a code block is included at or before the given layer
// Returns: true if included (value <= layer), false if not included (value > layer)
// Uses raw bit reading per ITU-T T.800 Annex B.10
//
// This follows OpenJPEG's opj_tgt_decode() logic:
// - 'low' propagates down from root, taking min(low, node_value) at each level
// - Bits are read while low < threshold AND low < node_value
// - A 0 bit means value > low, increment low
// - A 1 bit means value == low, set node_value = low
func (tt *tagTree) decodeInclusion(x, y int, layer int32, br *bitReader) (bool, error) {
	if tt.levels == 0 || x >= tt.width || y >= tt.height {
		return false, nil
	}

	// Build list of ancestors from root to leaf
	ancestors := make([]struct{ x, y int }, tt.levels)
	px, py := x, y
	for lvl := 0; lvl < tt.levels; lvl++ {
		ancestors[lvl] = struct{ x, y int }{px, py}
		px >>= 1
		py >>= 1
	}

	// low starts at 0 and propagates down, like OpenJPEG
	var low int32 = 0
	threshold := layer + 1 // We want to know if value <= layer

	// Decode from root to leaf
	for lvl := tt.levels - 1; lvl >= 0; lvl-- {
		ax, ay := ancestors[lvl].x, ancestors[lvl].y

		// Resume from saved state (known lower bound from previous layers)
		// This is critical: state tracks progress across layers, so we skip
		// bits already decoded in previous decodeInclusion calls
		if tt.states[lvl][ay][ax] > low {
			low = tt.states[lvl][ay][ax]
		}

		// Clamp low to node's value if already decoded (like OpenJPEG: if (low > node->value) low = node->value)
		if tt.nodes[lvl][ay][ax] >= 0 && low > tt.nodes[lvl][ay][ax] {
			low = tt.nodes[lvl][ay][ax]
		}

		// Update state from propagated low
		if low > tt.states[lvl][ay][ax] {
			tt.states[lvl][ay][ax] = low
		}

		// If already decoded, just propagate low and continue
		if tt.nodes[lvl][ay][ax] >= 0 {
			low = tt.nodes[lvl][ay][ax]
			continue
		}

		// Decode bits while low < threshold AND low < node_value (node_value is infinite since not decoded)
		// Per ITU-T T.800 Annex B.10 and OpenJPEG's opj_tgt_decode
		for low < threshold {
			bit, err := br.ReadBit()
			if err != nil {
				return false, err
			}
			if bit == 0 {
				// Value is greater than current low
				low++
				tt.states[lvl][ay][ax] = low
			} else {
				// Value equals current low
				tt.nodes[lvl][ay][ax] = low
				break
			}
		}

		// Update low for next level (child gets parent's value)
		if tt.nodes[lvl][ay][ax] >= 0 {
			low = tt.nodes[lvl][ay][ax]
		}
	}

	// Check if included at or before this layer
	val := tt.nodes[0][y][x]
	return val >= 0 && val <= layer, nil
}

// decodeZBP decodes the zero bit-planes value at position (x, y)
// Uses raw bit reading per ITU-T T.800 Annex B.10
//
// ZBP (zero bit-planes) uses the same tag tree logic as inclusion,
// but without a threshold - we always decode until we find the value.
// This follows OpenJPEG's opj_tgt_decode() with threshold = infinity.
func (tt *tagTree) decodeZBP(x, y int, br *bitReader) (int32, error) {
	if tt.levels == 0 || x >= tt.width || y >= tt.height {
		return 0, nil
	}

	// Build list of ancestors from root to leaf
	ancestors := make([]struct{ x, y int }, tt.levels)
	px, py := x, y
	for lvl := 0; lvl < tt.levels; lvl++ {
		ancestors[lvl] = struct{ x, y int }{px, py}
		px >>= 1
		py >>= 1
	}

	// low starts at 0 and propagates down, like OpenJPEG
	var low int32 = 0

	// Maximum ZBP value (guard against corrupt bitstreams)
	// Per spec, this should be <= Mb (number of magnitude bits), typically <= 16
	const maxZBP int32 = 32

	// Decode from root to leaf
	for lvl := tt.levels - 1; lvl >= 0; lvl-- {
		ax, ay := ancestors[lvl].x, ancestors[lvl].y

		// Resume from saved state (for multi-level tagtrees where parent
		// state may have advanced from previous decodes)
		if tt.states[lvl][ay][ax] > low {
			low = tt.states[lvl][ay][ax]
		}

		// Clamp low to node's value if already decoded
		if tt.nodes[lvl][ay][ax] >= 0 && low > tt.nodes[lvl][ay][ax] {
			low = tt.nodes[lvl][ay][ax]
		}

		// Update state from propagated low
		if low > tt.states[lvl][ay][ax] {
			tt.states[lvl][ay][ax] = low
		}

		// If already decoded, just propagate low and continue
		if tt.nodes[lvl][ay][ax] >= 0 {
			low = tt.nodes[lvl][ay][ax]
			continue
		}

		// Decode bits until we find the value (no threshold, but guard against corruption)
		for low < maxZBP {
			bit, err := br.ReadBit()
			if err != nil {
				return 0, err
			}
			if bit == 0 {
				// Value is greater than current low
				low++
				tt.states[lvl][ay][ax] = low
			} else {
				// Value equals current low
				tt.nodes[lvl][ay][ax] = low
				break
			}
		}

		// Safety: if we hit maxZBP without finding a 1 bit, set the value
		if tt.nodes[lvl][ay][ax] < 0 {
			tt.nodes[lvl][ay][ax] = low
		}

		// Update low for next level
		low = tt.nodes[lvl][ay][ax]
	}

	return tt.nodes[0][y][x], nil
}

// setValue sets the value at position (x, y) directly (for testing)
func (tt *tagTree) setValue(x, y int, val int32) {
	if tt.levels == 0 || x >= tt.width || y >= tt.height {
		return
	}
	tt.nodes[0][y][x] = val
}

// getValue gets the value at position (x, y), or -1 if not decoded
func (tt *tagTree) getValue(x, y int) int32 {
	if tt.levels == 0 || x >= tt.width || y >= tt.height {
		return 0
	}
	return tt.nodes[0][y][x]
}

// setValueWithPropagation sets the value at position (x, y) and propagates
// minimum values up through the tree hierarchy.
// This must be called for all leaf nodes before encoding.
func (tt *tagTree) setValueWithPropagation(x, y int, val int32) {
	if tt.levels == 0 || x >= tt.width || y >= tt.height {
		return
	}
	tt.nodes[0][y][x] = val

	// Propagate minimum values upward through the hierarchy
	px, py := x, y
	for lvl := 0; lvl < tt.levels-1; lvl++ {
		px >>= 1
		py >>= 1
		parentVal := tt.nodes[lvl+1][py][px]
		if parentVal < 0 || val < parentVal {
			tt.nodes[lvl+1][py][px] = val
		}
	}
}

// encodeInclusion encodes whether a code block is included at or before the given layer.
// This is the encoding counterpart of decodeInclusion.
// The value at (x,y) represents the first layer where the block is included.
// Per ITU-T T.800 Annex B.10: encode from root to leaf, writing 0-bits for
// "value > current" and a 1-bit when "value == current".
func (tt *tagTree) encodeInclusion(x, y int, layer int32, bw *bitWriter) {
	if tt.levels == 0 || x >= tt.width || y >= tt.height {
		return
	}

	// Build list of ancestors from root to leaf.
	// This mirrors decodeInclusion exactly.
	ancestors := make([]struct{ x, y int }, tt.levels)
	px, py := x, y
	for lvl := 0; lvl < tt.levels; lvl++ {
		ancestors[lvl] = struct{ x, y int }{px, py}
		px >>= 1
		py >>= 1
	}

	threshold := layer + 1
	var low int32 = 0

	// Encode from root to leaf (same traversal order as decoder)
	for lvl := tt.levels - 1; lvl >= 0; lvl-- {
		ax, ay := ancestors[lvl].x, ancestors[lvl].y

		// Resume from saved state (same as decoder)
		if tt.states[lvl][ay][ax] > low {
			low = tt.states[lvl][ay][ax]
		}

		nodeVal := tt.nodes[lvl][ay][ax]

		// Clamp low to node's value if known (same as decoder)
		if nodeVal >= 0 && low > nodeVal {
			low = nodeVal
		}

		// Update state
		if low > tt.states[lvl][ay][ax] {
			tt.states[lvl][ay][ax] = low
		}

		// If node already fully resolved (state has passed the value),
		// the decoder already knows this value - just propagate.
		if nodeVal >= 0 && tt.states[lvl][ay][ax] > nodeVal {
			low = nodeVal
			continue
		}

		// If state >= threshold, nothing more to encode at this threshold
		if tt.states[lvl][ay][ax] >= threshold {
			if nodeVal >= 0 {
				low = nodeVal
			}
			continue
		}

		// Encode bits: mirror the decoder's reading loop exactly.
		// The decoder reads while low < threshold:
		//   bit=0: value > low, low++, save state
		//   bit=1: value == low, set node, break
		for low < threshold {
			if nodeVal >= 0 && low == nodeVal {
				// Value found: write 1-bit. Decoder sets node=low and breaks.
				bw.WriteBit(1)
				// Mark as emitted so subsequent calls skip this node.
				// The decoder will have nodes[lvl] >= 0 after reading the 1-bit,
				// so it will skip on future calls. We mirror this by advancing
				// state past nodeVal.
				tt.states[lvl][ay][ax] = nodeVal + 1
				break
			}
			// Value > low: write 0-bit
			bw.WriteBit(0)
			low++
			tt.states[lvl][ay][ax] = low
		}

		if nodeVal >= 0 {
			low = nodeVal
		}
	}
}

// encodeValue encodes the absolute value at position (x, y).
// This is the encoding counterpart of decodeZBP.
// Used for zero bit-plane information.
// Encodes from root to leaf, writing 0-bits followed by a 1-bit when value found.
func (tt *tagTree) encodeValue(x, y int, bw *bitWriter) {
	if tt.levels == 0 || x >= tt.width || y >= tt.height {
		return
	}

	ancestors := make([]struct{ x, y int }, tt.levels)
	px, py := x, y
	for lvl := 0; lvl < tt.levels; lvl++ {
		ancestors[lvl] = struct{ x, y int }{px, py}
		px >>= 1
		py >>= 1
	}

	var low int32 = 0

	for lvl := tt.levels - 1; lvl >= 0; lvl-- {
		ax, ay := ancestors[lvl].x, ancestors[lvl].y

		if tt.states[lvl][ay][ax] > low {
			low = tt.states[lvl][ay][ax]
		}

		nodeVal := tt.nodes[lvl][ay][ax]
		if nodeVal >= 0 && low > nodeVal {
			low = nodeVal
		}

		if low > tt.states[lvl][ay][ax] {
			tt.states[lvl][ay][ax] = low
		}

		// Already fully encoded at this level (state must have advanced past value)
		if nodeVal >= 0 && tt.states[lvl][ay][ax] > nodeVal {
			low = nodeVal
			continue
		}

		// Encode until we reach the value
		for nodeVal >= 0 && low < nodeVal {
			bw.WriteBit(0)
			low++
			tt.states[lvl][ay][ax] = low
		}

		if nodeVal >= 0 && low == nodeVal {
			bw.WriteBit(1)
			tt.states[lvl][ay][ax] = nodeVal + 1
		}

		if nodeVal >= 0 {
			low = nodeVal
		}
	}
}

// resetStates resets all state values for a new encoding pass.
// Node values are preserved; only the incremental encoding state is cleared.
func (tt *tagTree) resetStates() {
	for lvl := 0; lvl < tt.levels; lvl++ {
		h := len(tt.states[lvl])
		for y := range h {
			for x := range tt.states[lvl][y] {
				tt.states[lvl][y][x] = 0
			}
		}
	}
}
