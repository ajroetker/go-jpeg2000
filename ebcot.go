package jpeg2000

// EBCOT (Embedded Block Coding with Optimized Truncation) Tier-1 Decoder
//
// This implements the bit-plane coding used in JPEG2000 as specified in
// ITU-T T.800 Annex D. EBCOT encodes wavelet coefficients using three
// coding passes per bit-plane:
//
// 1. Significance Propagation Pass: Codes coefficients with significant neighbors
// 2. Magnitude Refinement Pass: Refines already-significant coefficients
// 3. Cleanup Pass: Codes remaining coefficients with run-length optimization
//
// Each code block (typically 32x32 or 64x64) is independently coded using
// MQ arithmetic coding with 19 different contexts based on coefficient
// neighborhood patterns.

// EBCOT context IDs (0-18)
const (
	// Significance propagation contexts (0-8)
	// Context depends on number and orientation of significant neighbors

	// For HL/LH subbands (horizontal/vertical detail)
	ctxSigHL_0 = 0 // No significant neighbors
	ctxSigHL_1 = 1 // 1 significant neighbor
	ctxSigHL_2 = 2 // 2+ significant neighbors (horizontal)
	ctxSigHL_3 = 3 // Mixed pattern
	ctxSigHL_4 = 4 // Different pattern
	ctxSigHL_5 = 5 // Vertical pattern
	ctxSigHL_6 = 6 // Another pattern
	ctxSigHL_7 = 7 // Another pattern
	ctxSigHL_8 = 8 // High activity

	// Sign coding contexts (9-13)
	ctxSign_0 = 9  // Neutral sign context
	ctxSign_1 = 10 // Positive bias
	ctxSign_2 = 11 // Negative bias
	ctxSign_3 = 12 // Mixed
	ctxSign_4 = 13 // Another pattern

	// Magnitude refinement contexts (14-16)
	// Per ITU-T T.800 Table D.3, context depends on first refinement and neighbors
	ctxMagFirst = 14 // First refinement, no significant neighbors
	ctxMagOther = 15 // Subsequent refinements or has significant neighbors
	// Context 16 is for magnitude with certain neighbor patterns (not always used)

	// Cleanup pass contexts (17-18)
	// Per OpenJPEG t1.h: T1_CTXNO_AGG = 17, T1_CTXNO_UNI = 18
	ctxAggZero = 17 // Aggregation context for run-mode check
	ctxUniform = 18 // Uniform context (50/50 probability, state 46)
)

// Coefficient state flags
const (
	flagSignificant = 1 << iota // Coefficient has become significant
	flagSign                    // Sign bit (1 = negative)
	flagRefined                 // Has been refined at least once
	flagVisited                 // Visited in current pass
	flagNeighborSig             // Has at least one significant neighbor (for fast context lookup)
)

// T1_NMSEDEC_FRACBITS is the number of fractional bits used in coefficient
// representation per ITU-T T.800/OpenJPEG. Coefficients are stored as
// fixed-point values with 6 fractional bits, meaning the actual magnitude
// is coefficient_value >> 6.
const t1FracBits = 6

// LUT index bit positions for sign context calculation
// These match OpenJPEG's t1.h definitions
const (
	lutSgnW = 1 << 0 // West neighbor sign
	lutSigN = 1 << 1 // North neighbor significance
	lutSgnE = 1 << 2 // East neighbor sign
	lutSigW = 1 << 3 // West neighbor significance
	lutSgnN = 1 << 4 // North neighbor sign
	lutSigE = 1 << 5 // East neighbor significance
	lutSgnS = 1 << 6 // South neighbor sign
	lutSigS = 1 << 7 // South neighbor significance
)

// lutCtxnoSC maps 8-bit neighbor pattern to sign context (9-13)
// Copied directly from OpenJPEG t1_luts.h
var lutCtxnoSC = [256]byte{
	0x9, 0x9, 0xa, 0xa, 0x9, 0x9, 0xa, 0xa, 0xc, 0xc, 0xd, 0xb, 0xc, 0xc, 0xd, 0xb,
	0x9, 0x9, 0xa, 0xa, 0x9, 0x9, 0xa, 0xa, 0xc, 0xc, 0xb, 0xd, 0xc, 0xc, 0xb, 0xd,
	0xc, 0xc, 0xd, 0xd, 0xc, 0xc, 0xb, 0xb, 0xc, 0x9, 0xd, 0xa, 0x9, 0xc, 0xa, 0xb,
	0xc, 0xc, 0xb, 0xb, 0xc, 0xc, 0xd, 0xd, 0xc, 0x9, 0xb, 0xa, 0x9, 0xc, 0xa, 0xd,
	0x9, 0x9, 0xa, 0xa, 0x9, 0x9, 0xa, 0xa, 0xc, 0xc, 0xd, 0xb, 0xc, 0xc, 0xd, 0xb,
	0x9, 0x9, 0xa, 0xa, 0x9, 0x9, 0xa, 0xa, 0xc, 0xc, 0xb, 0xd, 0xc, 0xc, 0xb, 0xd,
	0xc, 0xc, 0xd, 0xd, 0xc, 0xc, 0xb, 0xb, 0xc, 0x9, 0xd, 0xa, 0x9, 0xc, 0xa, 0xb,
	0xc, 0xc, 0xb, 0xb, 0xc, 0xc, 0xd, 0xd, 0xc, 0x9, 0xb, 0xa, 0x9, 0xc, 0xa, 0xd,
	0xa, 0xa, 0xa, 0xa, 0xa, 0xa, 0xa, 0xa, 0xd, 0xb, 0xd, 0xb, 0xd, 0xb, 0xd, 0xb,
	0xa, 0xa, 0x9, 0x9, 0xa, 0xa, 0x9, 0x9, 0xd, 0xb, 0xc, 0xc, 0xd, 0xb, 0xc, 0xc,
	0xd, 0xd, 0xd, 0xd, 0xb, 0xb, 0xb, 0xb, 0xd, 0xa, 0xd, 0xa, 0xa, 0xb, 0xa, 0xb,
	0xd, 0xd, 0xc, 0xc, 0xb, 0xb, 0xc, 0xc, 0xd, 0xa, 0xc, 0x9, 0xa, 0xb, 0x9, 0xc,
	0xa, 0xa, 0x9, 0x9, 0xa, 0xa, 0x9, 0x9, 0xb, 0xd, 0xc, 0xc, 0xb, 0xd, 0xc, 0xc,
	0xa, 0xa, 0xa, 0xa, 0xa, 0xa, 0xa, 0xa, 0xb, 0xd, 0xb, 0xd, 0xb, 0xd, 0xb, 0xd,
	0xb, 0xb, 0xc, 0xc, 0xd, 0xd, 0xc, 0xc, 0xb, 0xa, 0xc, 0x9, 0xa, 0xd, 0x9, 0xc,
	0xb, 0xb, 0xb, 0xb, 0xd, 0xd, 0xd, 0xd, 0xb, 0xa, 0xb, 0xa, 0xa, 0xd, 0xa, 0xd,
}

// lutSPB maps 8-bit neighbor pattern to sign prediction bit (0 or 1)
// The actual sign = decoded_bit XOR lutSPB[index]
// Copied directly from OpenJPEG t1_luts.h
var lutSPB = [256]byte{
	0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1,
	0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1,
	0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
	0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1,
	1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1,
	0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
}

// Note: SubbandType and CodeBlock are defined in codestream.go

// CodeBlockStyle flags (from COD marker cblksty field)
const (
	cbstyBypass     = 0x01 // Selective arithmetic coding bypass
	cbstyReset      = 0x02 // Reset context probabilities on coding pass boundaries
	cbstyTerminate  = 0x04 // Termination on each coding pass (ERTERM)
	cbstyVertCausal = 0x08 // Vertically causal context
	cbstyPredTerm   = 0x10 // Predictable termination (SEGTERM)
	cbstySegSymbols = 0x20 // Segmentation symbols (SEGSYM)
)

// ebcotDecoder decodes EBCOT code blocks
type ebcotDecoder struct {
	mq     *mqDecoder
	width  int
	height int

	// Maximum dimensions (for state array allocation)
	maxWidth  int
	maxHeight int

	// State arrays (width+2 x height+2 for border handling)
	// Border rows/cols stay zero to simplify neighbor checks
	state [][]uint8 // Flags for each coefficient
	data  [][]int32 // Magnitude data (reconstructed coefficients)

	// Bit plane being processed
	bitPlane int

	// CodeBlockStyle flags from COD marker
	codeBlockStyle byte

	// ERTERM mode: per-pass segment handling
	segmentLengths []int  // Length of each pass's data segment
	segmentOffsets []int  // Byte offset in Data for each pass
	currentPass    int    // Current pass index (for segment tracking)
	codeBlockData  []byte // Reference to the code block's data

	// BYPASS mode: raw coding state tracking
	// When true, we're in raw coding mode and using the raw bit decoder
	inRawMode bool

	// BYPASS segment tracking (when BYPASS without ERTERM)
	// Each segment groups consecutive passes that share the same coding mode.
	bypassSegments     []bypassSegment // Segment definitions (passCount, isRaw)
	bypassSegmentIdx   int             // Current segment index
	bypassPassInSeg    int             // Passes consumed in current segment
	isBypassSegmented  bool            // True when using bypass segment mode

	// Subband type (for debug logging)
	subbandType SubbandType

	// Code block ID for tracing (unique per code block)
	cbID int

	// Number of significant bit planes for BYPASS mode threshold calculation
	// Set in DecodeCodeBlock from mb - ZeroBitPlanes
	numBitPlanes int
}

// newEBCOTDecoder creates a new EBCOT decoder
func newEBCOTDecoder(width, height int) *ebcotDecoder {
	e := &ebcotDecoder{
		width:     width,
		height:    height,
		maxWidth:  width,
		maxHeight: height,
	}

	// Allocate state arrays with borders
	e.state = make([][]uint8, height+2)
	e.data = make([][]int32, height+2)
	for i := range e.state {
		e.state[i] = make([]uint8, width+2)
		e.data[i] = make([]int32, width+2)
	}

	return e
}

// DecodeCodeBlock decodes a single code block
// Returns 2D array of signed coefficients (no border padding)
// subbandType is needed for context selection
func (e *ebcotDecoder) DecodeCodeBlock(cb *CodeBlock, subbandType SubbandType) ([][]int32, error) {
	// Update dimensions to match actual code block (may be smaller than max)
	e.width = cb.Width
	e.height = cb.Height

	// Store CodeBlockStyle for segmentation symbol handling
	e.codeBlockStyle = cb.CodeBlockStyle

	// Store subband type for debug logging
	e.subbandType = subbandType

	// Store segment information for ERTERM mode
	e.segmentLengths = cb.SegmentLengths
	e.codeBlockData = cb.Data
	e.currentPass = 0

	// Reset raw mode state for new code block
	e.inRawMode = false

	// Reset bypass segment tracking
	e.bypassSegments = nil
	e.bypassSegmentIdx = 0
	e.bypassPassInSeg = 0
	e.isBypassSegmented = false

	// Calculate segment offsets from lengths
	if len(cb.SegmentLengths) > 0 {
		e.segmentOffsets = make([]int, len(cb.SegmentLengths))
		offset := 0
		for i, length := range cb.SegmentLengths {
			e.segmentOffsets[i] = offset
			offset += length
		}
	} else {
		e.segmentOffsets = nil
	}

	// Detect BYPASS mode: compute bypass segments for decoder.
	// BYPASS can coexist with ERTERM - they are independent features per ITU-T T.800 D.2.
	// When both are set, ERTERM provides per-pass segments and BYPASS determines
	// whether each pass uses MQ or raw coding.
	// When BYPASS is set without ERTERM, segment boundaries are determined by
	// MQ-to-raw transitions.
	isBypass := cb.CodeBlockStyle&cbstyBypass != 0
	hasERTERM := cb.CodeBlockStyle&cbstyTerminate != 0
	if isBypass && !hasERTERM && len(cb.SegmentLengths) > 1 {
		// BYPASS without ERTERM: We have per-segment lengths from the packet parser.
		// Recompute the segment structure to know which segments are MQ vs RAW.
		mb := cb.MagnitudeBitPlanes
		if mb < 1 {
			mb = 8
		}
		numBitPlanes := mb - cb.ZeroBitPlanes
		e.bypassSegments = computeBypassSegments(cb.NumPasses, numBitPlanes)
		e.isBypassSegmented = len(e.bypassSegments) > 1
	}

	// Initialize MQ decoder for first segment
	if e.isERTERM() && len(e.segmentLengths) > 0 && len(cb.Data) > 0 {
		// ERTERM mode: initialize MQ with first segment.
		// Even if BYPASS is also set, the first pass (cleanup) always uses MQ.
		segData := e.getSegmentData(0)
		if e.mq == nil {
			e.mq = newMQDecoder(segData)
		} else {
			e.mq.Reset(segData)
		}
	} else if e.isBypassSegmented {
		// BYPASS segment mode: initialize MQ with first segment's data
		segData := e.getSegmentData(0)
		if e.mq == nil {
			e.mq = newMQDecoder(segData)
		} else {
			e.mq.Reset(segData)
		}
	} else {
		// Normal mode: initialize with all data
		if e.mq == nil {
			e.mq = newMQDecoder(cb.Data)
		} else {
			e.mq.Reset(cb.Data)
		}
	}

	// Clear state and data arrays
	e.resetState()

	// Start from the first coded bit plane
	// Per ITU-T T.800 and OpenJPEG t2.c, the number of coded bit planes is:
	//   numbps = Mb + 1 - ZBP
	// And the first bitplane is: bpno = numbps - 1 = Mb - ZBP
	//
	// This matches OpenJPEG t1.c where bpno_plus_one = cblk->numbps and the first
	// decoded bitplane is bpno = bpno_plus_one - 1.
	mb := cb.MagnitudeBitPlanes
	if mb < 1 {
		mb = 8 // Fallback for 8-bit data
	}
	startBitPlane := max(mb-cb.ZeroBitPlanes-1, 0)

	// Set number of bit planes for BYPASS mode threshold calculation
	// numbps = mb - ZBP (number of coded bit planes)
	e.numBitPlanes = mb - cb.ZeroBitPlanes

	// Decode using continuous passtype counter (matching OpenJPEG's algorithm)
	//
	// JPEG2000 pass types cycle: cleanup(2) -> SPP(0) -> MRP(1) -> cleanup(2) -> ...
	// The bit plane only decrements when passtype cycles from cleanup(2) to SPP(0).
	//
	// Pass types:
	//   0 = Significance Propagation Pass (SPP)
	//   1 = Magnitude Refinement Pass (MRP)
	//   2 = Cleanup Pass
	//
	// The first coded bit plane starts with cleanup (passtype=2).
	// After cleanup, passtype cycles to 0 (SPP) and bit plane decrements.
	passtype := 2 // Start with cleanup pass
	bp := startBitPlane

	for pass := 0; pass < cb.NumPasses; pass++ {
		e.bitPlane = bp

		if err := e.decodeSinglePass(bp, passtype, subbandType); err != nil {
			return nil, err
		}

		// Cycle passtype: 2 -> 0 -> 1 -> 2 -> ...
		// Bit plane decrements only on cleanup(2) -> SPP(0) transition
		passtype++
		if passtype == 3 {
			passtype = 0
			if bp > 0 {
				bp--
			}
		}
	}

	// Extract result without borders
	// With OpenJPEG-style signed data storage, sign is already applied
	result := make([][]int32, e.height)
	for y := 0; y < e.height; y++ {
		result[y] = make([]int32, e.width)
		for x := 0; x < e.width; x++ {
			result[y][x] = e.data[y+1][x+1]
		}
	}

	return result, nil
}

// resetState clears all state for new code block
// CRITICAL: Must clear the ENTIRE allocated array (maxHeight+2 x maxWidth+2),
// not just the current code block's size, to avoid residual state from larger
// previous code blocks contaminating smaller code blocks.
func (e *ebcotDecoder) resetState() {
	// NOTE: Don't reset significance tracking here - we want to accumulate across all code blocks
	// The test code should reset tracking before calling Decode()

	// Clear entire allocated array, not just current code block size
	// Arrays are allocated as (height+2) x (width+2), so we clear up to those bounds
	for y := 0; y < len(e.state); y++ {
		for x := 0; x < len(e.state[y]); x++ {
			e.state[y][x] = 0
			e.data[y][x] = 0
		}
	}
}

// isERTERM returns true if ERTERM (termination on each coding pass) is enabled
func (e *ebcotDecoder) isERTERM() bool {
	return e.codeBlockStyle&cbstyTerminate != 0
}

// isRESET returns true if RESET (reset context probabilities on pass boundaries) is enabled
func (e *ebcotDecoder) isRESET() bool {
	return e.codeBlockStyle&cbstyReset != 0
}

// isVertCausal returns true if VSC (vertically stripe causal context) is enabled
func (e *ebcotDecoder) isVertCausal() bool {
	return e.codeBlockStyle&cbstyVertCausal != 0
}

// isBypass returns true if BYPASS (selective arithmetic coding bypass) is enabled
func (e *ebcotDecoder) isBypass() bool {
	return e.codeBlockStyle&cbstyBypass != 0
}

// useRawMode determines if raw (bypass) mode should be used for the current pass.
// Per OpenJPEG t1.c, raw mode is used when ALL of these conditions are true:
//  1. BYPASS flag (0x01) is set in codeblock style
//  2. Current bitplane is at least 4 planes below the maximum
//  3. Pass type is SPP (0) or MRP (1) - cleanup pass ALWAYS uses MQ
//
// The bitplane threshold (numbps - 4) ensures that only the lower (less significant)
// bitplanes use raw coding, where the coefficient bits have higher entropy and
// arithmetic coding provides less benefit.
func (e *ebcotDecoder) useRawMode(bp int, passtype int, numBitPlanes int) bool {
	if !e.isBypass() {
		return false
	}
	// Cleanup pass always uses MQ arithmetic coding
	if passtype == passTypeCleanup {
		return false
	}
	// Raw mode for lower bitplanes only (at least 4 below max)
	// bp is 0-indexed where 0 is LSB, numBitPlanes-1 is MSB
	// Use raw when bp <= numBitPlanes - 5 (i.e., 4+ planes below max)
	return bp <= numBitPlanes-5
}

// getSegmentData returns the data slice for a specific segment (pass) index
func (e *ebcotDecoder) getSegmentData(passIndex int) []byte {
	if passIndex < 0 || passIndex >= len(e.segmentOffsets) || passIndex >= len(e.segmentLengths) {
		return nil
	}
	offset := e.segmentOffsets[passIndex]
	length := e.segmentLengths[passIndex]
	if offset+length > len(e.codeBlockData) {
		return nil
	}
	return e.codeBlockData[offset : offset+length]
}

// reinitMQForPass reinitializes the MQ decoder for a new pass in ERTERM mode.
//
// This function handles ERTERM (0x04) segment initialization only.
// RESET (0x02) context probability reset is handled separately in decodeSinglePass
// AFTER each pass completes, matching OpenJPEG's t1.c behavior.
//
// OpenJPEG's decoding order for ERTERM:
//   For each segment (1 pass per segment in ERTERM mode):
//     1. opj_mqc_init_dec(segment_data)  - Initialize decoder state (A, C, CT)
//     2. Process pass (sig/ref/cln)
//     3. if RESET: opj_mqc_resetstates() - Reset contexts AFTER pass
//
// NOTE: For pass 0, the MQ decoder was already initialized with Reset() in
// DecodeCodeBlock (which both resets contexts AND initializes decoder state).
// We skip segment reinitialization here to avoid double-initialization.
func (e *ebcotDecoder) reinitMQForPass() {
	if !e.isERTERM() || len(e.segmentLengths) == 0 {
		return
	}

	// Skip pass 0 - it was already initialized in DecodeCodeBlock with Reset()
	// which both resets contexts AND initializes the decoder state.
	if e.currentPass == 0 {
		return
	}

	// Always reinitialize MQ for each pass in ERTERM mode, even for 0-byte segments.
	// A 0-byte segment still needs fresh MQ state (the decoder will produce
	// predictable output from the 0xFF fill pattern).
	segData := e.getSegmentData(e.currentPass)
	if segData == nil {
		segData = []byte{}
	}

	// Initialize the MQ decoder with the new segment data.
	// Context probabilities are NOT reset here - that happens AFTER each pass
	// in decodeSinglePass, matching OpenJPEG's behavior.
	e.mq.InitSegment(segData)
}

// advancePass increments the current pass counter for ERTERM tracking
func (e *ebcotDecoder) advancePass() {
	e.currentPass++
}

// currentSegmentHasData returns true if the current segment has data to decode.
// For ERTERM mode, segments can have 0 length which means the pass was truncated.
// For bypass-segmented mode, always return true because OpenJPEG processes
// ALL passes even for 0-byte segments (the MQ/raw decoder operates on synthetic
// fill data and produces deterministic results).
// For ERTERM mode, check the current segment for data.
// For non-segmented mode, all data is concatenated so we always return true.
func (e *ebcotDecoder) currentSegmentHasData() bool {
	if e.isERTERM() {
		segData := e.getSegmentData(e.currentPass)
		return segData != nil && len(segData) > 0
	}
	// For bypass-segmented and normal modes, always process passes.
	// Even 0-byte segments are processed (decoder operates on fill data).
	return true
}

// handleBypassSegmentTransition checks if the current pass is the first pass
// of a new bypass segment. If so, it initializes the appropriate decoder
// (MQ or raw) with that segment's data slice.
//
// This is called at the start of each pass in bypass-segmented mode.
// The first segment (index 0) was already initialized in DecodeCodeBlock,
// so we only need to handle transitions to subsequent segments.
func (e *ebcotDecoder) handleBypassSegmentTransition() {
	if !e.isBypassSegmented {
		return
	}
	if e.bypassSegmentIdx >= len(e.bypassSegments) {
		return
	}

	// Check if this is the first pass of the current segment
	if e.bypassPassInSeg == 0 && e.bypassSegmentIdx > 0 {
		// Starting a new segment - initialize decoder with segment data
		segData := e.getSegmentData(e.bypassSegmentIdx)
		if segData == nil {
			segData = []byte{}
		}

		seg := e.bypassSegments[e.bypassSegmentIdx]
		if seg.isRaw {
			// RAW segment: set data buffer directly and init raw decoder from byte 0.
			// Do NOT call InitSegment() which runs initDec() (MQ initialization) --
			// that would consume bytes from the segment data via MQ bytein before
			// the raw decoder can read them.
			e.mq.SetRawData(segData)
			e.inRawMode = true
		} else {
			// MQ segment: initialize MQ decoder with segment data
			e.inRawMode = false
			e.mq.InitSegment(segData)
		}
	}
}

// advanceBypassPass advances the bypass segment tracking after a pass completes.
// When the current segment's passes are exhausted, advances to the next segment.
func (e *ebcotDecoder) advanceBypassPass() {
	if !e.isBypassSegmented {
		return
	}
	if e.bypassSegmentIdx >= len(e.bypassSegments) {
		return
	}

	e.bypassPassInSeg++
	if e.bypassPassInSeg >= e.bypassSegments[e.bypassSegmentIdx].passCount {
		// Finished this segment, advance to next
		e.bypassSegmentIdx++
		e.bypassPassInSeg = 0
	}
}

// Pass type constants matching OpenJPEG's algorithm
const (
	passTypeSPP     = 0 // Significance Propagation Pass
	passTypeMRP     = 1 // Magnitude Refinement Pass
	passTypeCleanup = 2 // Cleanup Pass
)

// decodeSinglePass decodes a single coding pass based on passtype.
// This matches OpenJPEG's continuous passtype counter approach.
//
// Pass types:
//   - 0 (SPP): Significance Propagation Pass - processes coefficients with significant neighbors
//   - 1 (MRP): Magnitude Refinement Pass - refines already-significant coefficients
//   - 2 (Cleanup): Cleanup Pass - processes remaining coefficients, uses run-mode optimization
//
// ERTERM mode: Each pass has its own independently terminated MQ segment.
//
// BYPASS mode: Raw coding passes (SPP/MRP on lower bitplanes) use a separate
// raw bit decoder that handles 0xFF byte stuffing differently from MQ coding.
// Per ITU-T T.800 and OpenJPEG, raw passes must:
//   1. Start from a byte boundary (after MQ cleanup terminates)
//   2. Handle 0xFF stuffing (only 7 bits after 0xFF)
//   3. Sync back to MQ position before next cleanup pass
//
// OpenJPEG conformance: This function matches the order of operations in
// OpenJPEG's t1.c opj_t1_decode_cblk for ERTERM and RESET handling:
//
//   For each segment (outer loop):
//     opj_mqc_init_dec(segment_data)
//     For each pass in segment (inner loop - 1 pass for ERTERM):
//       Process pass (sigpass, refpass, or clnpass)
//       if (RESET && MQ mode): opj_mqc_resetstates()  // Reset AFTER pass
//
// The RESET context reset happens AFTER each pass completes, not before.
// This ensures contexts are in initial state when the NEXT pass begins.
func (e *ebcotDecoder) decodeSinglePass(bp int, passtype int, subbandType SubbandType) error {
	// Clear visited flags only at the START of each bit plane, NOT every pass.
	// The visited flag tracks "processed in current bit plane" and must persist
	// across SPP -> MRP -> Cleanup within the same bit plane.
	//
	// Per JPEG2000 spec:
	// - First bit plane: starts with cleanup (passtype=2)
	// - Subsequent bit planes: start with SPP (passtype=0)
	//
	// So we clear visited when:
	// - passtype=2 AND this is the first pass (e.currentPass==0) → first bit plane cleanup
	// - passtype=0 → start of subsequent bit plane (SPP)
	if passtype == passTypeSPP || (passtype == passTypeCleanup && e.currentPass == 0) {
		e.clearVisited()
	}

	// Determine raw vs MQ mode early, before decoder initialization.
	// This is needed so we can choose the correct init path for ERTERM+BYPASS.
	useRaw := e.useRawMode(bp, passtype, e.numBitPlanes)

	// In bypass-segmented mode, the segment structure determines raw vs MQ.
	if e.isBypassSegmented && e.bypassSegmentIdx < len(e.bypassSegments) {
		useRaw = e.bypassSegments[e.bypassSegmentIdx].isRaw
	}

	// Initialize decoder for this pass's segment.
	// For ERTERM mode: each pass has its own segment, init MQ or raw accordingly.
	// For bypass-segmented mode: segments group passes, transitions handled separately.
	if e.isERTERM() && e.currentPass > 0 && len(e.segmentLengths) > 0 {
		// ERTERM: per-pass segment initialization
		segData := e.getSegmentData(e.currentPass)
		if segData == nil {
			segData = []byte{}
		}
		if useRaw {
			// Raw pass: use SetRawData directly (don't waste MQ init)
			e.mq.SetRawData(segData)
			e.inRawMode = true
		} else {
			// MQ pass: initialize MQ decoder with segment data
			e.mq.InitSegment(segData)
			e.inRawMode = false
		}
	} else if !e.isERTERM() {
		// Non-ERTERM: use reinitMQForPass (no-op) and bypass segment transitions
		if e.isBypassSegmented {
			e.handleBypassSegmentTransition()
		}
	}

	// Execute the appropriate pass based on passtype
	if e.currentSegmentHasData() {
		switch passtype {
		case passTypeSPP:
			if useRaw {
				if !e.isERTERM() && !e.isBypassSegmented {
					// Non-segmented bypass: initialize raw from MQ position
					if !e.inRawMode {
						e.mq.InitRawDecoder()
						e.inRawMode = true
					}
				}
				e.significancePropagationPassRaw(bp, subbandType)
			} else {
				e.significancePropagationPass(bp, subbandType)
			}
		case passTypeMRP:
			if useRaw {
				if !e.isERTERM() && !e.isBypassSegmented {
					// Non-segmented bypass: continue raw decoding
					if !e.inRawMode {
						e.mq.InitRawDecoder()
						e.inRawMode = true
					}
				}
				e.magnitudeRefinementPassRaw(bp)
			} else {
				e.magnitudeRefinementPass(bp)
			}
		case passTypeCleanup:
			// Cleanup pass always uses MQ arithmetic coding, even in BYPASS mode
			if !e.isERTERM() && !e.isBypassSegmented {
				// Non-segmented bypass: sync back from raw position
				if e.inRawMode {
					e.mq.SyncFromRaw()
					e.inRawMode = false
				}
			}
			e.cleanupPass(bp, subbandType)
			e.readSegmentationSymbols()
		}
	}

	// Finish segment for ERTERM mode
	if e.isERTERM() {
		e.mq.FinishSegment()
	}

	// RESET (0x02): Reset context probabilities AFTER each MQ pass completes.
	// Per OpenJPEG t1.c:
	//   if ((cblksty & J2K_CCP_CBLKSTY_RESET) && type == T1_TYPE_MQ) {
	//       opj_mqc_resetstates(mqc);
	//       opj_mqc_setstate(mqc, T1_CTXNO_UNI, 0, 46);
	//       opj_mqc_setstate(mqc, T1_CTXNO_AGG, 0, 3);
	//       opj_mqc_setstate(mqc, T1_CTXNO_ZC, 0, 4);
	//   }
	// Only applies to MQ passes, NOT raw passes (matching OpenJPEG's type==T1_TYPE_MQ check).
	// In ERTERM mode, each pass reinitializes the decoder, but contexts persist
	// across segments unless RESET is set.
	if e.isRESET() && !useRaw {
		e.mq.ResetContexts()
	}

	// Advance bypass segment tracking
	if e.isBypassSegmented {
		e.advanceBypassPass()
	}

	// Advance to next pass (increments segment counter for ERTERM)
	e.advancePass()

	return nil
}

// decodeBitPlane is kept for backwards compatibility but is no longer used.
// The new decodeSinglePass function is used instead.
// TODO: Remove this function after verifying the fix works.
//
// NOTE: This legacy function does NOT implement RESET (0x02) context reset
// handling per OpenJPEG conformance. Use decodeSinglePass instead.
func (e *ebcotDecoder) decodeBitPlane(bp int, numPasses int, subbandType SubbandType, isFirstBitPlane bool) error {
	// Clear visited flags
	e.clearVisited()

	// Helper to finish pass with RESET handling (for OpenJPEG conformance)
	finishPass := func() {
		if e.isERTERM() {
			e.mq.FinishSegment()
		}
		// RESET: Reset contexts AFTER each pass (matches OpenJPEG t1.c)
		if e.isRESET() {
			e.mq.ResetContexts()
		}
		e.advancePass()
	}

	if isFirstBitPlane {
		// First bit plane: only cleanup pass runs.
		// sig_prop and mag_ref don't run because nothing is significant yet.
		// This is true for both ERTERM and non-ERTERM modes.
		if numPasses >= 1 {
			e.reinitMQForPass()
			if e.currentSegmentHasData() {
				e.cleanupPass(bp, subbandType)
				e.readSegmentationSymbols()
			}
			finishPass()
		}
	} else {
		// Subsequent bit planes: sig_prop, mag_ref, cleanup
		if numPasses >= 1 {
			e.reinitMQForPass()
			if e.currentSegmentHasData() {
				e.significancePropagationPass(bp, subbandType)
			}
			finishPass()
		}
		if numPasses >= 2 {
			e.reinitMQForPass()
			if e.currentSegmentHasData() {
				e.magnitudeRefinementPass(bp)
			}
			finishPass()
		}
		if numPasses >= 3 {
			e.reinitMQForPass()
			if e.currentSegmentHasData() {
				e.cleanupPass(bp, subbandType)
				e.readSegmentationSymbols()
			}
			finishPass()
		}
	}

	return nil
}

// clearVisited clears visited flags for new bit plane
func (e *ebcotDecoder) clearVisited() {
	for y := 1; y <= e.height; y++ {
		for x := 1; x <= e.width; x++ {
			e.state[y][x] &^= flagVisited
		}
	}
}

// significancePropagationPass - Pass 1
// Processes coefficients that have at least one significant neighbor
// but are not yet significant themselves
func (e *ebcotDecoder) significancePropagationPass(bp int, subbandType SubbandType) {
	vsc := e.isVertCausal()
	// Process in stripe-oriented order (4 rows at a time, column by column)
	for stripe := 0; stripe < (e.height+3)/4; stripe++ {
		y0 := stripe * 4
		y1 := min(y0+4, e.height)

		for x := 0; x < e.width; x++ {
			for y := y0; y < y1; y++ {
				// Index with border offset
				yy := y + 1
				xx := x + 1

				// VSC flags: first row of stripe blocks north propagation,
				// last row of stripe masks south neighbors in context
				vscFirstRow := vsc && (y%4 == 0)
				vscLastRow := vsc && (y%4 == 3)

				// Skip if already significant
				if e.state[yy][xx]&flagSignificant != 0 {
					continue
				}

				// Check if has significant neighbor
				if !e.hasSignificantNeighbor(xx, yy, vscLastRow) {
					continue
				}

				// Mark as visited
				e.state[yy][xx] |= flagVisited

				// Decode significance bit
				ctx := e.getSigContext(xx, yy, subbandType, vscLastRow)
				sig := e.mq.Decode(ctx)

				if sig != 0 {
					// Becomes significant - set initial magnitude
					e.setSignificant(xx, yy, bp, vscFirstRow)

					// Decode sign
					signCtx, xorBit := e.getSignContext(xx, yy, vscLastRow)
					signBit := e.mq.Decode(signCtx)
					signBit ^= xorBit // XOR with prediction

					if signBit != 0 {
						e.state[yy][xx] |= flagSign
						// Apply sign directly to data (OpenJPEG style)
						e.data[yy][xx] = -e.data[yy][xx]
					}
				}
			}
		}
	}
}

// magnitudeRefinementPass - Pass 2
// Refines coefficients that were significant before this bit plane
// CRITICAL: Must use stripe-based scanning order (4 rows at a time, column by column)
// to match the order in which bits were encoded. Using row-major order causes
// the MQ decoder to output bits for wrong coefficients.
func (e *ebcotDecoder) magnitudeRefinementPass(bp int) {
	vsc := e.isVertCausal()
	poshalf := int32(1) << uint(bp)

	// Process in stripe-oriented order (4 rows at a time, column by column)
	for stripe := 0; stripe < (e.height+3)/4; stripe++ {
		y0 := stripe * 4
		y1 := min(y0+4, e.height)

		for x := 0; x < e.width; x++ {
			for y := y0; y < y1; y++ {
				yy := y + 1
				xx := x + 1

				if e.state[yy][xx]&flagSignificant == 0 {
					continue
				}
				if e.state[yy][xx]&flagVisited != 0 {
					continue
				}

				// VSC: mask south neighbors when at last row of stripe
				vscLastRow := vsc && (y%4 == 3)

				// Determine context (14/15/16) based on refinement state and neighbors
				var ctx int
				hasNeighbor := e.hasSignificantNeighbor(xx, yy, vscLastRow)
				isFirstRefinement := e.state[yy][xx]&flagRefined == 0

				if !isFirstRefinement {
					ctx = 16
				} else if hasNeighbor {
					ctx = ctxMagOther // 15
				} else {
					ctx = ctxMagFirst // 14
				}

				if isFirstRefinement {
					e.state[yy][xx] |= flagRefined
				}

				bit := e.mq.Decode(ctx)

				isNegative := e.data[yy][xx] < 0
				if (bit != 0) != isNegative {
					e.data[yy][xx] += poshalf
				} else {
					e.data[yy][xx] -= poshalf
				}
			}
		}
	}
}

// significancePropagationPassRaw - Raw (bypass) version of significance propagation pass
// Used when BYPASS mode is enabled for lower bitplanes.
// Instead of MQ arithmetic coding, reads raw bits directly from the bitstream.
// Per OpenJPEG t1.c opj_t1_dec_sigpass_raw: reads 1 bit for significance,
// and if significant, reads 1 more bit for sign.
func (e *ebcotDecoder) significancePropagationPassRaw(bp int, subbandType SubbandType) {
	vsc := e.isVertCausal()
	// Process in stripe-oriented order (4 rows at a time, column by column)
	for stripe := 0; stripe < (e.height+3)/4; stripe++ {
		y0 := stripe * 4
		y1 := min(y0+4, e.height)

		for x := 0; x < e.width; x++ {
			for y := y0; y < y1; y++ {
				yy := y + 1
				xx := x + 1

				vscFirstRow := vsc && (y%4 == 0)
				vscLastRow := vsc && (y%4 == 3)

				if e.state[yy][xx]&flagSignificant != 0 {
					continue
				}
				if !e.hasSignificantNeighbor(xx, yy, vscLastRow) {
					continue
				}

				e.state[yy][xx] |= flagVisited

				sig := e.mq.DecodeBitRaw()

				if sig != 0 {
					e.setSignificant(xx, yy, bp, vscFirstRow)

					signBit := e.mq.DecodeBitRaw()

					if signBit != 0 {
						e.state[yy][xx] |= flagSign
						e.data[yy][xx] = -e.data[yy][xx]
					}
				}
			}
		}
	}
}

// magnitudeRefinementPassRaw - Raw (bypass) version of magnitude refinement pass
// Used when BYPASS mode is enabled for lower bitplanes.
// Per OpenJPEG t1.c opj_t1_dec_refpass_raw: reads 1 raw bit for refinement.
func (e *ebcotDecoder) magnitudeRefinementPassRaw(bp int) {
	poshalf := int32(1) << uint(bp)

	// Process in stripe-oriented order (4 rows at a time, column by column)
	for stripe := 0; stripe < (e.height+3)/4; stripe++ {
		y0 := stripe * 4
		y1 := min(y0+4, e.height)

		for x := 0; x < e.width; x++ {
			for y := y0; y < y1; y++ {
				// Index with border offset
				yy := y + 1
				xx := x + 1

				// Skip if not significant
				if e.state[yy][xx]&flagSignificant == 0 {
					continue
				}

				// Skip if visited in current pass (already processed by sig_prop)
				if e.state[yy][xx]&flagVisited != 0 {
					continue
				}

				// Mark as refined (same as MQ version)
				isFirstRefinement := e.state[yy][xx]&flagRefined == 0
				if isFirstRefinement {
					e.state[yy][xx] |= flagRefined
				}

				// Decode refinement bit (raw)
				bit := e.mq.DecodeBitRaw()

				// Apply refinement (same logic as MQ version)
				isNegative := e.data[yy][xx] < 0
				if (bit != 0) != isNegative {
					e.data[yy][xx] += poshalf
				} else {
					e.data[yy][xx] -= poshalf
				}
			}
		}
	}
}

// cleanupPass - Pass 3
// Processes remaining coefficients not yet processed
// Uses run-length mode for efficiency when 4 consecutive coefficients
// have no significant neighbors
func (e *ebcotDecoder) cleanupPass(bp int, subbandType SubbandType) {
	vsc := e.isVertCausal()
	// Process in stripe-oriented order
	for stripe := 0; stripe < (e.height+3)/4; stripe++ {
		y0 := stripe * 4
		y1 := min(y0+4, e.height)

		for x := 0; x < e.width; x++ {
			xx := x + 1
			stateSnapshot := [4]uint8{}
			for i := 0; i < 4 && y0+i < e.height; i++ {
				yy := y0 + i + 1
				stateSnapshot[i] = e.state[yy][xx]
			}

			runMode := e.canUseRunModeFromSnapshot(y1-y0, stateSnapshot[:])

			if runMode {
				aggBit := e.mq.Decode(ctxAggZero)

				if aggBit == 0 {
					for i := 0; i < 4 && y0+i < e.height; i++ {
						e.state[y0+i+1][x+1] |= flagVisited
					}
					continue
				}

				bit1 := e.mq.Decode(ctxUniform)
				bit0 := e.mq.Decode(ctxUniform)
				runLen := (bit1 << 1) | bit0

				for i := 0; i < 4 && y0+i < e.height; i++ {
					yy := y0 + i + 1
					xx := x + 1
					y := y0 + i

					vscFirstRow := vsc && (y%4 == 0)
					vscLastRow := vsc && (y%4 == 3)

					if e.state[yy][xx]&flagVisited != 0 {
						continue
					}

					if i < runLen {
						e.state[yy][xx] |= flagVisited
						continue
					}

					if i == runLen {
						if e.state[yy][xx]&flagSignificant != 0 {
							e.state[yy][xx] |= flagVisited
							continue
						}
						e.setSignificant(xx, yy, bp, vscFirstRow)
						e.state[yy][xx] |= flagVisited

						signCtx, xorBit := e.getSignContext(xx, yy, vscLastRow)
						signBit := e.mq.Decode(signCtx)
						signBit ^= xorBit

						if signBit != 0 {
							e.state[yy][xx] |= flagSign
							e.data[yy][xx] = -e.data[yy][xx]
						}
					} else {
						e.cleanupDecodeOne(xx, yy, bp, subbandType, vscFirstRow, vscLastRow)
					}
				}
			} else {
				for i := 0; i < 4 && y0+i < e.height; i++ {
					yy := y0 + i + 1
					xx := x + 1
					y := y0 + i

					if e.state[yy][xx]&flagVisited != 0 {
						continue
					}

					vscFirstRow := vsc && (y%4 == 0)
					vscLastRow := vsc && (y%4 == 3)

					e.cleanupDecodeOne(xx, yy, bp, subbandType, vscFirstRow, vscLastRow)
				}
			}
		}
	}
}

// cleanupDecodeOne decodes one coefficient in cleanup pass
func (e *ebcotDecoder) cleanupDecodeOne(x, y int, bp int, subbandType SubbandType, vscFirstRow, vscLastRow bool) {
	e.state[y][x] |= flagVisited

	if e.state[y][x]&flagSignificant != 0 {
		return
	}

	ctx := e.getSigContext(x, y, subbandType, vscLastRow)
	sig := e.mq.Decode(ctx)

	if sig != 0 {
		e.setSignificant(x, y, bp, vscFirstRow)

		signCtx, xorBit := e.getSignContext(x, y, vscLastRow)
		signBit := e.mq.Decode(signCtx)
		signBit ^= xorBit

		if signBit != 0 {
			e.state[y][x] |= flagSign
			e.data[y][x] = -e.data[y][x]
		}
	}
}

// canUseRunModeFromSnapshot checks if 4 consecutive coefficients can use run-length mode.
// CRITICAL: This function uses a pre-captured snapshot of the ENTIRE state byte
// taken at the START of processing this stripe column, before any setSignificant() calls.
//
// This matches OpenJPEG's behavior exactly: "if (flags == 0)"
// They capture the flag state once at stripe entry and use that initial snapshot.
//
// Requirements for run-mode (per OpenJPEG):
// - count >= 4 (full stripe)
// - flags == 0 for all 4 coefficients (no significant, no visited, no neighbor flags)
func (e *ebcotDecoder) canUseRunModeFromSnapshot(count int, stateSnapshot []uint8) bool {
	if count < 4 {
		return false
	}

	// Check all 4 positions using the pre-captured state snapshot
	// Per OpenJPEG: run-mode requires flags == 0 for all coefficients in the stripe
	for i := range 4 {
		if stateSnapshot[i] != 0 {
			return false
		}
	}

	return true
}

// Context determination functions

// getSigContext determines significance context (0-8)
// Based on number and orientation of significant neighbors
// This follows OpenJPEG's t1_init_ctxno_zc exactly
func (e *ebcotDecoder) getSigContext(x, y int, subbandType SubbandType, vscLastRow bool) int {
	h, v, d := e.countSigNeighbors(x, y, vscLastRow)

	// OpenJPEG orient mapping:
	// - orient 0,1 (LL/LH): Check h first, then v, then d
	// - orient 2 (HL): Swap h and v, then use same logic as orient 0/1
	// - orient 3 (HH): Use hv=h+v combined, d as primary discriminator

	switch subbandType {
	case SubbandHH:
		// HH subband (orient 3): diagonal neighbors are primary
		hv := h + v
		if d == 0 {
			if hv == 0 {
				return 0
			}
			if hv == 1 {
				return 1
			}
			return 2 // hv >= 2
		}
		if d == 1 {
			if hv == 0 {
				return 3
			}
			if hv == 1 {
				return 4
			}
			return 5 // hv >= 2
		}
		if d == 2 {
			if hv == 0 {
				return 6
			}
			return 7 // hv >= 1
		}
		return 8 // d >= 3

	case SubbandHL:
		// HL subband: swap h and v per ITU-T T.800 Table D.1
		// For HL, v is the primary discriminator (roles of h and v exchanged)
		h, v = v, h
		fallthrough

	case SubbandLL, SubbandLH:
		// LL and LH subbands: h is the primary discriminator per ITU-T T.800 Table D.1
		if h == 0 {
			if v == 0 {
				if d == 0 {
					return 0
				}
				if d == 1 {
					return 1
				}
				return 2 // d >= 2
			}
			if v == 1 {
				return 3
			}
			return 4 // v >= 2
		}
		if h == 1 {
			if v == 0 {
				if d == 0 {
					return 5
				}
				return 6 // d >= 1
			}
			return 7 // v >= 1
		}
		return 8 // h >= 2
	}

	return 0
}

// getSignContext determines sign context (9-13) and XOR prediction bit
// Returns (context, xorBit)
// Uses OpenJPEG's pre-computed lookup tables for exact bit-compatible decoding
func (e *ebcotDecoder) getSignContext(x, y int, vscLastRow bool) (int, int) {
	// Build 8-bit LUT index from neighbor state
	// Format matches OpenJPEG's t1_getctxtno_sc_or_spb_index:
	//   bit 0: West sign (lutSgnW)
	//   bit 1: North significance (lutSigN)
	//   bit 2: East sign (lutSgnE)
	//   bit 3: West significance (lutSigW)
	//   bit 4: North sign (lutSgnN)
	//   bit 5: East significance (lutSigE)
	//   bit 6: South sign (lutSgnS)
	//   bit 7: South significance (lutSigS)

	var lu int

	// West neighbor (x-1)
	westState := e.state[y][x-1]
	if westState&flagSignificant != 0 {
		lu |= lutSigW
		if westState&flagSign != 0 {
			lu |= lutSgnW
		}
	}

	// East neighbor (x+1)
	eastState := e.state[y][x+1]
	if eastState&flagSignificant != 0 {
		lu |= lutSigE
		if eastState&flagSign != 0 {
			lu |= lutSgnE
		}
	}

	// North neighbor (y-1)
	northState := e.state[y-1][x]
	if northState&flagSignificant != 0 {
		lu |= lutSigN
		if northState&flagSign != 0 {
			lu |= lutSgnN
		}
	}

	// South neighbor (y+1) — excluded when VSC at last row of stripe
	if !vscLastRow {
		southState := e.state[y+1][x]
		if southState&flagSignificant != 0 {
			lu |= lutSigS
			if southState&flagSign != 0 {
				lu |= lutSgnS
			}
		}
	}

	// Look up context and sign prediction from OpenJPEG tables
	ctx := int(lutCtxnoSC[lu])
	xorBit := int(lutSPB[lu])

	return ctx, xorBit
}

// getMagContext determines magnitude refinement context (14-15)
func (e *ebcotDecoder) getMagContext(x, y int, vscLastRow bool) int {
	// Check if any significant neighbors
	if e.hasSignificantNeighbor(x, y, vscLastRow) {
		return ctxMagOther
	}
	return ctxMagFirst
}

// Helper functions

// hasSignificantNeighbor checks if coefficient has any significant neighbor.
// When vscLastRow is false, uses the cached flagNeighborSig for O(1) lookup.
// When vscLastRow is true (VSC at last row of stripe), checks neighbors explicitly
// excluding south, southwest, and southeast (which are in the next stripe).
func (e *ebcotDecoder) hasSignificantNeighbor(x, y int, vscLastRow bool) bool {
	if !vscLastRow {
		return e.state[y][x]&flagNeighborSig != 0
	}
	// VSC last row: check all neighbors except south row
	return e.state[y][x-1]&flagSignificant != 0 || // West
		e.state[y][x+1]&flagSignificant != 0 || // East
		e.state[y-1][x]&flagSignificant != 0 || // North
		e.state[y-1][x-1]&flagSignificant != 0 || // Northwest
		e.state[y-1][x+1]&flagSignificant != 0 // Northeast
}

// setSignificant marks coefficient as significant and sets initial magnitude.
// CRITICAL: Also updates all 8 neighbors to set flagNeighborSig, which is required
// for correct context calculation in subsequent passes.
//
// When vscFirstRow is true (VSC enabled AND coefficient at first row of stripe),
// north neighbor propagation is skipped. This matches OpenJPEG's
// opj_t1_update_flags_macro: "if (ci == 0U && !(vsc))" which only updates
// north neighbors when NOT at the first row of a stripe with VSC enabled.
//
// Per OpenJPEG t1.c: Use "oneplushalf" scheme for ALL coefficient reconstruction
// (both lossless and lossy). Initial value = one | half = 1.5 * 2^(bp-1).
// This places the coefficient at the midpoint of the quantization bin.
// Subsequent magnitude refinement passes add/subtract half to narrow the interval.
func (e *ebcotDecoder) setSignificant(x, y int, bp int, vscFirstRow bool) {
	if e.state[y][x]&flagSignificant != 0 {
		return // Already significant - don't overwrite
	}

	e.state[y][x] |= flagSignificant

	// Oneplushalf midpoint reconstruction (matches OpenJPEG t1.c)
	one := int32(1) << uint(bp+1)
	half := int32(1) << uint(bp)
	e.data[y][x] = one | half

	// Propagate neighbor significance to neighbors.
	// Matches OpenJPEG's opj_t1_update_flags_macro.

	// Horizontal neighbors (always propagated)
	e.state[y][x-1] |= flagNeighborSig // West
	e.state[y][x+1] |= flagNeighborSig // East

	// South neighbors (always propagated)
	e.state[y+1][x] |= flagNeighborSig   // South
	e.state[y+1][x-1] |= flagNeighborSig // Southwest
	e.state[y+1][x+1] |= flagNeighborSig // Southeast

	// North neighbors: skip when VSC enabled at first row of stripe.
	// Per OpenJPEG: "if (ci == 0U && !(vsc))" — only propagate north when
	// NOT at first row of stripe with VSC. This prevents the previous stripe's
	// last row from seeing this stripe's first row as a significant south neighbor.
	if !vscFirstRow {
		e.state[y-1][x] |= flagNeighborSig   // North
		e.state[y-1][x-1] |= flagNeighborSig // Northwest
		e.state[y-1][x+1] |= flagNeighborSig // Northeast
	}
}

// countSigNeighbors counts significant neighbors in three categories.
// Returns (horizontal, vertical, diagonal).
// When vscLastRow is true (VSC at last row of stripe), south neighbors are excluded.
func (e *ebcotDecoder) countSigNeighbors(x, y int, vscLastRow bool) (h, v, d int) {
	// Horizontal neighbors
	if e.state[y][x-1]&flagSignificant != 0 {
		h++
	}
	if e.state[y][x+1]&flagSignificant != 0 {
		h++
	}

	// Vertical neighbors
	if e.state[y-1][x]&flagSignificant != 0 {
		v++
	}
	if !vscLastRow {
		if e.state[y+1][x]&flagSignificant != 0 {
			v++
		}
	}

	// Diagonal neighbors (north always, south only if not VSC last row)
	if e.state[y-1][x-1]&flagSignificant != 0 {
		d++
	}
	if e.state[y-1][x+1]&flagSignificant != 0 {
		d++
	}
	if !vscLastRow {
		if e.state[y+1][x-1]&flagSignificant != 0 {
			d++
		}
		if e.state[y+1][x+1]&flagSignificant != 0 {
			d++
		}
	}

	return
}

// readSegmentationSymbols reads the segmentation symbols after a cleanup pass
// when the SEGSYM flag (0x20) is set in CodeBlockStyle.
//
// Per ITU-T T.800 Section D.4.1 and OpenJPEG t1.c:
// When segmentation symbols are enabled, 4 bits with value 0xA (binary 1010)
// are encoded after each cleanup pass using the uniform context (context 18).
// This helps detect errors in the encoded data stream.
//
// The decoder must read these 4 bits to stay synchronized with the MQ state.
// If the value is not 0xA, it indicates a decoding error, but we continue
// anyway as per OpenJPEG behavior (it logs a warning but doesn't fail).
func (e *ebcotDecoder) readSegmentationSymbols() {
	if e.codeBlockStyle&cbstySegSymbols == 0 {
		return
	}

	// Read 4 bits using uniform context, MSB-first (like OpenJPEG t1.c:1349-1355)
	// Expected value is 0xA = 1010 binary
	_ = e.mq.Decode(ctxUniform)
	_ = e.mq.Decode(ctxUniform)
	_ = e.mq.Decode(ctxUniform)
	_ = e.mq.Decode(ctxUniform)
}
