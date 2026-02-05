package jpeg2000

// MQ Arithmetic Decoder for JPEG2000 EBCOT
//
// This package implements the MQ context-adaptive binary arithmetic decoder
// as specified in ITU-T T.800 (JPEG2000) Annex C. The MQ coder is used in
// JPEG2000's Embedded Block Coding with Optimized Truncation (EBCOT) algorithm
// for entropy coding of wavelet coefficients.
//
// Key Features:
// - Context-adaptive probability estimation with 47 states
// - 19 independent coding contexts for different coefficient types
// - Automatic interval renormalization
// - Byte stuffing for marker detection (0xFF handling)
// - Raw bit decoding (bypass mode) for cleanup passes
//
// The MQ decoder maintains:
// - A: probability interval (16-bit, always >= 0x8000 after operations)
// - C: code register (28-bit value from bitstream)
// - CT: bit counter for renormalization
// - contexts: 19 independent context states with probability estimates
//
// Performance: ~4.5 ns/op for context-based decode, ~1.2 ns/op for raw bits
// (zero allocations in both cases)

// mqDecoder implements the MQ context-adaptive binary arithmetic coder
// as specified in ITU-T T.800 (JPEG2000) Annex C.
type mqDecoder struct {
	// Coder state
	A  uint32 // Probability interval (16-bit, but use 32 for operations)
	C  uint32 // Code register (28 bits used)
	CT int    // Counter for renormalization (bits available in C)

	// Input stream
	data []byte
	pos  int

	// Raw decoder state (for BYPASS mode)
	// Raw decoding uses a separate byte stream with simpler 0xFF handling
	rawC   uint32 // Current byte being read (shifted left as bits consumed)
	rawCT  int    // Bits remaining in rawC
	rawPos int    // Position in data for raw decoding

	// End-of-byte-stream tracking (per OpenJPEG)
	// Counts how many times we've hit a 0xFF followed by marker (>0x8F)
	// Used for PTERM validation - standard expects 2-3 such markers
	endOfByteStreamCounter int

	// Context states (19 contexts for EBCOT)
	contexts [19]mqContext
}

type mqContext struct {
	index int // Index into probability table (0-46)
	mps   int // Most probable symbol (0 or 1)
}

// mqProbEntry represents one entry in the MQ probability estimation table
type mqProbEntry struct {
	qe        uint16 // Probability estimate value
	nmps      int    // Next state if MPS is decoded
	nlps      int    // Next state if LPS is decoded
	switchMPS bool   // Whether to switch MPS value when transitioning to nlps
}

// MQ probability table from JPEG2000 spec Table D.2
// Each entry: (Qe value, next state if MPS, next state if LPS, switch MPS flag)
var mqProbTable = [47]mqProbEntry{
	{0x5601, 1, 1, true},    // 0
	{0x3401, 2, 6, false},   // 1
	{0x1801, 3, 9, false},   // 2
	{0x0AC1, 4, 12, false},  // 3
	{0x0521, 5, 29, false},  // 4
	{0x0221, 38, 33, false}, // 5
	{0x5601, 7, 6, true},    // 6
	{0x5401, 8, 14, false},  // 7
	{0x4801, 9, 14, false},  // 8
	{0x3801, 10, 14, false}, // 9
	{0x3001, 11, 17, false}, // 10
	{0x2401, 12, 18, false}, // 11
	{0x1C01, 13, 20, false}, // 12
	{0x1601, 29, 21, false}, // 13
	{0x5601, 15, 14, true},  // 14
	{0x5401, 16, 14, false}, // 15
	{0x5101, 17, 15, false}, // 16
	{0x4801, 18, 16, false}, // 17
	{0x3801, 19, 17, false}, // 18
	{0x3401, 20, 18, false}, // 19
	{0x3001, 21, 19, false}, // 20
	{0x2801, 22, 19, false}, // 21
	{0x2401, 23, 20, false}, // 22
	{0x2201, 24, 21, false}, // 23
	{0x1C01, 25, 22, false}, // 24
	{0x1801, 26, 23, false}, // 25
	{0x1601, 27, 24, false}, // 26
	{0x1401, 28, 25, false}, // 27
	{0x1201, 29, 26, false}, // 28
	{0x1101, 30, 27, false}, // 29
	{0x0AC1, 31, 28, false}, // 30
	{0x09C1, 32, 29, false}, // 31
	{0x08A1, 33, 30, false}, // 32
	{0x0521, 34, 31, false}, // 33
	{0x0441, 35, 32, false}, // 34
	{0x02A1, 36, 33, false}, // 35
	{0x0221, 37, 34, false}, // 36
	{0x0141, 38, 35, false}, // 37
	{0x0111, 39, 36, false}, // 38
	{0x0085, 40, 37, false}, // 39
	{0x0049, 41, 38, false}, // 40
	{0x0025, 42, 39, false}, // 41
	{0x0015, 43, 40, false}, // 42
	{0x0009, 44, 41, false}, // 43
	{0x0005, 45, 42, false}, // 44
	{0x0001, 45, 43, false}, // 45
	{0x5601, 46, 46, false}, // 46 (uniform context)
}

// newMQDecoder creates a new MQ decoder initialized with the given data
func newMQDecoder(data []byte) *mqDecoder {
	mq := &mqDecoder{
		data: data,
		pos:  0,
	}
	mq.ResetContexts()
	mq.initDec()
	return mq
}

// initDec initializes the decoder for regular (non-ERTERM) mode.
// Implements INITDEC procedure from JPEG2000 spec C.3.5
//
// This matches OpenJPEG's opj_mqc_init_dec:
// 1. Read first byte (data[0]) into C at bits [16-23], pos stays at 0
// 2. Call bytein which:
//    - Looks at current byte (data[0]) to check for 0xFF
//    - Adds the NEXT byte (data[1]) to C at bits [8-15]
//    - Advances pos to 1
// 3. Shift C left by 7, decrement CT by 7
// 4. Set A = 0x8000
//
// After init: C contains byte0 at [23-16] and byte1 at [14-7] (after shift)
func (mq *mqDecoder) initDec() {
	// Initialize interval
	mq.A = 0x8000

	// Read first byte directly to position [16-23]
	// pos stays at 0 - bytein will look at data[0] but add data[1]
	if mq.pos < len(mq.data) {
		mq.C = uint32(mq.data[mq.pos]) << 16
	} else {
		mq.C = 0xFF << 16
	}
	mq.CT = 0

	// Call bytein - looks at data[pos] for 0xFF check, adds data[pos+1] to C
	mq.bytein()

	// Shift and adjust counter
	mq.C <<= 7
	mq.CT -= 7
}

// Decode decodes one bit using the specified context (0-18)
// Implements DECODE procedure from JPEG2000 spec C.3.2
func (mq *mqDecoder) Decode(ctx int) int {
	if ctx < 0 || ctx >= len(mq.contexts) {
		// Invalid context, return 0
		return 0
	}

	context := &mq.contexts[ctx]
	entry := &mqProbTable[context.index]
	qe := uint32(entry.qe)

	// Partition interval
	mq.A -= qe

	// Extract the high bits of C for comparison
	// Per OpenJPEG mqc_inl.h: if ((c >> 16) < (*curctx)->qeval)
	// The C register comparison uses 16-bit shift to align with qe values
	chigh := mq.C >> 16

	// Check which sub-interval C falls into
	// Per OpenJPEG mqc_inl.h: if ((c >> 16) < qe) → LPS path
	if chigh < qe {
		// LPS sub-interval (C in [0, Qe))
		// LPS_EXCHANGE
		if mq.A < qe {
			// Actually MPS
			context.index = entry.nmps
			mq.A = qe
			d := context.mps
			mq.renormalize()
			return d
		}
		// LPS
		mq.A = qe
		d := 1 - context.mps
		if entry.switchMPS {
			context.mps = 1 - context.mps
		}
		context.index = entry.nlps
		mq.renormalize()
		return d
	}

	// MPS sub-interval (C in [Qe, A))
	// Per OpenJPEG mqc_inl.h: c -= (*curctx)->qeval << 16
	mq.C -= qe << 16

	// Per OpenJPEG mqc_inl.h MPS_EXCHANGE logic:
	// - First check if renormalization needed: (a & 0x8000) == 0
	// - If renorm needed, check exchange condition: a < qeval
	// These are separate conditions that OpenJPEG checks in this order
	if mq.A < 0x8000 {
		// Need renormalization - check if exchange is also needed
		if mq.A < qe {
			// MPS_EXCHANGE: A < qe means we actually output LPS
			// Note: Do NOT set mq.A = qe here (that's only in LPS_EXCHANGE)
			d := 1 - context.mps
			if entry.switchMPS {
				context.mps = 1 - context.mps
			}
			context.index = entry.nlps
			mq.renormalize()
			return d
		}
		// MPS with renormalization needed (no exchange)
		context.index = entry.nmps
		d := context.mps
		mq.renormalize()
		return d
	}
	// No exchange needed, return MPS
	// Per OpenJPEG mqc_inl.h: In fast MPS path, NO context update happens
	// The context only transitions when exchange macros are called (A < 0x8000)
	return context.mps
}

// DecodeBitRaw decodes a raw bit (bypass mode, no context)
// Per OpenJPEG's opj_mqc_raw_decode, raw bits are read MSB-first from
// a separate byte stream that handles 0xFF byte stuffing.
//
// In JPEG2000 BYPASS mode, raw coding passes read bits directly without
// MQ arithmetic coding. The bit stream still follows 0xFF byte stuffing rules:
// after 0xFF, only 7 bits are read from the next byte (the MSB is skipped).
// DecodeBitRaw decodes one raw bit from the bypass bitstream.
// Matches OpenJPEG's opj_mqc_raw_decode exactly:
//   - Bits are extracted MSB-first using a shift counter (ct)
//   - When ct reaches 0, the next byte is read
//   - After a 0xFF byte, the next byte uses only 7 bits (stuffing)
//   - Marker detection: if next byte > 0x8F after 0xFF, stay on 0xFF
func (mq *mqDecoder) DecodeBitRaw() int {
	if mq.rawCT == 0 {
		// Need to read next byte
		if mq.rawC == 0xFF {
			// Previous byte was 0xFF: check for marker or stuffing
			if mq.rawPos < len(mq.data) && mq.data[mq.rawPos] > 0x8F {
				// Marker detected: keep c as 0xFF, give 8 bits
				mq.rawC = 0xFF
				mq.rawCT = 8
			} else if mq.rawPos < len(mq.data) {
				// Stuffed byte: read next byte with only 7 data bits
				mq.rawC = uint32(mq.data[mq.rawPos])
				mq.rawPos++
				mq.rawCT = 7
			} else {
				// End of data after 0xFF
				mq.rawC = 0xFF
				mq.rawCT = 8
			}
		} else {
			// Normal case: read next byte with 8 bits
			if mq.rawPos < len(mq.data) {
				mq.rawC = uint32(mq.data[mq.rawPos])
				mq.rawPos++
				mq.rawCT = 8
			} else {
				// End of data
				mq.rawC = 0xFF
				mq.rawCT = 8
			}
		}
	}
	mq.rawCT--
	d := int((mq.rawC >> mq.rawCT) & 1)
	return d
}

// InitRawDecoder initializes the raw bit decoder for BYPASS mode.
// Must be called before the first raw coding pass to set up byte alignment.
// Per OpenJPEG, raw decoding starts from a fresh byte boundary.
func (mq *mqDecoder) InitRawDecoder() {
	// Start raw decoding from the current MQ position
	// The raw decoder uses its own position and state
	mq.rawPos = mq.pos
	mq.rawC = 0
	mq.rawCT = 0
}

// SetRawData sets the data buffer for raw (bypass) decoding without MQ initialization.
// Used for bypass-segmented mode where each RAW segment has its own data slice.
// Unlike InitSegment, this does NOT call initDec() which would consume bytes via MQ
// bytein before the raw decoder can access them.
func (mq *mqDecoder) SetRawData(data []byte) {
	mq.data = data
	mq.rawPos = 0
	mq.rawC = 0
	mq.rawCT = 0
}

// SyncFromRaw synchronizes the MQ position after raw coding passes.
// Must be called after raw passes complete to update MQ state for next cleanup.
func (mq *mqDecoder) SyncFromRaw() {
	// Update MQ position to where raw decoder left off
	mq.pos = mq.rawPos
}

// renormalize performs interval renormalization after LPS or when A < 0x8000
// Implements RENORMD procedure from JPEG2000 spec C.3.3
func (mq *mqDecoder) renormalize() {
	for mq.A < 0x8000 {
		if mq.CT == 0 {
			mq.bytein()
		}
		mq.A <<= 1
		mq.C <<= 1
		mq.CT--
	}
}

// bytein reads next byte into code register
// Implements BYTEIN procedure from JPEG2000 spec C.3.4
//
// This implementation matches OpenJPEG's opj_mqc_bytein_macro exactly:
//
//	l_c = *(mqc->bp + 1);           // Read NEXT byte first (look-ahead)
//	if (*mqc->bp == 0xff) {         // Check CURRENT byte for 0xFF
//	    if (l_c > 0x8f) {           // Marker detected
//	        c += 0xff00; ct = 8;    // Don't advance bp
//	    } else {                    // Stuffed byte
//	        mqc->bp++;
//	        c += l_c << 9; ct = 7;  // Add next byte shifted by 9
//	    }
//	} else {                        // Normal byte
//	    mqc->bp++;
//	    c += l_c << 8; ct = 8;      // Add NEXT byte (l_c), not current!
//	}
//
// Key insight: OpenJPEG adds the NEXT byte (*(bp+1)) to C, not the current byte.
func (mq *mqDecoder) bytein() {
	// Check if we can read the current byte
	if mq.pos >= len(mq.data) {
		// No more data available, fill with 0xFF (per spec)
		mq.C += 0xFF << 8
		mq.CT = 8
		mq.endOfByteStreamCounter++
		return
	}

	// Read NEXT byte first (look-ahead), like OpenJPEG: l_c = *(bp + 1)
	var nextByte byte = 0xFF // Default if no next byte
	if mq.pos+1 < len(mq.data) {
		nextByte = mq.data[mq.pos+1]
	}

	// Check CURRENT byte for 0xFF marker prefix
	curByte := mq.data[mq.pos]

	if curByte == 0xFF {
		// Current byte is 0xFF - check next byte for marker vs stuffing
		if nextByte > 0x8F {
			// Marker segment detected (0xFF followed by >0x8F)
			// Per ITU-T T.800 C.3.4 and OpenJPEG: when marker is detected,
			// the decoder should act as if an infinite sequence of 0xFF follows.
			// Do NOT advance pos - stay pointing at the 0xFF
			mq.C += 0xFF << 8
			mq.CT = 8
			mq.endOfByteStreamCounter++
			return
		}

		// 0xFF followed by value <= 0x8F (stuffed zero bit)
		// Advance past the 0xFF, add the next byte shifted by 9 (7 bits available)
		mq.pos++
		mq.C += uint32(nextByte) << 9
		mq.CT = 7
	} else {
		// Normal byte (not 0xFF): advance and add the NEXT byte to C
		mq.pos++
		mq.C += uint32(nextByte) << 8
		mq.CT = 8
	}
}

// Position returns the current byte position in the input stream
func (mq *mqDecoder) Position() int {
	return mq.pos
}

// Reset resets decoder for new code block (full reset including contexts)
func (mq *mqDecoder) Reset(data []byte) {
	mq.data = data
	mq.ResetContexts()
	// Reset position before init (initDec will advance it)
	mq.pos = 0
	mq.endOfByteStreamCounter = 0
	// Reset raw decoder state
	mq.rawC = 0
	mq.rawCT = 0
	mq.rawPos = 0
	mq.initDec()
}

// InitSegment initializes the decoder for a new segment WITHOUT resetting contexts.
// This is used in ERTERM mode where each pass has its own data segment, but
// context probabilities should persist across segments (per OpenJPEG behavior).
//
// OpenJPEG uses the same opj_mqc_init_dec for both initial and segment restarts.
// The initialization is: c = *bp << 16, bytein(), c <<= 7, ct -= 7, a = 0x8000
//
// Contexts are only reset:
// 1. Once at the start of the code block (before any segments)
// 2. Between passes if the RESET flag (0x02) is set
func (mq *mqDecoder) InitSegment(data []byte) {
	mq.data = data
	mq.pos = 0
	mq.endOfByteStreamCounter = 0
	mq.initDec() // Same initialization as OpenJPEG's opj_mqc_init_dec
	// Note: contexts are NOT reset - this matches OpenJPEG behavior
}

// ResetContexts resets all contexts to initial state
// Per OpenJPEG mqc.c:313-315, contexts have specific initial states:
//   - Context 0 (ZC first): state 4
//   - Context 17 (AGG): state 3
//   - Context 18 (UNI): state 46 (uniform probability)
//   - All others: state 0, MPS = 0
func (mq *mqDecoder) ResetContexts() {
	for i := range mq.contexts {
		mq.contexts[i].index = 0
		mq.contexts[i].mps = 0
	}
	// Set non-zero initial states per OpenJPEG mqc.c opj_mqc_resetstates
	if len(mq.contexts) > 0 {
		mq.contexts[0].index = 4 // T1_CTXNO_ZC (first significance context)
	}
	if len(mq.contexts) > 17 {
		mq.contexts[17].index = 3 // T1_CTXNO_AGG (aggregation context)
		// Note: MPS stays at 0 per decoder initialization
	}
	if len(mq.contexts) > 18 {
		mq.contexts[18].index = 46 // T1_CTXNO_UNI (uniform context)
	}
}

// FinishSegment finalizes decoding of a segment.
// In ERTERM mode, segments may end with residual bits in the C register.
// This resets the bit counter to force byte alignment on next segment init.
func (mq *mqDecoder) FinishSegment() {
	mq.CT = 0
}
