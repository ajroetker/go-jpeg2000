package jpeg2000

// MQ Arithmetic Encoder for JPEG2000 EBCOT
//
// This file implements the MQ context-adaptive binary arithmetic encoder
// as specified in ITU-T T.800 (JPEG2000) Annex C. The MQ encoder is the
// encoding counterpart to the MQ decoder in mq.go and produces bitstreams
// that the decoder can correctly decode.
//
// The encoder shares the same probability table (mqProbTable) and the same
// 19 coding contexts as the decoder. Context states evolve identically
// during encoding and decoding, ensuring that the decoder can track the
// encoder's probability model.
//
// Key procedures per ITU-T T.800 Annex C:
//   - INITENC (C.2.9): Initialize encoder state
//   - CODEMPS (C.2.7): Encode a most probable symbol
//   - CODELPS (C.2.8): Encode a least probable symbol
//   - RENORME (C.2.5): Renormalization after coding
//   - BYTEOUT (C.2.10): Output byte with 0xFF stuffing
//   - FLUSH (C.2.11): Finalize and emit remaining state

// mqEncoder implements the MQ context-adaptive binary arithmetic encoder
// per ITU-T T.800 Annex C.
type mqEncoder struct {
	// Coder state
	A  uint32 // Probability interval (16-bit range, always >= 0x8000 after renorm)
	C  uint32 // Code register (carries and partial bytes)
	CT int    // Counter for renormalization (bits until next BYTEOUT)

	// Output buffer
	buf []byte // Accumulated output bytes
	bp  int    // Current write position in buf (-1 before first byte)

	// Context states (19 contexts for EBCOT, same as decoder)
	contexts [19]mqContext

	// Raw encoder state (for BYPASS mode)
	rawC  byte // Current byte being assembled
	rawCT int  // Bits remaining in rawC (counts down from 8 or 7)
}

// newMQEncoder creates a new MQ encoder with initialized state.
func newMQEncoder() *mqEncoder {
	mq := &mqEncoder{}
	mq.ResetContexts()
	mq.Reset()
	return mq
}

// Reset reinitializes the encoder state for a new encoding session.
// Implements INITENC procedure from ITU-T T.800 Annex C (C.2.9).
func (mq *mqEncoder) Reset() {
	mq.A = 0x8000
	mq.C = 0
	mq.CT = 12
	// Pre-allocate output buffer; start with reasonable capacity
	mq.buf = mq.buf[:0]
	if cap(mq.buf) < 128 {
		mq.buf = make([]byte, 0, 128)
	}
	mq.bp = -1 // Before first byte
	// Reset raw state
	mq.rawC = 0
	mq.rawCT = 0
}

// ResetContexts resets all contexts to their initial states.
// Must match the decoder's initial states exactly:
//   - Context 0: state 4 (T1_CTXNO_ZC)
//   - Context 17: state 3 (T1_CTXNO_AGG)
//   - Context 18: state 46 (T1_CTXNO_UNI, uniform probability)
//   - All others: state 0, MPS = 0
func (mq *mqEncoder) ResetContexts() {
	for i := range mq.contexts {
		mq.contexts[i].index = 0
		mq.contexts[i].mps = 0
	}
	if len(mq.contexts) > 0 {
		mq.contexts[0].index = 4
	}
	if len(mq.contexts) > 17 {
		mq.contexts[17].index = 3
	}
	if len(mq.contexts) > 18 {
		mq.contexts[18].index = 46
	}
}

// Encode encodes one symbol (0 or 1) using the specified context (0-18).
// Implements the ENCODE procedure from ITU-T T.800 Annex C (C.2.6).
func (mq *mqEncoder) Encode(ctx int, bit int) {
	if ctx < 0 || ctx >= len(mq.contexts) {
		return
	}
	if bit == mq.contexts[ctx].mps {
		mq.codeMPS(ctx)
	} else {
		mq.codeLPS(ctx)
	}
}

// codeMPS encodes a most probable symbol.
// Implements CODEMPS procedure per OpenJPEG's opj_mqc_codemps_macro (mqc_inl.h).
//
// Three paths:
//   - Fast path (A >= 0x8000): C += Qe, no renorm needed
//   - Renorm, no exchange (A >= Qe): C += Qe, renorm
//   - Renorm, exchange (A < Qe): A = Qe (no C change), renorm
func (mq *mqEncoder) codeMPS(ctx int) {
	context := &mq.contexts[ctx]
	entry := &mqProbTable[context.index]
	qe := uint32(entry.qe)

	mq.A -= qe
	if mq.A < 0x8000 {
		// Renormalization needed
		if mq.A < qe {
			// MPS_EXCHANGE: A < Qe, swap intervals. Set A = Qe, no C change.
			mq.A = qe
		} else {
			// No exchange, but renorm needed: C += Qe
			mq.C += qe
		}
		context.index = entry.nmps
		mq.renormEnc()
	} else {
		// Fast path (no renorm needed): C += Qe
		mq.C += qe
	}
}

// codeLPS encodes a least probable symbol.
// Implements CODELPS procedure per OpenJPEG's opj_mqc_codelps_macro (mqc_inl.h).
//
// Two paths:
//   - Normal (A < Qe): C += Qe
//   - Exchange (A >= Qe): A = Qe (no C change)
func (mq *mqEncoder) codeLPS(ctx int) {
	context := &mq.contexts[ctx]
	entry := &mqProbTable[context.index]
	qe := uint32(entry.qe)

	mq.A -= qe
	if mq.A < qe {
		// Normal LPS path: C += Qe
		mq.C += qe
	} else {
		// LPS_EXCHANGE: swap intervals. Set A = Qe, no C change.
		mq.A = qe
	}
	// Switch MPS value if required by table
	if entry.switchMPS {
		context.mps = 1 - context.mps
	}
	context.index = entry.nlps
	mq.renormEnc()
}

// renormEnc performs interval renormalization during encoding.
// Implements RENORME procedure from ITU-T T.800 Annex C (C.2.5).
func (mq *mqEncoder) renormEnc() {
	for mq.A < 0x8000 {
		mq.A <<= 1
		mq.C <<= 1
		mq.CT--
		if mq.CT == 0 {
			mq.byteout()
		}
	}
}

// byteout outputs a byte from the code register with 0xFF stuffing.
// Implements BYTEOUT procedure from ITU-T T.800 Annex C (C.2.10).
//
// The procedure handles three cases:
//  1. Previous byte was 0xFF: output 7 bits (bit stuffing)
//  2. Carry propagation from C: increment previous byte, handle cascading 0xFF
//  3. Normal: output 8 bits
func (mq *mqEncoder) byteout() {
	if mq.bp < 0 {
		// First byte output: allocate position and write
		mq.buf = append(mq.buf, byte(mq.C>>19))
		mq.bp = 0
		mq.C &= 0x7FFFF
		mq.CT = 8
		return
	}

	if mq.buf[mq.bp] == 0xFF {
		// Previous byte is 0xFF: bit stuffing required
		// Output only 7 bits shifted by 20 (instead of 19)
		mq.bp++
		mq.buf = append(mq.buf, byte(mq.C>>20))
		mq.C &= 0xFFFFF
		mq.CT = 7
	} else if mq.C >= 0x8000000 {
		// Carry bit set: propagate carry to previous byte
		mq.buf[mq.bp]++
		if mq.buf[mq.bp] == 0xFF {
			// Carry made previous byte 0xFF: apply stuffing rules
			mq.C &= 0x7FFFFFF
			mq.bp++
			mq.buf = append(mq.buf, byte(mq.C>>20))
			mq.C &= 0xFFFFF
			mq.CT = 7
		} else {
			// Normal output after carry
			mq.bp++
			mq.buf = append(mq.buf, byte(mq.C>>19))
			mq.C &= 0x7FFFF
			mq.CT = 8
		}
	} else {
		// Normal byte output
		mq.bp++
		mq.buf = append(mq.buf, byte(mq.C>>19))
		mq.C &= 0x7FFFF
		mq.CT = 8
	}
}

// EncodeRaw encodes a raw bit (bypass mode, no arithmetic coding).
// Matches the format expected by DecodeBitRaw in the decoder.
//
// Raw bits are packed MSB-first into bytes with 0xFF byte stuffing:
// after writing a 0xFF byte, the next byte uses only 7 data bits
// (the MSB is implicitly zero to prevent accidental marker creation).
func (mq *mqEncoder) EncodeRaw(bit int) {
	if mq.rawCT == 0 {
		// Start a new raw byte
		mq.rawC = 0
		mq.rawCT = 8
	}
	mq.rawCT--
	if bit != 0 {
		mq.rawC |= 1 << mq.rawCT
	}
	if mq.rawCT == 0 {
		mq.buf = append(mq.buf, mq.rawC)
		if mq.rawC == 0xFF {
			// After 0xFF, next byte uses only 7 bits (bit stuffing)
			mq.rawCT = 7
		} else {
			mq.rawCT = 8
		}
		mq.rawC = 0
	}
}

// FlushRaw flushes any pending raw bits to the output buffer.
// Must be called after the last EncodeRaw call in a bypass pass
// to ensure all bits are written.
func (mq *mqEncoder) FlushRaw() {
	if mq.rawCT > 0 && mq.rawCT < 8 {
		// Partial byte remains; pad remaining bits with zeros (already zero)
		mq.buf = append(mq.buf, mq.rawC)
	}
	mq.rawC = 0
	mq.rawCT = 0
}

// Flush finalizes the encoding and returns the encoded byte stream.
// Implements the FLUSH procedure from ITU-T T.800 Annex C (C.2.11).
//
// The flush procedure uses SETBITS to determine the minimal bits needed
// to unambiguously represent the final interval, then emits remaining
// bytes via BYTEOUT.
func (mq *mqEncoder) Flush() []byte {
	// SETBITS (C.2.11)
	// Determine the minimum C value that falls within the current interval.
	temp := mq.C + mq.A
	mq.C |= 0xFFFF
	if mq.C >= temp {
		mq.C -= 0x8000
	}

	// First BYTEOUT to emit accumulated bits
	mq.C <<= mq.CT
	mq.byteout()

	// Second BYTEOUT per spec
	mq.C <<= mq.CT
	mq.byteout()

	// Trim trailing 0xFF if present (not a valid termination byte)
	result := mq.buf
	if len(result) > 0 && result[len(result)-1] == 0xFF {
		result = result[:len(result)-1]
	}

	// Return a copy so the internal buffer can be reused
	out := make([]byte, len(result))
	copy(out, result)
	return out
}

// BytesWritten returns the number of bytes currently in the output buffer.
// Used to track per-pass lengths in continuous MQ mode.
func (mq *mqEncoder) BytesWritten() int {
	return len(mq.buf)
}

// Bytes returns the current encoded output without flushing.
// The returned slice is a copy of the internal buffer.
func (mq *mqEncoder) Bytes() []byte {
	out := make([]byte, len(mq.buf))
	copy(out, mq.buf)
	return out
}
