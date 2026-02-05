package jpeg2000

// bitWriter provides bit-level writing to a byte buffer.
// Bits are written in MSB-first order (most significant bit first).
//
// Supports optional bit-stuffing per ITU-T T.800 (JPEG2000):
// After writing a 0xFF byte, a stuffed 0-bit is inserted as the MSB of
// the next byte. This prevents accidental marker sequences in the output.
type bitWriter struct {
	buf       []byte // completed bytes
	curByte   byte   // current byte being assembled
	bitPos    uint   // number of bits written in current byte (0-7)
	bitStuff  bool   // enable 0xFF bit-stuffing mode
	prevWasFF bool   // previous completed byte was 0xFF
}

// newBitWriter creates a new bit writer with bit-stuffing disabled.
func newBitWriter() *bitWriter {
	return &bitWriter{}
}

// newBitWriterWithStuffing creates a bit writer with 0xFF bit-stuffing enabled.
func newBitWriterWithStuffing() *bitWriter {
	return &bitWriter{
		bitStuff: true,
	}
}

// SetBitStuffing enables or disables bit-stuffing mode.
func (w *bitWriter) SetBitStuffing(enabled bool) {
	w.bitStuff = enabled
	if !enabled {
		w.prevWasFF = false
	}
}

// WriteBit writes a single bit (0 or 1), MSB first.
//
// With bit-stuffing enabled: after completing a 0xFF byte, the MSB of
// the next byte is forced to 0 (the stuffed bit). This means only 7
// data bits can be stored in bytes that follow a 0xFF byte.
func (w *bitWriter) WriteBit(bit int) {
	// If bit-stuffing is active and the previous completed byte was 0xFF,
	// the MSB of this new byte must be 0 (stuffed bit). We start bitPos
	// at 1 with curByte's bit 7 already 0, mirroring the reader's logic.
	if w.bitPos == 0 && w.prevWasFF {
		// Stuffed 0-bit occupies bit 7; advance bitPos past it.
		// curByte is already 0, so bit 7 is already 0.
		w.bitPos = 1
		w.prevWasFF = false
	}

	if bit != 0 {
		w.curByte |= 1 << (7 - w.bitPos)
	}
	w.bitPos++

	if w.bitPos == 8 {
		w.flushByte()
	}
}

// flushByte appends the current assembled byte to the buffer and resets state.
func (w *bitWriter) flushByte() {
	w.buf = append(w.buf, w.curByte)
	if w.bitStuff {
		w.prevWasFF = w.curByte == 0xFF
	}
	w.curByte = 0
	w.bitPos = 0
}

// WriteBits writes n bits from val (MSB first, n <= 32).
func (w *bitWriter) WriteBits(val uint32, n int) {
	for i := n - 1; i >= 0; i-- {
		w.WriteBit(int((val >> uint(i)) & 1))
	}
}

// WriteByte writes 8 bits from b.
func (w *bitWriter) WriteByte(b byte) error {
	w.WriteBits(uint32(b), 8)
	return nil
}

// ByteAlign pads with zero bits to reach the next byte boundary.
// If already byte-aligned, this is a no-op.
func (w *bitWriter) ByteAlign() {
	if w.bitPos == 0 {
		return
	}
	// Handle the stuffed-bit case: if prevWasFF is set and bitPos is 0,
	// WriteBit will insert the stuffed bit. But here bitPos > 0, so the
	// stuffed bit (if any) was already inserted by a prior WriteBit call.
	for w.bitPos != 0 {
		w.WriteBit(0)
	}
}

// Flush finalizes writing and returns the encoded bytes.
// Any remaining bits in the partial byte are padded with zeros.
// Returns a copy of the internal buffer.
func (w *bitWriter) Flush() []byte {
	// If there's a partial byte pending, handle stuffing and pad it.
	if w.bitPos > 0 {
		// If prevWasFF and bitPos is 0, WriteBit would insert the stuff bit,
		// but here bitPos > 0 means stuffing was already applied. Just pad.
		for w.bitPos != 0 {
			w.WriteBit(0)
		}
	} else if w.prevWasFF && w.bitStuff {
		// Edge case: the last completed byte was 0xFF and no more bits follow.
		// Per ITU-T T.800, we must still emit the stuffed byte so the decoder
		// doesn't misinterpret the trailing 0xFF as a marker prefix.
		w.WriteBit(0) // inserts stuffed 0-bit at MSB
		// Pad remaining bits
		for w.bitPos != 0 {
			w.WriteBit(0)
		}
	}

	out := make([]byte, len(w.buf))
	copy(out, w.buf)
	return out
}

// Len returns the current length in bytes, including any partial byte
// that has not yet been flushed.
func (w *bitWriter) Len() int {
	n := len(w.buf)
	if w.bitPos > 0 {
		n++
	}
	return n
}

// Reset resets the writer for reuse, clearing all internal state.
func (w *bitWriter) Reset() {
	w.buf = w.buf[:0]
	w.curByte = 0
	w.bitPos = 0
	w.prevWasFF = false
}
