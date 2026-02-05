package jpeg2000

import (
	"encoding/binary"
	"fmt"
	"strings"
)

// bitReader provides bit-level reading from a byte stream.
// Bits are read in MSB-first order (most significant bit first).
//
// Supports optional bit-stuffing per ITU-T T.800 (JPEG2000):
// After reading a 0xFF byte, the MSB of the next byte is a stuffed
// 0-bit that must be skipped. This prevents accidental marker sequences.
type bitReader struct {
	data       []byte
	pos        int  // byte position
	bitPos     uint // bit position within current byte (0-7), reads MSB first
	bitStuff   bool // enable 0xFF bit-stuffing mode
	prevWasFF  bool // previous byte was 0xFF (for bit-stuffing)
	hitMarker  bool // true if we've hit a marker sequence (0xFF followed by 0x80-0xFF)
}

// newBitReader creates a new bit reader from the given data.
func newBitReader(data []byte) *bitReader {
	return &bitReader{
		data:     data,
		pos:      0,
		bitPos:   0,
		bitStuff: false,
	}
}

// newBitReaderWithStuffing creates a bit reader with 0xFF bit-stuffing enabled.
// Per ITU-T T.800, after reading a 0xFF byte, the MSB of the next byte must be skipped.
func newBitReaderWithStuffing(data []byte) *bitReader {
	return &bitReader{
		data:     data,
		pos:      0,
		bitPos:   0,
		bitStuff: true,
	}
}

// SetBitStuffing enables or disables 0xFF bit-stuffing mode.
func (r *bitReader) SetBitStuffing(enabled bool) {
	r.bitStuff = enabled
	if !enabled {
		r.prevWasFF = false
		r.hitMarker = false
	}
}

// HitMarker returns true if a marker sequence was detected during bit-stuffed reading.
// This happens when 0xFF is followed by a byte with MSB=1 (0x80-0xFF).
func (r *bitReader) HitMarker() bool {
	return r.hitMarker
}

// ResetMarker clears the marker detection flag.
func (r *bitReader) ResetMarker() {
	r.hitMarker = false
}

// ReadBit reads a single bit (MSB first order).
// Returns 0 or 1, or an error if EOF.
//
// With bit-stuffing enabled: after completing a 0xFF byte, the MSB of the
// next byte is skipped ONLY if it's 0 (a stuffed bit per ITU-T T.800).
// If the MSB is 1, it indicates a marker sequence (0xFF followed by 0x80-0xFF),
// and we should NOT skip the bit - the marker should be handled by the caller.
// When a marker is detected, hitMarker is set to true.
func (r *bitReader) ReadBit() (int, error) {
	if r.pos >= len(r.data) {
		return 0, ErrTruncatedData
	}

	// If we've hit a marker, stop reading
	if r.hitMarker {
		return 0, ErrTruncatedData
	}

	// Read bit from current byte (MSB first)
	bit := int((r.data[r.pos] >> (7 - r.bitPos)) & 1)

	// Advance bit position
	r.bitPos++
	if r.bitPos == 8 {
		// Track if this completed byte was 0xFF (for bit-stuffing)
		wasFF := r.bitStuff && r.data[r.pos] == 0xFF

		r.bitPos = 0
		r.pos++

		// Bit-stuffing: after 0xFF, skip MSB of next byte ONLY if it's 0.
		// Per ITU-T T.800, the stuffed bit is always 0. If MSB is 1, it indicates
		// a marker sequence (0xFF 0x80-0xFF) and should NOT be skipped.
		// The caller (packet header parser) should detect and handle markers.
		if wasFF && r.pos < len(r.data) {
			nextByte := r.data[r.pos]
			if nextByte&0x80 == 0 {
				// MSB is 0 - it's a stuffed bit, skip it
				r.bitPos = 1
			} else {
				// MSB is 1 - it's a marker sequence, set flag and stop
				// Back up to point at the 0xFF so EPH/SOP check can find it
				r.pos--
				r.bitPos = 0 // Byte-aligned at the marker start
				r.hitMarker = true
			}
		}
	}

	return bit, nil
}

// ReadBits reads n bits (n <= 32), MSB first.
// Returns the bits as a uint32, or an error if not enough data.
func (r *bitReader) ReadBits(n int) (uint32, error) {
	if n < 0 || n > 32 {
		return 0, fmt.Errorf("jpeg2000: invalid bit count: %d", n)
	}

	if n == 0 {
		return 0, nil
	}

	var result uint32
	for range n {
		bit, err := r.ReadBit()
		if err != nil {
			return 0, err
		}
		result = (result << 1) | uint32(bit)
	}

	return result, nil
}

// ReadByte reads 8 bits as a byte.
func (r *bitReader) ReadByte() (byte, error) {
	// Fast path: if byte-aligned, read directly
	if r.bitPos == 0 {
		if r.pos >= len(r.data) {
			return 0, ErrTruncatedData
		}
		b := r.data[r.pos]
		r.pos++
		return b, nil
	}

	// Slow path: read 8 bits
	bits, err := r.ReadBits(8)
	if err != nil {
		return 0, err
	}
	return byte(bits), nil
}

// ReadUint16 reads 16 bits big-endian.
func (r *bitReader) ReadUint16() (uint16, error) {
	// Fast path: if byte-aligned, read directly
	if r.bitPos == 0 && r.pos+2 <= len(r.data) {
		val := binary.BigEndian.Uint16(r.data[r.pos:])
		r.pos += 2
		return val, nil
	}

	// Slow path: read 16 bits
	bits, err := r.ReadBits(16)
	if err != nil {
		return 0, err
	}
	return uint16(bits), nil
}

// ReadUint32 reads 32 bits big-endian.
func (r *bitReader) ReadUint32() (uint32, error) {
	// Fast path: if byte-aligned, read directly
	if r.bitPos == 0 && r.pos+4 <= len(r.data) {
		val := binary.BigEndian.Uint32(r.data[r.pos:])
		r.pos += 4
		return val, nil
	}

	// Slow path: read 32 bits
	return r.ReadBits(32)
}

// ByteAlign aligns to next byte boundary.
// If already byte-aligned, this is a no-op.
func (r *bitReader) ByteAlign() {
	if r.bitPos != 0 {
		r.bitPos = 0
		r.pos++
	}
}

// Position returns current byte position.
// If not byte-aligned, returns the position of the current partial byte.
func (r *bitReader) Position() int {
	return r.pos
}

// BitPosition returns current bit position (byte * 8 + bit offset).
func (r *bitReader) BitPosition() int {
	return r.pos*8 + int(r.bitPos)
}

// Remaining returns bytes remaining from current position.
// This is approximate if not byte-aligned.
func (r *bitReader) Remaining() int {
	rem := len(r.data) - r.pos
	if rem < 0 {
		return 0
	}
	return rem
}

// Len returns total length of data.
func (r *bitReader) Len() int {
	return len(r.data)
}

// ByteAt returns byte at given position (for debugging).
func (r *bitReader) ByteAt(i int) byte {
	if i >= 0 && i < len(r.data) {
		return r.data[i]
	}
	return 0
}

// Skip skips n bytes.
// This byte-aligns first, then skips.
func (r *bitReader) Skip(n int) error {
	r.ByteAlign()
	if r.pos+n > len(r.data) {
		return ErrTruncatedData
	}
	r.pos += n
	return nil
}

// Peek returns next byte without advancing.
// This byte-aligns first if needed.
func (r *bitReader) Peek() (byte, error) {
	// Save state
	savedPos := r.pos
	savedBitPos := r.bitPos

	// Byte align if needed
	if r.bitPos != 0 {
		r.ByteAlign()
	}

	if r.pos >= len(r.data) {
		// Restore state
		r.pos = savedPos
		r.bitPos = savedBitPos
		return 0, ErrTruncatedData
	}

	b := r.data[r.pos]

	// Restore state
	r.pos = savedPos
	r.bitPos = savedBitPos

	return b, nil
}

// Bytes returns remaining bytes as slice.
// This byte-aligns first.
func (r *bitReader) Bytes() []byte {
	r.ByteAlign()
	if r.pos >= len(r.data) {
		return nil
	}
	return r.data[r.pos:]
}

// PeekByte returns byte at offset from current position without advancing.
// This assumes we're byte-aligned or will byte-align for peek.
func (r *bitReader) PeekByte(offset int) (byte, error) {
	// Calculate actual position after byte alignment
	peekPos := r.pos
	if r.bitPos != 0 {
		peekPos++
	}
	peekPos += offset

	if peekPos >= len(r.data) {
		return 0, ErrTruncatedData
	}
	return r.data[peekPos], nil
}

// PeekBits returns the next n bits without advancing, for debugging
func (r *bitReader) PeekBits(n int) string {
	// Save state
	savedPos := r.pos
	savedBitPos := r.bitPos

	var result strings.Builder
	for i := 0; i < n && r.pos < len(r.data); i++ {
		bit, err := r.ReadBit()
		if err != nil {
			break
		}
		if bit == 0 {
			result.WriteString("0")
		} else {
			result.WriteString("1")
		}
	}

	// Restore state
	r.pos = savedPos
	r.bitPos = savedBitPos

	return result.String()
}
