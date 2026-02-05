package jpeg2000

import (
	"bytes"
	"testing"
)

func TestBitReader_ReadBit(t *testing.T) {
	tests := []struct {
		name     string
		data     []byte
		expected []int
		wantErr  bool
	}{
		{
			name:     "single byte all zeros",
			data:     []byte{0x00},
			expected: []int{0, 0, 0, 0, 0, 0, 0, 0},
		},
		{
			name:     "single byte all ones",
			data:     []byte{0xFF},
			expected: []int{1, 1, 1, 1, 1, 1, 1, 1},
		},
		{
			name:     "alternating bits",
			data:     []byte{0xAA}, // 10101010
			expected: []int{1, 0, 1, 0, 1, 0, 1, 0},
		},
		{
			name:     "MSB first",
			data:     []byte{0x80}, // 10000000
			expected: []int{1, 0, 0, 0, 0, 0, 0, 0},
		},
		{
			name:     "multiple bytes",
			data:     []byte{0xF0, 0x0F}, // 11110000 00001111
			expected: []int{1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1},
		},
		{
			name:     "empty data",
			data:     []byte{},
			expected: []int{},
			wantErr:  true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := newBitReader(tt.data)

			for i, want := range tt.expected {
				got, err := r.ReadBit()
				if err != nil {
					t.Fatalf("ReadBit() at bit %d: unexpected error: %v", i, err)
				}
				if got != want {
					t.Errorf("ReadBit() at bit %d = %d, want %d", i, got, want)
				}
			}

			// Try reading one more bit
			_, err := r.ReadBit()
			if !tt.wantErr && err == nil {
				t.Error("expected error when reading beyond data")
			}
		})
	}
}

func TestBitReader_ReadBits(t *testing.T) {
	tests := []struct {
		name    string
		data    []byte
		n       int
		want    uint32
		wantErr bool
	}{
		{
			name: "read 0 bits",
			data: []byte{0xFF},
			n:    0,
			want: 0,
		},
		{
			name: "read 1 bit",
			data: []byte{0x80},
			n:    1,
			want: 1,
		},
		{
			name: "read 4 bits",
			data: []byte{0xF0},
			n:    4,
			want: 0x0F,
		},
		{
			name: "read 8 bits",
			data: []byte{0xAB},
			n:    8,
			want: 0xAB,
		},
		{
			name: "read 16 bits",
			data: []byte{0x12, 0x34},
			n:    16,
			want: 0x1234,
		},
		{
			name: "read 32 bits",
			data: []byte{0x12, 0x34, 0x56, 0x78},
			n:    32,
			want: 0x12345678,
		},
		{
			name: "read 12 bits across bytes",
			data: []byte{0xAB, 0xC0},
			n:    12,
			want: 0xABC,
		},
		{
			name:    "read more than 32 bits",
			data:    []byte{0xFF, 0xFF, 0xFF, 0xFF, 0xFF},
			n:       33,
			wantErr: true,
		},
		{
			name:    "read beyond data",
			data:    []byte{0xFF},
			n:       16,
			wantErr: true,
		},
		{
			name:    "negative bit count",
			data:    []byte{0xFF},
			n:       -1,
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := newBitReader(tt.data)
			got, err := r.ReadBits(tt.n)

			if tt.wantErr {
				if err == nil {
					t.Error("ReadBits() expected error, got nil")
				}
				return
			}

			if err != nil {
				t.Fatalf("ReadBits() unexpected error: %v", err)
			}

			if got != tt.want {
				t.Errorf("ReadBits(%d) = 0x%X, want 0x%X", tt.n, got, tt.want)
			}
		})
	}
}

func TestBitReader_ReadByte(t *testing.T) {
	tests := []struct {
		name     string
		data     []byte
		skipBits int // bits to skip before reading byte
		want     byte
		wantErr  bool
	}{
		{
			name:     "byte aligned",
			data:     []byte{0xAB, 0xCD},
			skipBits: 0,
			want:     0xAB,
		},
		{
			name:     "not byte aligned",
			data:     []byte{0xAB, 0xCD}, // skip 4 bits: 1010 [1011 1100] 1101
			skipBits: 4,
			want:     0xBC,
		},
		{
			name:     "single byte",
			data:     []byte{0x42},
			skipBits: 0,
			want:     0x42,
		},
		{
			name:     "read at end",
			data:     []byte{0xFF},
			skipBits: 8,
			wantErr:  true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := newBitReader(tt.data)

			// Skip bits if needed
			for i := 0; i < tt.skipBits; i++ {
				_, err := r.ReadBit()
				if err != nil {
					t.Fatalf("ReadBit() during skip: %v", err)
				}
			}

			got, err := r.ReadByte()

			if tt.wantErr {
				if err == nil {
					t.Error("ReadByte() expected error, got nil")
				}
				return
			}

			if err != nil {
				t.Fatalf("ReadByte() unexpected error: %v", err)
			}

			if got != tt.want {
				t.Errorf("ReadByte() = 0x%02X, want 0x%02X", got, tt.want)
			}
		})
	}
}

func TestBitReader_ReadUint16(t *testing.T) {
	tests := []struct {
		name     string
		data     []byte
		skipBits int
		want     uint16
		wantErr  bool
	}{
		{
			name:     "byte aligned",
			data:     []byte{0x12, 0x34},
			skipBits: 0,
			want:     0x1234,
		},
		{
			name:     "not byte aligned",
			data:     []byte{0x12, 0x34, 0x56}, // skip 4: 0001 [0010 0011 0100 0101] 0110
			skipBits: 4,
			want:     0x2345,
		},
		{
			name:     "insufficient data",
			data:     []byte{0x12},
			skipBits: 0,
			wantErr:  true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := newBitReader(tt.data)

			for i := 0; i < tt.skipBits; i++ {
				_, err := r.ReadBit()
				if err != nil {
					t.Fatalf("ReadBit() during skip: %v", err)
				}
			}

			got, err := r.ReadUint16()

			if tt.wantErr {
				if err == nil {
					t.Error("ReadUint16() expected error, got nil")
				}
				return
			}

			if err != nil {
				t.Fatalf("ReadUint16() unexpected error: %v", err)
			}

			if got != tt.want {
				t.Errorf("ReadUint16() = 0x%04X, want 0x%04X", got, tt.want)
			}
		})
	}
}

func TestBitReader_ReadUint32(t *testing.T) {
	tests := []struct {
		name     string
		data     []byte
		skipBits int
		want     uint32
		wantErr  bool
	}{
		{
			name:     "byte aligned",
			data:     []byte{0x12, 0x34, 0x56, 0x78},
			skipBits: 0,
			want:     0x12345678,
		},
		{
			name:     "not byte aligned",
			data:     []byte{0x12, 0x34, 0x56, 0x78, 0x9A}, // skip 4
			skipBits: 4,
			want:     0x23456789,
		},
		{
			name:     "insufficient data",
			data:     []byte{0x12, 0x34},
			skipBits: 0,
			wantErr:  true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := newBitReader(tt.data)

			for i := 0; i < tt.skipBits; i++ {
				_, err := r.ReadBit()
				if err != nil {
					t.Fatalf("ReadBit() during skip: %v", err)
				}
			}

			got, err := r.ReadUint32()

			if tt.wantErr {
				if err == nil {
					t.Error("ReadUint32() expected error, got nil")
				}
				return
			}

			if err != nil {
				t.Fatalf("ReadUint32() unexpected error: %v", err)
			}

			if got != tt.want {
				t.Errorf("ReadUint32() = 0x%08X, want 0x%08X", got, tt.want)
			}
		})
	}
}

func TestBitReader_ByteAlign(t *testing.T) {
	data := []byte{0xAB, 0xCD, 0xEF}
	r := newBitReader(data)

	// Read 3 bits
	_, err := r.ReadBits(3)
	if err != nil {
		t.Fatalf("ReadBits() error: %v", err)
	}

	// Should be at bit position 3
	if r.bitPos != 3 {
		t.Errorf("bitPos = %d, want 3", r.bitPos)
	}

	// Byte align
	r.ByteAlign()

	// Should be at byte position 1, bit position 0
	if r.pos != 1 || r.bitPos != 0 {
		t.Errorf("after ByteAlign: pos=%d, bitPos=%d, want pos=1, bitPos=0", r.pos, r.bitPos)
	}

	// Next byte should be 0xCD
	b, err := r.ReadByte()
	if err != nil {
		t.Fatalf("ReadByte() error: %v", err)
	}
	if b != 0xCD {
		t.Errorf("ReadByte() = 0x%02X, want 0xCD", b)
	}

	// ByteAlign when already aligned should be no-op
	r.ByteAlign()
	if r.pos != 2 || r.bitPos != 0 {
		t.Errorf("after second ByteAlign: pos=%d, bitPos=%d, want pos=2, bitPos=0", r.pos, r.bitPos)
	}
}

func TestBitReader_Position(t *testing.T) {
	data := []byte{0x12, 0x34, 0x56, 0x78}
	r := newBitReader(data)

	if r.Position() != 0 {
		t.Errorf("initial Position() = %d, want 0", r.Position())
	}

	// Read a byte
	_, err := r.ReadByte()
	if err != nil {
		t.Fatalf("ReadByte() error: %v", err)
	}

	if r.Position() != 1 {
		t.Errorf("Position() after ReadByte = %d, want 1", r.Position())
	}

	// Read some bits (not byte-aligned)
	_, err = r.ReadBits(4)
	if err != nil {
		t.Fatalf("ReadBits() error: %v", err)
	}

	// Position should still return current byte
	if r.Position() != 1 {
		t.Errorf("Position() after ReadBits = %d, want 1", r.Position())
	}
}

func TestBitReader_Remaining(t *testing.T) {
	data := []byte{0x12, 0x34, 0x56, 0x78}
	r := newBitReader(data)

	if r.Remaining() != 4 {
		t.Errorf("initial Remaining() = %d, want 4", r.Remaining())
	}

	_, err := r.ReadUint16()
	if err != nil {
		t.Fatalf("ReadUint16() error: %v", err)
	}

	if r.Remaining() != 2 {
		t.Errorf("Remaining() after ReadUint16 = %d, want 2", r.Remaining())
	}

	_, err = r.ReadUint16()
	if err != nil {
		t.Fatalf("ReadUint16() error: %v", err)
	}

	if r.Remaining() != 0 {
		t.Errorf("Remaining() at end = %d, want 0", r.Remaining())
	}
}

func TestBitReader_Skip(t *testing.T) {
	data := []byte{0x12, 0x34, 0x56, 0x78}
	r := newBitReader(data)

	// Skip 2 bytes
	err := r.Skip(2)
	if err != nil {
		t.Fatalf("Skip(2) error: %v", err)
	}

	if r.Position() != 2 {
		t.Errorf("Position() after Skip = %d, want 2", r.Position())
	}

	// Read next byte
	b, err := r.ReadByte()
	if err != nil {
		t.Fatalf("ReadByte() error: %v", err)
	}
	if b != 0x56 {
		t.Errorf("ReadByte() = 0x%02X, want 0x56", b)
	}

	// Skip beyond data
	err = r.Skip(10)
	if err == nil {
		t.Error("Skip(10) expected error, got nil")
	}
}

func TestBitReader_Peek(t *testing.T) {
	data := []byte{0x12, 0x34, 0x56}
	r := newBitReader(data)

	// Peek at first byte
	b, err := r.Peek()
	if err != nil {
		t.Fatalf("Peek() error: %v", err)
	}
	if b != 0x12 {
		t.Errorf("Peek() = 0x%02X, want 0x12", b)
	}

	// Position should not change
	if r.Position() != 0 {
		t.Errorf("Position() after Peek = %d, want 0", r.Position())
	}

	// Read some bits
	_, err = r.ReadBits(4)
	if err != nil {
		t.Fatalf("ReadBits() error: %v", err)
	}

	// Peek should align and peek next byte
	b, err = r.Peek()
	if err != nil {
		t.Fatalf("Peek() after ReadBits error: %v", err)
	}
	if b != 0x34 {
		t.Errorf("Peek() after ReadBits = 0x%02X, want 0x34", b)
	}

	// Bit position should be restored
	if r.bitPos != 4 {
		t.Errorf("bitPos after Peek = %d, want 4", r.bitPos)
	}
}

func TestBitReader_Bytes(t *testing.T) {
	data := []byte{0x12, 0x34, 0x56, 0x78}
	r := newBitReader(data)

	// Skip 1 byte
	err := r.Skip(1)
	if err != nil {
		t.Fatalf("Skip() error: %v", err)
	}

	// Get remaining bytes
	remaining := r.Bytes()
	expected := []byte{0x34, 0x56, 0x78}

	if !bytes.Equal(remaining, expected) {
		t.Errorf("Bytes() = %v, want %v", remaining, expected)
	}

	// Read to end
	err = r.Skip(3)
	if err != nil {
		t.Fatalf("Skip() error: %v", err)
	}

	remaining = r.Bytes()
	if remaining != nil {
		t.Errorf("Bytes() at end = %v, want nil", remaining)
	}
}

func TestBitReader_EdgeCases(t *testing.T) {
	t.Run("empty reader", func(t *testing.T) {
		r := newBitReader([]byte{})

		_, err := r.ReadBit()
		if err == nil {
			t.Error("ReadBit() on empty reader expected error")
		}

		_, err = r.ReadByte()
		if err == nil {
			t.Error("ReadByte() on empty reader expected error")
		}
	})

	t.Run("read exactly to end", func(t *testing.T) {
		r := newBitReader([]byte{0xFF})

		// Read 8 bits exactly
		for i := range 8 {
			_, err := r.ReadBit()
			if err != nil {
				t.Fatalf("ReadBit() %d error: %v", i, err)
			}
		}

		// Next read should fail
		_, err := r.ReadBit()
		if err == nil {
			t.Error("ReadBit() beyond data expected error")
		}
	})

	t.Run("mixed bit and byte operations", func(t *testing.T) {
		r := newBitReader([]byte{0xAB, 0xCD, 0xEF})

		// Read 4 bits
		bits, err := r.ReadBits(4)
		if err != nil {
			t.Fatalf("ReadBits() error: %v", err)
		}
		if bits != 0x0A {
			t.Errorf("ReadBits(4) = 0x%X, want 0x0A", bits)
		}

		// Read byte (should read next 8 bits: 1011 1100)
		b, err := r.ReadByte()
		if err != nil {
			t.Fatalf("ReadByte() error: %v", err)
		}
		if b != 0xBC {
			t.Errorf("ReadByte() = 0x%02X, want 0xBC", b)
		}

		// Read remaining 4 bits
		bits, err = r.ReadBits(4)
		if err != nil {
			t.Fatalf("ReadBits() error: %v", err)
		}
		if bits != 0x0D {
			t.Errorf("ReadBits(4) = 0x%X, want 0x0D", bits)
		}
	})
}
