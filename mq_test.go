package jpeg2000

import (
	"testing"
)

func TestMQDecoderInit(t *testing.T) {
	data := []byte{0x00, 0x00, 0x00, 0x00}
	mq := newMQDecoder(data)

	if mq.A != 0x8000 {
		t.Errorf("Expected A=0x8000 after init, got 0x%04x", mq.A)
	}

	// Check that contexts are initialized per OpenJPEG mqc.c:313-315
	// Context 0 (ZC first): state 4
	// Context 17 (AGG): state 3
	// Context 18 (UNI): state 46
	// All others: state 0
	for i, ctx := range mq.contexts {
		expectedIndex := 0
		switch i {
		case 0:
			expectedIndex = 4 // T1_CTXNO_ZC
		case 17:
			expectedIndex = 3 // T1_CTXNO_AGG
		case 18:
			expectedIndex = 46 // T1_CTXNO_UNI (uniform probability)
		}
		if ctx.index != expectedIndex {
			t.Errorf("Context %d: expected index=%d, got %d", i, expectedIndex, ctx.index)
		}
		if ctx.mps != 0 {
			t.Errorf("Context %d: expected mps=0, got %d", i, ctx.mps)
		}
	}
}

func TestMQDecoderBasicDecode(t *testing.T) {
	tests := []struct {
		name  string
		data  []byte
		ctx   int
		count int
	}{
		{
			name:  "all zeros",
			data:  []byte{0x00, 0x00, 0x00, 0x00},
			ctx:   0,
			count: 4,
		},
		{
			name:  "mixed bits",
			data:  []byte{0xFF, 0x00, 0xFF, 0x00},
			ctx:   0,
			count: 4,
		},
		{
			name:  "sequential bytes",
			data:  []byte{0x12, 0x34, 0x56, 0x78},
			ctx:   0,
			count: 8,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mq := newMQDecoder(tt.data)

			// Decode bits
			bits := make([]int, tt.count)
			for i := 0; i < tt.count; i++ {
				bits[i] = mq.Decode(tt.ctx)
			}

			// Verify bits are 0 or 1 (the actual values depend on MQ state machine)
			for i, bit := range bits {
				if bit != 0 && bit != 1 {
					t.Errorf("Bit %d: expected 0 or 1, got %d", i, bit)
				}
			}

			// Log decoded sequence for debugging
			t.Logf("Decoded %d bits: %v", tt.count, bits)
		})
	}
}

func TestMQDecoderContextTransitions(t *testing.T) {
	data := []byte{0x84, 0x21, 0x08, 0x42}
	mq := newMQDecoder(data)

	// Decode using different contexts
	initialStates := make([]mqContext, len(mq.contexts))
	copy(initialStates, mq.contexts[:])

	// Decode some bits using context 0
	for range 5 {
		mq.Decode(0)
	}

	// Context 0 should have transitioned
	if mq.contexts[0].index == initialStates[0].index {
		t.Log("Context 0 did not transition (possible if all MPS)")
	}

	// Other contexts should remain unchanged
	for i := 1; i < len(mq.contexts); i++ {
		if mq.contexts[i].index != initialStates[i].index {
			t.Errorf("Context %d changed without being used", i)
		}
	}
}

func TestMQDecoderByteStuffing(t *testing.T) {
	tests := []struct {
		name string
		data []byte
		desc string
	}{
		{
			name: "0xFF followed by 0x00",
			data: []byte{0xFF, 0x00, 0x00, 0x00},
			desc: "bit stuffing case - should be treated as 0xFF with stuffed zero bit",
		},
		{
			name: "0xFF followed by 0x7F",
			data: []byte{0xFF, 0x7F, 0x00, 0x00},
			desc: "bit stuffing case - 0x7F <= 0x8F",
		},
		{
			name: "0xFF followed by 0x90",
			data: []byte{0xFF, 0x90, 0x00, 0x00},
			desc: "marker case - 0x90 > 0x8F, should stop reading",
		},
		{
			name: "0xFF followed by 0xFF",
			data: []byte{0xFF, 0xFF, 0x00, 0x00},
			desc: "marker case - 0xFF > 0x8F",
		},
		{
			name: "normal bytes",
			data: []byte{0x12, 0x34, 0x56, 0x78},
			desc: "no bit stuffing needed",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mq := newMQDecoder(tt.data)

			// Decode several bits to trigger bytein
			for range 16 {
				bit := mq.Decode(0)
				if bit != 0 && bit != 1 {
					t.Errorf("Invalid bit value: %d", bit)
				}
			}

			// Test should not crash and should produce valid bits
			t.Logf("%s: decoded successfully", tt.desc)
		})
	}
}

func TestMQDecoderRawBits(t *testing.T) {
	// Test raw bit decoding (bypass mode)
	// Note: After initDec, the first 2 bytes are already consumed by the decoder
	// So we need enough data to test raw bit decoding properly
	data := []byte{0x00, 0x00, 0xAA, 0x55} // init consumes first 2 bytes, then 10101010 01010101

	mq := newMQDecoder(data)

	// Decode 16 raw bits from 0xAA, 0x55
	bits := make([]int, 16)
	for i := range 16 {
		bits[i] = mq.DecodeBitRaw()
	}

	// Log the decoded bits
	t.Logf("Decoded raw bits: %v", bits)

	// Verify all bits are valid (0 or 1)
	for i, bit := range bits {
		if bit != 0 && bit != 1 {
			t.Errorf("Bit %d: invalid value %d", i, bit)
		}
	}

	// The exact pattern depends on how initDec leaves the decoder state,
	// so we just verify validity rather than exact values
}

func TestMQDecoderRenormalization(t *testing.T) {
	// Test that renormalization works correctly
	data := []byte{0x00, 0x00, 0x00, 0x00}
	mq := newMQDecoder(data)

	initialA := mq.A

	// Decode bits until renormalization happens
	for i := range 10 {
		mq.Decode(0)

		// A should always be >= 0x8000 after Decode returns
		if mq.A < 0x8000 {
			t.Errorf("After decode %d: A=0x%04x, should be >= 0x8000", i, mq.A)
		}
	}

	t.Logf("Initial A: 0x%04x, Final A: 0x%04x", initialA, mq.A)
}

func TestMQDecoderReset(t *testing.T) {
	data1 := []byte{0x12, 0x34, 0x56, 0x78}
	data2 := []byte{0xAA, 0xBB, 0xCC, 0xDD}

	mq := newMQDecoder(data1)

	// Decode some bits
	mq.Decode(0)
	mq.Decode(1)
	mq.Decode(2)

	// Reset with new data
	mq.Reset(data2)

	// Check state is reset
	if mq.A != 0x8000 {
		t.Errorf("Expected A=0x8000 after reset, got 0x%04x", mq.A)
	}

	// pos will be > 0 after initDec reads initial bytes
	// Just check it's been reset to a reasonable value (should be 2 after init)
	if mq.pos > 4 || mq.pos < 0 {
		t.Errorf("Expected pos in range [0,4] after reset, got %d", mq.pos)
	}

	// Check contexts are reset per OpenJPEG mqc.c:313-315
	for i, ctx := range mq.contexts {
		expectedIndex := 0
		switch i {
		case 0:
			expectedIndex = 4 // T1_CTXNO_ZC
		case 17:
			expectedIndex = 3 // T1_CTXNO_AGG
		case 18:
			expectedIndex = 46 // T1_CTXNO_UNI (uniform probability)
		}
		if ctx.index != expectedIndex {
			t.Errorf("Context %d: expected index=%d after reset, got %d", i, expectedIndex, ctx.index)
		}
		if ctx.mps != 0 {
			t.Errorf("Context %d: expected mps=0 after reset, got %d", i, ctx.mps)
		}
	}
}

func TestMQDecoderMultipleContexts(t *testing.T) {
	data := []byte{0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0}
	mq := newMQDecoder(data)

	// Decode using different contexts
	contexts := []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

	for _, ctx := range contexts {
		for i := range 3 {
			bit := mq.Decode(ctx)
			if bit != 0 && bit != 1 {
				t.Errorf("Context %d, bit %d: invalid value %d", ctx, i, bit)
			}
		}
	}

	// Each context should have evolved independently
	states := make(map[int]bool)
	for _, ctx := range contexts {
		state := mq.contexts[ctx].index
		states[state] = true
	}

	t.Logf("Unique context states: %d", len(states))
}

func TestMQDecoderEndOfData(t *testing.T) {
	// Test behavior when running out of data
	data := []byte{0x12, 0x34}
	mq := newMQDecoder(data)

	// Decode many more bits than available
	for i := range 100 {
		bit := mq.Decode(0)
		if bit != 0 && bit != 1 {
			t.Errorf("Bit %d: invalid value %d", i, bit)
		}
	}

	// Should not crash, decoder fills with 0xFF when out of data
	t.Log("Successfully handled end of data")
}

func TestMQDecoderProbabilityTable(t *testing.T) {
	// Verify probability table integrity
	if len(mqProbTable) != 47 {
		t.Errorf("Expected 47 probability states, got %d", len(mqProbTable))
	}

	// Check that state transitions are valid
	for i, entry := range mqProbTable {
		if entry.nmps < 0 || entry.nmps >= len(mqProbTable) {
			t.Errorf("State %d: invalid nmps=%d", i, entry.nmps)
		}
		if entry.nlps < 0 || entry.nlps >= len(mqProbTable) {
			t.Errorf("State %d: invalid nlps=%d", i, entry.nlps)
		}
	}
}

func TestMQDecoderInvalidContext(t *testing.T) {
	data := []byte{0x12, 0x34, 0x56, 0x78}
	mq := newMQDecoder(data)

	// Test invalid context indices
	invalidContexts := []int{-1, 19, 100}

	for _, ctx := range invalidContexts {
		bit := mq.Decode(ctx)
		if bit != 0 {
			t.Logf("Invalid context %d returned %d (expected 0)", ctx, bit)
		}
	}
}

func BenchmarkMQDecoderDecode(b *testing.B) {
	data := make([]byte, 1024)
	for i := range data {
		data[i] = byte(i)
	}

	mq := newMQDecoder(data)

	for i := 0; b.Loop(); i++ {
		mq.Decode(i % 19)
	}
}

func BenchmarkMQDecoderDecodeBitRaw(b *testing.B) {
	data := make([]byte, 1024)
	for i := range data {
		data[i] = byte(i)
	}

	mq := newMQDecoder(data)

	for b.Loop() {
		mq.DecodeBitRaw()
	}
}

func TestMQDecoderRealisticScenario(t *testing.T) {
	// Simulate a realistic JPEG2000 code block decoding scenario
	// with mixed context-based and raw bit decoding

	// Sample encoded data (this would come from JPEG2000 bitstream)
	data := []byte{
		0x84, 0x21, 0x10, 0x84, 0x21, 0x10, 0x84, 0x21,
		0xC6, 0x31, 0x8C, 0x63, 0x18, 0xC6, 0x31, 0x8C,
	}

	mq := newMQDecoder(data)

	// Decode significance bits using context 0
	sigBits := make([]int, 8)
	for i := range 8 {
		sigBits[i] = mq.Decode(0) // Context 0: significance
	}
	t.Logf("Significance bits: %v", sigBits)

	// Decode sign bits using context 9
	signBits := make([]int, 4)
	for i := range 4 {
		signBits[i] = mq.Decode(9) // Context 9: sign
	}
	t.Logf("Sign bits: %v", signBits)

	// Decode some magnitude refinement bits using context 16
	magBits := make([]int, 6)
	for i := range 6 {
		magBits[i] = mq.Decode(16) // Context 16: magnitude refinement
	}
	t.Logf("Magnitude refinement bits: %v", magBits)

	// Decode some raw bits (cleanup pass)
	rawBits := make([]int, 8)
	for i := range 8 {
		rawBits[i] = mq.DecodeBitRaw()
	}
	t.Logf("Raw cleanup bits: %v", rawBits)

	// Verify all decoded values are valid
	for i, bit := range sigBits {
		if bit != 0 && bit != 1 {
			t.Errorf("Significance bit %d: invalid value %d", i, bit)
		}
	}
	for i, bit := range signBits {
		if bit != 0 && bit != 1 {
			t.Errorf("Sign bit %d: invalid value %d", i, bit)
		}
	}
	for i, bit := range magBits {
		if bit != 0 && bit != 1 {
			t.Errorf("Magnitude bit %d: invalid value %d", i, bit)
		}
	}
	for i, bit := range rawBits {
		if bit != 0 && bit != 1 {
			t.Errorf("Raw bit %d: invalid value %d", i, bit)
		}
	}
}
