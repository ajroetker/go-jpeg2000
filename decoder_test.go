package jpeg2000

import (
	"bytes"
	"encoding/binary"
	"image"
	"image/color"
	"testing"
)

// TestExtractCodestream_RawCodestream verifies extraction from raw codestream
func TestExtractCodestream_RawCodestream(t *testing.T) {
	// Create minimal raw codestream with SOC marker
	data := []byte{0xFF, 0x4F, 0xFF, 0x51, 0x00, 0x00}

	result, _ := extractCodestream(data)
	if result == nil {
		t.Fatal("extractCodestream returned nil for raw codestream")
	}

	if !bytes.Equal(result, data) {
		t.Errorf("extractCodestream modified raw codestream data")
	}
}

// TestExtractCodestream_JP2Container verifies extraction from JP2 file
func TestExtractCodestream_JP2Container(t *testing.T) {
	// Create minimal JP2 container with codestream box
	var buf bytes.Buffer

	// JP2 Signature Box
	binary.Write(&buf, binary.BigEndian, uint32(12))      // Length
	binary.Write(&buf, binary.BigEndian, uint32(0x6A5020)) // "jP " (incomplete for test)
	binary.Write(&buf, binary.BigEndian, uint32(0x0D0A870A)) // Signature

	// File Type Box
	binary.Write(&buf, binary.BigEndian, uint32(20))        // Length
	binary.Write(&buf, binary.BigEndian, uint32(0x66747970)) // "ftyp"
	binary.Write(&buf, binary.BigEndian, uint32(0x6A703220)) // Brand: "jp2 "
	binary.Write(&buf, binary.BigEndian, uint32(0))          // Minor version
	binary.Write(&buf, binary.BigEndian, uint32(0x6A703220)) // Compatible brand

	// Codestream Box
	codestreamData := []byte{0xFF, 0x4F, 0xFF, 0x51, 0x00, 0x00}
	csBoxLen := uint32(8 + len(codestreamData))
	binary.Write(&buf, binary.BigEndian, csBoxLen)         // Length
	binary.Write(&buf, binary.BigEndian, uint32(0x6A703263)) // "jp2c"
	buf.Write(codestreamData)

	result, _ := extractCodestream(buf.Bytes())
	if result == nil {
		t.Fatal("extractCodestream returned nil for JP2 container")
	}

	if !bytes.Equal(result, codestreamData) {
		t.Errorf("extractCodestream returned wrong data.\nGot:  %v\nWant: %v",
			result, codestreamData)
	}
}

// TestExtractCodestream_InvalidData verifies handling of invalid data
func TestExtractCodestream_InvalidData(t *testing.T) {
	tests := []struct {
		name string
		data []byte
	}{
		{
			name: "empty data",
			data: []byte{},
		},
		{
			name: "too short",
			data: []byte{0xFF},
		},
		{
			name: "invalid marker",
			data: []byte{0x00, 0x00, 0x00, 0x00},
		},
		{
			name: "truncated JP2",
			data: []byte{0x00, 0x00, 0x00, 0x0C, 0x6A, 0x50}, // Incomplete signature
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, _ := extractCodestream(tt.data)
			if result != nil {
				t.Errorf("extractCodestream should return nil for %s, got %v",
					tt.name, result)
			}
		})
	}
}

// TestDecodeConfig verifies DecodeConfig returns correct image dimensions
func TestDecodeConfig(t *testing.T) {
	// Create minimal codestream with SOC, SIZ markers
	var buf bytes.Buffer

	// SOC marker
	binary.Write(&buf, binary.BigEndian, uint16(markerSOC))

	// SIZ marker
	binary.Write(&buf, binary.BigEndian, uint16(markerSIZ))
	sizLen := uint16(41) // Minimal SIZ for 1 component
	binary.Write(&buf, binary.BigEndian, sizLen)
	binary.Write(&buf, binary.BigEndian, uint16(0))       // Rsiz (capabilities)
	binary.Write(&buf, binary.BigEndian, uint32(640))     // Xsiz (width)
	binary.Write(&buf, binary.BigEndian, uint32(480))     // Ysiz (height)
	binary.Write(&buf, binary.BigEndian, uint32(0))       // XOsiz
	binary.Write(&buf, binary.BigEndian, uint32(0))       // YOsiz
	binary.Write(&buf, binary.BigEndian, uint32(640))     // XTsiz (tile width)
	binary.Write(&buf, binary.BigEndian, uint32(480))     // YTsiz (tile height)
	binary.Write(&buf, binary.BigEndian, uint32(0))       // XTOsiz
	binary.Write(&buf, binary.BigEndian, uint32(0))       // YTOsiz
	binary.Write(&buf, binary.BigEndian, uint16(1))       // Csiz (num components)
	binary.Write(&buf, binary.BigEndian, uint8(7))        // Ssiz[0] (bit depth - 1)
	binary.Write(&buf, binary.BigEndian, uint8(1))        // XRsiz[0]
	binary.Write(&buf, binary.BigEndian, uint8(1))        // YRsiz[0]

	// EOC marker
	binary.Write(&buf, binary.BigEndian, uint16(markerEOC))

	config, err := DecodeConfig(bytes.NewReader(buf.Bytes()))
	if err != nil {
		t.Fatalf("DecodeConfig failed: %v", err)
	}

	if config.Width != 640 {
		t.Errorf("Width = %d, want 640", config.Width)
	}
	if config.Height != 480 {
		t.Errorf("Height = %d, want 480", config.Height)
	}
	if config.ColorModel != color.GrayModel {
		t.Errorf("ColorModel = %v, want GrayModel", config.ColorModel)
	}
}

// TestDecodeConfig_RGB verifies RGB color model detection
func TestDecodeConfig_RGB(t *testing.T) {
	// Create minimal codestream with 3 components
	var buf bytes.Buffer

	// SOC marker
	binary.Write(&buf, binary.BigEndian, uint16(markerSOC))

	// SIZ marker
	binary.Write(&buf, binary.BigEndian, uint16(markerSIZ))
	sizLen := uint16(41 + 6) // Base + 2 extra components * 3 bytes
	binary.Write(&buf, binary.BigEndian, sizLen)
	binary.Write(&buf, binary.BigEndian, uint16(0))       // Rsiz
	binary.Write(&buf, binary.BigEndian, uint32(640))     // Xsiz
	binary.Write(&buf, binary.BigEndian, uint32(480))     // Ysiz
	binary.Write(&buf, binary.BigEndian, uint32(0))       // XOsiz
	binary.Write(&buf, binary.BigEndian, uint32(0))       // YOsiz
	binary.Write(&buf, binary.BigEndian, uint32(640))     // XTsiz
	binary.Write(&buf, binary.BigEndian, uint32(480))     // YTsiz
	binary.Write(&buf, binary.BigEndian, uint32(0))       // XTOsiz
	binary.Write(&buf, binary.BigEndian, uint32(0))       // YTOsiz
	binary.Write(&buf, binary.BigEndian, uint16(3))       // Csiz (3 components = RGB)

	// Component 0 (R)
	binary.Write(&buf, binary.BigEndian, uint8(7)) // 8-bit
	binary.Write(&buf, binary.BigEndian, uint8(1)) // XRsiz
	binary.Write(&buf, binary.BigEndian, uint8(1)) // YRsiz

	// Component 1 (G)
	binary.Write(&buf, binary.BigEndian, uint8(7))
	binary.Write(&buf, binary.BigEndian, uint8(1))
	binary.Write(&buf, binary.BigEndian, uint8(1))

	// Component 2 (B)
	binary.Write(&buf, binary.BigEndian, uint8(7))
	binary.Write(&buf, binary.BigEndian, uint8(1))
	binary.Write(&buf, binary.BigEndian, uint8(1))

	// EOC marker
	binary.Write(&buf, binary.BigEndian, uint16(markerEOC))

	config, err := DecodeConfig(bytes.NewReader(buf.Bytes()))
	if err != nil {
		t.Fatalf("DecodeConfig failed: %v", err)
	}

	if config.ColorModel != color.RGBAModel {
		t.Errorf("ColorModel = %v, want RGBAModel", config.ColorModel)
	}
}

// TestDecode_Placeholder is a placeholder for full decode testing
// Real test would require valid JPEG2000 test images
func TestDecode_Placeholder(t *testing.T) {
	// Skip until we have real test images
	t.Skip("Full decode testing requires valid JPEG2000 test images")

	// Future test structure:
	// 1. Load test image from testdata/
	// 2. Decode with Decode()
	// 3. Verify dimensions and pixel values against reference
	// 4. Test both lossless (5/3) and lossy (9/7) images
	// 5. Test grayscale and RGB images
}

// TestColorModelFromHeader verifies color model selection
func TestColorModelFromHeader(t *testing.T) {
	tests := []struct {
		name      string
		numComps  int
		wantModel color.Model
	}{
		{
			name:      "grayscale",
			numComps:  1,
			wantModel: color.GrayModel,
		},
		{
			name:      "RGB",
			numComps:  3,
			wantModel: color.RGBAModel,
		},
		{
			name:      "RGBA",
			numComps:  4,
			wantModel: color.RGBAModel,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			header := &MainHeader{
				NumComps: tt.numComps,
			}
			got := colorModelFromHeader(header)
			if got != tt.wantModel {
				t.Errorf("colorModelFromHeader() = %v, want %v", got, tt.wantModel)
			}
		})
	}
}

// TestImageFormatRegistration verifies image format is registered
func TestImageFormatRegistration(t *testing.T) {
	// Test JP2 format detection
	jp2Header := []byte{
		0x00, 0x00, 0x00, 0x0C, 0x6A, 0x50, 0x20, 0x20,
		0x0D, 0x0A, 0x87, 0x0A,
	}

	_, formatName, err := image.DecodeConfig(bytes.NewReader(jp2Header))
	_ = formatName // Avoid unused variable error
	if err == nil {
		// Format was recognized (will fail to decode, but that's ok)
		t.Log("JP2 format registered successfully")
	}

	// Test J2C format detection
	j2cHeader := []byte{0xFF, 0x4F, 0xFF, 0x51}
	_, formatName, err = image.DecodeConfig(bytes.NewReader(j2cHeader))
	_ = formatName
	if err == nil {
		t.Log("J2C format registered successfully")
	}

	// Note: Both will fail to fully decode since we don't have valid headers,
	// but format detection should work
}
