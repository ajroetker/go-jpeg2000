package jpeg2000

import (
	"encoding/binary"
	"fmt"
)

// JP2 box type constants
const (
	jp2BoxSignature    = 0x6A502020 // "jP  " - JP2 signature
	jp2BoxFileType     = 0x66747970 // "ftyp" - File type
	jp2BoxHeader       = 0x6A703268 // "jp2h" - JP2 header (superbox)
	jp2BoxCodestream   = 0x6A703263 // "jp2c" - Contiguous codestream
	jp2BoxImageHeader  = 0x69686472 // "ihdr" - Image header
	jp2BoxColorSpec    = 0x636F6C72 // "colr" - Color specification
	jp2BoxResolution   = 0x72657320 // "res " - Resolution (superbox)
	jp2BoxCaptureRes   = 0x72657363 // "resc" - Capture resolution
	jp2BoxDisplayRes   = 0x72657364 // "resd" - Display resolution
	jp2BoxPalette      = 0x70636C72 // "pclr" - Palette
	jp2BoxComponentMap = 0x636D6170 // "cmap" - Component mapping
	jp2BoxChannelDef   = 0x63646566 // "cdef" - Channel definition
	jp2BoxBitsPerComp  = 0x62706363 // "bpcc" - Bits per component
	jp2BoxUUID         = 0x75756964 // "uuid" - UUID
	jp2BoxUUIDInfo     = 0x75696E66 // "uinf" - UUID info
	jp2BoxXML          = 0x786D6C20 // "xml " - XML
)

// JP2ColorMethod defines the color specification method
type JP2ColorMethod uint8

const (
	JP2ColorEnumerated JP2ColorMethod = 1 // Enumerated colorspace
	JP2ColorICC        JP2ColorMethod = 2 // Restricted ICC profile
	JP2ColorICCAny     JP2ColorMethod = 3 // Any ICC profile (JP2 extensions)
)

// JP2ColorSpace defines enumerated color spaces per ITU-T T.800 Table I.1
type JP2ColorSpace uint32

const (
	JP2ColorUnknown   JP2ColorSpace = 0
	JP2ColorBiLevel1  JP2ColorSpace = 1  // bi-level (1 = black)
	JP2ColorYCbCr1    JP2ColorSpace = 3  // YCbCr (ITU-R BT.601-5)
	JP2ColorYCbCr2    JP2ColorSpace = 4  // YCbCr (ITU-R BT.601-5 based on Rec.601)
	JP2ColorYCbCr3    JP2ColorSpace = 5  // YCbCr (ITU-R BT.709)
	JP2ColorPhotoYCC  JP2ColorSpace = 9  // PhotoYCC
	JP2ColorCMY       JP2ColorSpace = 11 // CMY
	JP2ColorCMYK      JP2ColorSpace = 12 // CMYK
	JP2ColorYCCK      JP2ColorSpace = 13 // YCCK
	JP2ColorCIELab    JP2ColorSpace = 14 // CIELab (default illuminant/range)
	JP2ColorBiLevel2  JP2ColorSpace = 15 // bi-level (1 = white)
	JP2ColorSRGB      JP2ColorSpace = 16 // sRGB
	JP2ColorGrayscale JP2ColorSpace = 17 // Greyscale
	JP2ColorSYCC      JP2ColorSpace = 18 // sYCC (standard YCbCr)
	JP2ColorCIEJab    JP2ColorSpace = 19 // CIEJab
	JP2ColorESRGB     JP2ColorSpace = 20 // e-sRGB
	JP2ColorROMM      JP2ColorSpace = 21 // ROMM-RGB
	JP2ColorYPbPr60   JP2ColorSpace = 22 // YPbPr (1125/60)
	JP2ColorYPbPr50   JP2ColorSpace = 23 // YPbPr (1250/50)
	JP2ColorESYCC     JP2ColorSpace = 24 // e-sYCC
)

// JP2Metadata holds parsed JP2 container information
type JP2Metadata struct {
	// From ihdr box
	Height      uint32
	Width       uint32
	NumComps    uint16
	BitDepth    int   // From BPC field (bit depth - 1, plus sign bit)
	Signed      bool  // Component sample signedness
	Compression uint8 // Should be 7 for JP2
	UnkC        uint8 // Colorspace unknown flag
	IPR         uint8 // Intellectual property flag

	// From colr box
	ColorMethod JP2ColorMethod
	Precedence  uint8
	Approx      uint8
	ColorSpace  JP2ColorSpace // Only valid if ColorMethod == JP2ColorEnumerated
	ICCProfile  []byte        // ICC profile data if ColorMethod == JP2ColorICC

	// From res box (optional)
	CaptureResX float64 // Capture resolution in grid points per meter
	CaptureResY float64
	DisplayResX float64 // Display resolution in grid points per meter
	DisplayResY float64

	// From pclr box (palette)
	Palette *JP2Palette

	// From cmap box (component mapping)
	ComponentMap []JP2ComponentMapping

	// From cdef box (channel definition)
	ChannelDefs []JP2ChannelDef

	// Metadata boxes (optional)
	UUIDData [][]byte // Raw UUID box contents
	XMLData  [][]byte // Raw XML box contents

	// Internal tracking
	hasIHDR bool
	hasCOLR bool
}

// JP2Palette holds palette box (pclr) data per ITU-T T.800 I.5.3.4
type JP2Palette struct {
	NumEntries int     // Number of palette entries (NE)
	NumColumns int     // Number of columns/channels (NPC)
	BitDepths  []int   // Bit depth per column (Bi)
	Signed     []bool  // Whether each column is signed
	Entries    [][]int // [entry][column] lookup values
}

// JP2ComponentMapping holds component mapping box (cmap) data per ITU-T T.800 I.5.3.5
type JP2ComponentMapping struct {
	Component   int // Codestream component index (CMP)
	MappingType int // 0=direct, 1=palette mapping (MTYP)
	PaletteCol  int // Palette column index (PCOL), only used if MappingType=1
}

// JP2ChannelDef holds channel definition box (cdef) data per ITU-T T.800 I.5.3.6
type JP2ChannelDef struct {
	Channel     int // Channel number (Cn)
	Type        int // Channel type: 0=color, 1=opacity, 2=premult opacity, 0xFFFF=unspecified
	Association int // Color association: 1=R/Y, 2=G/Cb, 3=B/Cr, 0=whole image, 0xFFFF=unspecified
}

// IsYCbCr returns true if the color space is a YCbCr variant
func (m *JP2Metadata) IsYCbCr() bool {
	if m == nil || m.ColorMethod != JP2ColorEnumerated {
		return false
	}
	switch m.ColorSpace {
	case JP2ColorYCbCr1, JP2ColorYCbCr2, JP2ColorYCbCr3,
		JP2ColorSYCC, JP2ColorESYCC, JP2ColorPhotoYCC,
		JP2ColorYPbPr60, JP2ColorYPbPr50:
		return true
	default:
		return false
	}
}

// IsGrayscale returns true if the color space is grayscale
func (m *JP2Metadata) IsGrayscale() bool {
	if m == nil || m.ColorMethod != JP2ColorEnumerated {
		return false
	}
	return m.ColorSpace == JP2ColorGrayscale ||
		m.ColorSpace == JP2ColorBiLevel1 ||
		m.ColorSpace == JP2ColorBiLevel2
}

// IsSRGB returns true if the color space is sRGB
func (m *JP2Metadata) IsSRGB() bool {
	if m == nil || m.ColorMethod != JP2ColorEnumerated {
		return false
	}
	return m.ColorSpace == JP2ColorSRGB || m.ColorSpace == JP2ColorESRGB
}

// parseJP2Container parses all JP2 boxes and returns metadata and codestream
func parseJP2Container(data []byte) (*JP2Metadata, []byte, error) {
	if len(data) < 12 {
		return nil, nil, fmt.Errorf("data too short for JP2 container")
	}

	meta := &JP2Metadata{}
	var codestream []byte

	pos := 0
	for pos+8 <= len(data) {
		boxLen, boxType, headerLen, err := parseBoxHeader(data, pos)
		if err != nil {
			break
		}

		// Handle box extends to end of file
		if boxLen == 0 {
			boxLen = len(data) - pos
		}

		// Sanity check
		if boxLen < headerLen || pos+boxLen > len(data) {
			break
		}

		boxData := data[pos+headerLen : pos+boxLen]

		switch boxType {
		case jp2BoxHeader:
			// JP2 header is a superbox - parse its contents
			parseJP2HeaderBox(boxData, meta)

		case jp2BoxImageHeader:
			parseImageHeader(boxData, meta)

		case jp2BoxColorSpec:
			parseColorSpec(boxData, meta)

		case jp2BoxResolution:
			parseResolutionBox(boxData, meta)

		case jp2BoxCodestream:
			codestream = boxData

		case jp2BoxUUID:
			if len(boxData) > 0 {
				uuidCopy := make([]byte, len(boxData))
				copy(uuidCopy, boxData)
				meta.UUIDData = append(meta.UUIDData, uuidCopy)
			}

		case jp2BoxXML:
			if len(boxData) > 0 {
				xmlCopy := make([]byte, len(boxData))
				copy(xmlCopy, boxData)
				meta.XMLData = append(meta.XMLData, xmlCopy)
			}
		}

		pos += boxLen
	}

	return meta, codestream, nil
}

// parseBoxHeader parses a JP2 box header and returns length, type, and header size
func parseBoxHeader(data []byte, pos int) (boxLen int, boxType uint32, headerLen int, err error) {
	if pos+8 > len(data) {
		return 0, 0, 0, fmt.Errorf("insufficient data for box header")
	}

	boxLen = int(binary.BigEndian.Uint32(data[pos:]))
	boxType = binary.BigEndian.Uint32(data[pos+4:])
	headerLen = 8

	if boxLen == 1 {
		// Extended length (8-byte length field)
		if pos+16 > len(data) {
			return 0, 0, 0, fmt.Errorf("insufficient data for extended box length")
		}
		boxLen = int(binary.BigEndian.Uint64(data[pos+8:]))
		headerLen = 16
	} else if boxLen == 0 {
		// Box extends to end of file - will be handled by caller
		headerLen = 8
	} else if boxLen < 8 {
		return 0, 0, 0, fmt.Errorf("invalid box length: %d", boxLen)
	}

	return boxLen, boxType, headerLen, nil
}

// parseJP2HeaderBox parses the jp2h superbox contents
func parseJP2HeaderBox(data []byte, meta *JP2Metadata) {
	pos := 0
	for pos+8 <= len(data) {
		boxLen, boxType, headerLen, err := parseBoxHeader(data, pos)
		if err != nil {
			break
		}

		if boxLen == 0 {
			boxLen = len(data) - pos
		}

		if boxLen < headerLen || pos+boxLen > len(data) {
			break
		}

		boxData := data[pos+headerLen : pos+boxLen]

		switch boxType {
		case jp2BoxImageHeader:
			parseImageHeader(boxData, meta)
		case jp2BoxColorSpec:
			parseColorSpec(boxData, meta)
		case jp2BoxResolution:
			parseResolutionBox(boxData, meta)
		case jp2BoxPalette:
			parsePaletteBox(boxData, meta)
		case jp2BoxComponentMap:
			parseComponentMapBox(boxData, meta)
		case jp2BoxChannelDef:
			parseChannelDefBox(boxData, meta)
		}

		pos += boxLen
	}
}

// parseImageHeader parses the ihdr box
func parseImageHeader(data []byte, meta *JP2Metadata) {
	if len(data) < 14 {
		return
	}

	meta.Height = binary.BigEndian.Uint32(data[0:4])
	meta.Width = binary.BigEndian.Uint32(data[4:8])
	meta.NumComps = binary.BigEndian.Uint16(data[8:10])

	// BPC: bit 7 is sign, bits 0-6 are (bit depth - 1)
	bpc := data[10]
	meta.Signed = (bpc & 0x80) != 0
	meta.BitDepth = int((bpc & 0x7F) + 1)

	meta.Compression = data[11]
	meta.UnkC = data[12]
	meta.IPR = data[13]

	meta.hasIHDR = true
}

// parseColorSpec parses the colr box
func parseColorSpec(data []byte, meta *JP2Metadata) {
	if len(data) < 3 {
		return
	}

	meta.ColorMethod = JP2ColorMethod(data[0])
	meta.Precedence = data[1]
	meta.Approx = data[2]

	switch meta.ColorMethod {
	case JP2ColorEnumerated:
		if len(data) >= 7 {
			meta.ColorSpace = JP2ColorSpace(binary.BigEndian.Uint32(data[3:7]))
		}

	case JP2ColorICC, JP2ColorICCAny:
		if len(data) > 3 {
			meta.ICCProfile = make([]byte, len(data)-3)
			copy(meta.ICCProfile, data[3:])
		}
	}

	meta.hasCOLR = true
}

// parseResolutionBox parses the res superbox (contains resc and/or resd)
func parseResolutionBox(data []byte, meta *JP2Metadata) {
	pos := 0
	for pos+8 <= len(data) {
		boxLen, boxType, headerLen, err := parseBoxHeader(data, pos)
		if err != nil {
			break
		}

		if boxLen == 0 {
			boxLen = len(data) - pos
		}

		if boxLen < headerLen || pos+boxLen > len(data) {
			break
		}

		boxData := data[pos+headerLen : pos+boxLen]

		switch boxType {
		case jp2BoxCaptureRes:
			parseResolutionData(boxData, &meta.CaptureResY, &meta.CaptureResX)
		case jp2BoxDisplayRes:
			parseResolutionData(boxData, &meta.DisplayResY, &meta.DisplayResX)
		}

		pos += boxLen
	}
}

// parseResolutionData parses resolution box content (resc or resd)
// Format: VRcN(2), VRcD(2), HRcN(2), HRcD(2), VRcE(1), HRcE(1)
func parseResolutionData(data []byte, vRes, hRes *float64) {
	if len(data) < 10 {
		return
	}

	vrcN := binary.BigEndian.Uint16(data[0:2])
	vrcD := binary.BigEndian.Uint16(data[2:4])
	hrcN := binary.BigEndian.Uint16(data[4:6])
	hrcD := binary.BigEndian.Uint16(data[6:8])
	vrcE := int8(data[8])
	hrcE := int8(data[9])

	// Resolution in grid points per meter
	if vrcD > 0 {
		*vRes = float64(vrcN) / float64(vrcD) * pow10(int(vrcE))
	}
	if hrcD > 0 {
		*hRes = float64(hrcN) / float64(hrcD) * pow10(int(hrcE))
	}
}

// parsePaletteBox parses the pclr (palette) box per ITU-T T.800 I.5.3.4.
// Format: NE(2) NPC(1) Bi(NPC bytes) C_ij(NE*NPC entries)
func parsePaletteBox(data []byte, meta *JP2Metadata) {
	if len(data) < 3 {
		return
	}

	ne := int(binary.BigEndian.Uint16(data[0:2]))
	npc := int(data[2])
	if npc == 0 || len(data) < 3+npc {
		return
	}

	pal := &JP2Palette{
		NumEntries: ne,
		NumColumns: npc,
		BitDepths:  make([]int, npc),
		Signed:     make([]bool, npc),
		Entries:    make([][]int, ne),
	}

	for i := range npc {
		b := data[3+i]
		pal.Signed[i] = (b & 0x80) != 0
		pal.BitDepths[i] = int(b&0x7F) + 1
	}

	offset := 3 + npc
	for i := range ne {
		pal.Entries[i] = make([]int, npc)
		for j := range npc {
			bd := pal.BitDepths[j]
			if bd <= 8 {
				if offset >= len(data) {
					return
				}
				val := int(data[offset])
				if pal.Signed[j] && val >= (1<<(bd-1)) {
					val -= (1 << bd)
				}
				pal.Entries[i][j] = val
				offset++
			} else {
				if offset+1 >= len(data) {
					return
				}
				val := int(binary.BigEndian.Uint16(data[offset : offset+2]))
				if pal.Signed[j] && val >= (1<<(bd-1)) {
					val -= (1 << bd)
				}
				pal.Entries[i][j] = val
				offset += 2
			}
		}
	}

	meta.Palette = pal
}

// parseComponentMapBox parses the cmap (component mapping) box per ITU-T T.800 I.5.3.5.
// Format: repeated CMP(2) MTYP(1) PCOL(1)
func parseComponentMapBox(data []byte, meta *JP2Metadata) {
	n := len(data) / 4
	if n == 0 {
		return
	}

	meta.ComponentMap = make([]JP2ComponentMapping, n)
	for i := range n {
		meta.ComponentMap[i] = JP2ComponentMapping{
			Component:   int(binary.BigEndian.Uint16(data[i*4 : i*4+2])),
			MappingType: int(data[i*4+2]),
			PaletteCol:  int(data[i*4+3]),
		}
	}
}

// parseChannelDefBox parses the cdef (channel definition) box per ITU-T T.800 I.5.3.6.
// Format: N(2) repeated Cn(2) Typ(2) Asoc(2)
func parseChannelDefBox(data []byte, meta *JP2Metadata) {
	if len(data) < 2 {
		return
	}

	n := int(binary.BigEndian.Uint16(data[0:2]))
	if len(data) < 2+n*6 {
		return
	}

	meta.ChannelDefs = make([]JP2ChannelDef, n)
	for i := range n {
		meta.ChannelDefs[i] = JP2ChannelDef{
			Channel:     int(binary.BigEndian.Uint16(data[2+i*6 : 4+i*6])),
			Type:        int(binary.BigEndian.Uint16(data[4+i*6 : 6+i*6])),
			Association: int(binary.BigEndian.Uint16(data[6+i*6 : 8+i*6])),
		}
	}
}

// NeedsChannelReorder returns true if cdef specifies a non-identity channel mapping
// for color channels (e.g., BGR instead of RGB).
func (m *JP2Metadata) NeedsChannelReorder() bool {
	if m == nil || len(m.ChannelDefs) == 0 {
		return false
	}
	for _, cd := range m.ChannelDefs {
		if cd.Type == 0 && cd.Channel != cd.Association-1 {
			return true
		}
	}
	return false
}

// GetChannelOrder returns the output channel order based on cdef associations.
// Returns a mapping where result[outputIndex] = inputChannelIndex.
// For RGB output: association 1=R, 2=G, 3=B.
func (m *JP2Metadata) GetChannelOrder() []int {
	if m == nil || len(m.ChannelDefs) == 0 {
		return nil
	}

	// Find max association to determine output size
	maxAssoc := 0
	for _, cd := range m.ChannelDefs {
		if cd.Type == 0 && cd.Association > 0 && cd.Association != 0xFFFF {
			if cd.Association > maxAssoc {
				maxAssoc = cd.Association
			}
		}
	}
	if maxAssoc == 0 {
		return nil
	}

	order := make([]int, maxAssoc)
	for i := range order {
		order[i] = i // default: identity
	}

	for _, cd := range m.ChannelDefs {
		if cd.Type == 0 && cd.Association > 0 && cd.Association <= maxAssoc {
			order[cd.Association-1] = cd.Channel
		}
	}

	return order
}

// pow10 returns 10^n
func pow10(n int) float64 {
	result := 1.0
	if n > 0 {
		for range n {
			result *= 10
		}
	} else {
		for i := 0; i > n; i-- {
			result /= 10
		}
	}
	return result
}
