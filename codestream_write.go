package jpeg2000

import (
	"encoding/binary"
	"fmt"
	"io"
)

// CodestreamWriter writes a JPEG2000 codestream to an io.Writer.
// It produces valid codestreams conforming to ITU-T T.800 by writing
// the required marker segments (SOC, SIZ, COD, QCD) and tile data.
type CodestreamWriter struct {
	w      io.Writer
	header *MainHeader
}

// NewCodestreamWriter creates a new codestream writer that writes to w
// using the image parameters from header.
func NewCodestreamWriter(w io.Writer, header *MainHeader) *CodestreamWriter {
	return &CodestreamWriter{
		w:      w,
		header: header,
	}
}

// WriteMainHeader writes the main header of a JPEG2000 codestream.
// This includes the SOC marker followed by SIZ, COD, and QCD marker segments.
// Per ITU-T T.800, these must appear in this order before any tile-parts.
func (cw *CodestreamWriter) WriteMainHeader() error {
	if err := cw.writeSOC(); err != nil {
		return fmt.Errorf("write SOC: %w", err)
	}
	if err := cw.writeSIZ(); err != nil {
		return fmt.Errorf("write SIZ: %w", err)
	}
	if err := cw.writeCOD(); err != nil {
		return fmt.Errorf("write COD: %w", err)
	}
	if err := cw.writeQCD(); err != nil {
		return fmt.Errorf("write QCD: %w", err)
	}
	return nil
}

// WriteTilePart writes a complete tile-part consisting of an SOT marker segment,
// the SOD marker, and the tile bitstream data. tileIdx is the zero-based tile
// index and tileData contains the pre-assembled packet data for this tile.
//
// The tile-part length (Psot) in the SOT marker includes the SOT marker itself
// (2 bytes), the SOT segment (10 bytes), the SOD marker (2 bytes), and the
// tile data length.
func (cw *CodestreamWriter) WriteTilePart(tileIdx int, tileData []byte) error {
	if err := cw.writeSOT(tileIdx, tileData); err != nil {
		return fmt.Errorf("write SOT for tile %d: %w", tileIdx, err)
	}
	if err := cw.writeSOD(tileData); err != nil {
		return fmt.Errorf("write SOD for tile %d: %w", tileIdx, err)
	}
	return nil
}

// WriteEOC writes the end-of-codestream marker (EOC).
func (cw *CodestreamWriter) WriteEOC() error {
	return writeMarker(cw.w, markerEOC)
}

// writeSOC writes the Start of Codestream marker (2 bytes).
func (cw *CodestreamWriter) writeSOC() error {
	return writeMarker(cw.w, markerSOC)
}

// writeSIZ writes the Image and Tile Size marker segment.
// Per ITU-T T.800 Table A.9, the SIZ marker provides the uncompressed image
// and tile dimensions, the number of components, and component sub-sampling
// factors needed for decoding.
func (cw *CodestreamWriter) writeSIZ() error {
	h := cw.header

	// SIZ segment length: Lsiz = 38 + 3*Csiz
	// (Lsiz(2) + Rsiz(2) + Xsiz(4) + Ysiz(4) + XOsiz(4) + YOsiz(4) +
	//  XTsiz(4) + YTsiz(4) + XTOsiz(4) + YTOsiz(4) + Csiz(2) + 3*Csiz)
	segLen := 38 + 3*h.NumComps

	if err := writeMarker(cw.w, markerSIZ); err != nil {
		return err
	}
	if err := writeUint16(cw.w, uint16(segLen)); err != nil {
		return err
	}

	// Rsiz: Profile 0 (unrestricted)
	if err := writeUint16(cw.w, 0x0000); err != nil {
		return err
	}

	// Xsiz, Ysiz: image dimensions
	if err := writeUint32(cw.w, uint32(h.Width)); err != nil {
		return err
	}
	if err := writeUint32(cw.w, uint32(h.Height)); err != nil {
		return err
	}

	// XOsiz, YOsiz: image offset
	if err := writeUint32(cw.w, uint32(h.XOsiz)); err != nil {
		return err
	}
	if err := writeUint32(cw.w, uint32(h.YOsiz)); err != nil {
		return err
	}

	// XTsiz, YTsiz: tile size
	tileWidth := h.TileWidth
	if tileWidth == 0 {
		tileWidth = h.Width
	}
	tileHeight := h.TileHeight
	if tileHeight == 0 {
		tileHeight = h.Height
	}
	if err := writeUint32(cw.w, uint32(tileWidth)); err != nil {
		return err
	}
	if err := writeUint32(cw.w, uint32(tileHeight)); err != nil {
		return err
	}

	// XTOsiz, YTOsiz: tile grid offset
	if err := writeUint32(cw.w, uint32(h.XTOsiz)); err != nil {
		return err
	}
	if err := writeUint32(cw.w, uint32(h.YTOsiz)); err != nil {
		return err
	}

	// Csiz: number of components
	if err := writeUint16(cw.w, uint16(h.NumComps)); err != nil {
		return err
	}

	// Per-component parameters: Ssiz, XRsiz, YRsiz
	for i := 0; i < h.NumComps; i++ {
		// Ssiz: bit depth and sign
		// bit 7 = signed, bits 0-6 = (bit depth - 1)
		bitDepth := 8
		if i < len(h.BitDepth) && h.BitDepth[i] > 0 {
			bitDepth = h.BitDepth[i]
		}
		signed := false
		if i < len(h.Signed) {
			signed = h.Signed[i]
		}
		ssiz := byte(bitDepth - 1)
		if signed {
			ssiz |= 0x80
		}
		if _, err := cw.w.Write([]byte{ssiz}); err != nil {
			return err
		}

		// XRsiz: horizontal sample separation
		xrsiz := byte(1)
		if i < len(h.XRsiz) && h.XRsiz[i] > 0 {
			xrsiz = byte(h.XRsiz[i])
		}
		if _, err := cw.w.Write([]byte{xrsiz}); err != nil {
			return err
		}

		// YRsiz: vertical sample separation
		yrsiz := byte(1)
		if i < len(h.YRsiz) && h.YRsiz[i] > 0 {
			yrsiz = byte(h.YRsiz[i])
		}
		if _, err := cw.w.Write([]byte{yrsiz}); err != nil {
			return err
		}
	}

	return nil
}

// writeCOD writes the Coding Style Default marker segment.
// Per ITU-T T.800 Table A.12, COD specifies the coding style parameters
// that apply to all components unless overridden by COC markers.
func (cw *CodestreamWriter) writeCOD() error {
	h := cw.header

	// Determine if we need custom precinct sizes
	hasCustomPrecincts := len(h.PrecinctSizes) > 0
	numResLevels := h.NumDecompLevels + 1

	// Segment length: Lcod = 12 (fixed fields) + optional precinct sizes
	// Fixed: Lcod(2) + Scod(1) + SGcod(4) + SPcod_fixed(5) = 12
	segLen := 12
	if hasCustomPrecincts {
		segLen += numResLevels
	}

	if err := writeMarker(cw.w, markerCOD); err != nil {
		return err
	}
	if err := writeUint16(cw.w, uint16(segLen)); err != nil {
		return err
	}

	// Scod: coding style flags
	// bit 0: custom precinct sizes
	// bit 1: SOP markers
	// bit 2: EPH markers
	var scod byte
	if hasCustomPrecincts {
		scod |= 0x01
	}
	if h.UsesSOP {
		scod |= 0x02
	}
	if h.UsesEPH {
		scod |= 0x04
	}
	if _, err := cw.w.Write([]byte{scod}); err != nil {
		return err
	}

	// SGcod: progression order (1 byte)
	if _, err := cw.w.Write([]byte{h.ProgressionOrder}); err != nil {
		return err
	}

	// SGcod: number of layers (2 bytes)
	numLayers := max(h.NumLayers, 1)
	if err := writeUint16(cw.w, uint16(numLayers)); err != nil {
		return err
	}

	// SGcod: MCT flag (1 byte)
	var mct byte
	if h.MCT {
		mct = 1
	}
	if _, err := cw.w.Write([]byte{mct}); err != nil {
		return err
	}

	// SPcod: number of decomposition levels (1 byte)
	if _, err := cw.w.Write([]byte{byte(h.NumDecompLevels)}); err != nil {
		return err
	}

	// SPcod: code block width exponent (1 byte)
	// Stored as (exponent - 2), so 64 = 2^6 -> stored as 4
	cbWidth := h.CodeBlockWidth
	if cbWidth == 0 {
		cbWidth = 64 // default
	}
	cbWidthExp := byte(ilog2(cbWidth) - 2)
	if _, err := cw.w.Write([]byte{cbWidthExp}); err != nil {
		return err
	}

	// SPcod: code block height exponent (1 byte)
	cbHeight := h.CodeBlockHeight
	if cbHeight == 0 {
		cbHeight = 64 // default
	}
	cbHeightExp := byte(ilog2(cbHeight) - 2)
	if _, err := cw.w.Write([]byte{cbHeightExp}); err != nil {
		return err
	}

	// SPcod: code block style (1 byte)
	if _, err := cw.w.Write([]byte{h.CodeBlockStyle}); err != nil {
		return err
	}

	// SPcod: wavelet transform (1 byte)
	// Codestream encoding: 0 = 9/7 irreversible, 1 = 5/3 reversible
	// Go types: Wavelet53 = 0 (iota), Wavelet97 = 1
	// So we must map from Go type to codestream value
	waveletByte := waveletTypeToByte(h.WaveletFilter)
	if _, err := cw.w.Write([]byte{waveletByte}); err != nil {
		return err
	}

	// SPcod: precinct sizes (if Scod bit 0 is set)
	// One byte per resolution level: PPx (low 4 bits) | PPy (high 4 bits)
	if hasCustomPrecincts {
		for r := range numResLevels {
			var ppx, ppy int
			if r < len(h.PrecinctSizes) {
				ppx = h.PrecinctSizes[r][0]
				ppy = h.PrecinctSizes[r][1]
			} else {
				ppx = 15 // default
				ppy = 15
			}
			ppByte := byte(ppx&0x0F) | byte((ppy&0x0F)<<4)
			if _, err := cw.w.Write([]byte{ppByte}); err != nil {
				return err
			}
		}
	}

	return nil
}

// writeQCD writes the Quantization Default marker segment.
// Per ITU-T T.800 Table A.19, QCD specifies the default quantization
// parameters for all components.
func (cw *CodestreamWriter) writeQCD() error {
	h := cw.header

	if err := writeMarker(cw.w, markerQCD); err != nil {
		return err
	}

	// Sqcd byte: quantization style (bits 0-4) | guard bits (bits 5-7)
	sqcd := (h.QuantStyle & 0x1F) | byte(h.GuardBits<<5)

	switch h.QuantStyle {
	case 0:
		// No quantization (reversible 5/3): one byte per subband
		// Each byte: exponent(5 bits) << 3
		numSubbands := len(h.Exponents)
		if numSubbands == 0 {
			// Default: 3*NumDecompLevels + 1 subbands
			numSubbands = 3*h.NumDecompLevels + 1
		}

		// Lqcd = 3 + numSubbands (Lqcd(2) + Sqcd(1) + numSubbands)
		segLen := 3 + numSubbands
		if err := writeUint16(cw.w, uint16(segLen)); err != nil {
			return err
		}
		if _, err := cw.w.Write([]byte{sqcd}); err != nil {
			return err
		}

		for i := 0; i < numSubbands; i++ {
			exp := 0
			if i < len(h.Exponents) {
				exp = h.Exponents[i]
			}
			// 5-bit exponent shifted left by 3
			if _, err := cw.w.Write([]byte{byte(exp << 3)}); err != nil {
				return err
			}
		}

	case 1:
		// Scalar derived: single 2-byte value for the LL band
		// Lqcd = 3 + 2 = 5 (Lqcd(2) + Sqcd(1) + 2 bytes for base step)
		segLen := 5
		if err := writeUint16(cw.w, uint16(segLen)); err != nil {
			return err
		}
		if _, err := cw.w.Write([]byte{sqcd}); err != nil {
			return err
		}

		exp := 0
		if len(h.Exponents) > 0 {
			exp = h.Exponents[0]
		}
		mant := 0
		if len(h.Mantissas) > 0 {
			mant = h.Mantissas[0]
		}
		val := uint16(exp<<11) | uint16(mant&0x7FF)
		if err := writeUint16(cw.w, val); err != nil {
			return err
		}

	case 2:
		// Scalar expounded: two bytes per subband
		numSubbands := len(h.Exponents)
		if numSubbands == 0 {
			numSubbands = 3*h.NumDecompLevels + 1
		}

		// Lqcd = 3 + 2*numSubbands (Lqcd(2) + Sqcd(1) + 2*numSubbands)
		segLen := 3 + 2*numSubbands
		if err := writeUint16(cw.w, uint16(segLen)); err != nil {
			return err
		}
		if _, err := cw.w.Write([]byte{sqcd}); err != nil {
			return err
		}

		for i := 0; i < numSubbands; i++ {
			exp := 0
			if i < len(h.Exponents) {
				exp = h.Exponents[i]
			}
			mant := 0
			if i < len(h.Mantissas) {
				mant = h.Mantissas[i]
			}
			val := uint16(exp<<11) | uint16(mant&0x7FF)
			if err := writeUint16(cw.w, val); err != nil {
				return err
			}
		}

	default:
		return fmt.Errorf("unsupported quantization style: %d", h.QuantStyle)
	}

	return nil
}

// writeSOT writes the Start of Tile-Part marker segment.
// Per ITU-T T.800 Table A.7, the SOT marker segment is always 12 bytes:
// marker(2) + Lsot(2) + Isot(2) + Psot(4) + TPsot(1) + TNsot(1)
func (cw *CodestreamWriter) writeSOT(tileIdx int, tileData []byte) error {
	if err := writeMarker(cw.w, markerSOT); err != nil {
		return err
	}

	// Lsot: length of the SOT marker segment body (always 10)
	if err := writeUint16(cw.w, 10); err != nil {
		return err
	}

	// Isot: tile index
	if err := writeUint16(cw.w, uint16(tileIdx)); err != nil {
		return err
	}

	// Psot: total length of this tile-part from start of SOT marker
	// = SOT marker(2) + SOT body(10) + SOD marker(2) + tile data length
	psot := uint32(2 + 10 + 2 + len(tileData))
	if err := writeUint32(cw.w, psot); err != nil {
		return err
	}

	// TPsot: tile-part index (0 for first/only tile-part)
	if _, err := cw.w.Write([]byte{0}); err != nil {
		return err
	}

	// TNsot: total number of tile-parts for this tile (1 = single tile-part)
	if _, err := cw.w.Write([]byte{1}); err != nil {
		return err
	}

	return nil
}

// writeSOD writes the Start of Data marker followed by the tile bitstream data.
func (cw *CodestreamWriter) writeSOD(tileData []byte) error {
	if err := writeMarker(cw.w, markerSOD); err != nil {
		return err
	}
	if len(tileData) > 0 {
		if _, err := cw.w.Write(tileData); err != nil {
			return err
		}
	}
	return nil
}

// WriteJP2File writes a complete JP2 file wrapping a JPEG2000 codestream.
// The JP2 file format (ITU-T T.800 Annex I) consists of a sequence of boxes:
//   - Signature Box (jP): file identification
//   - FileType Box (ftyp): brand and compatibility
//   - JP2 Header Box (jp2h): image header and color specification
//   - Contiguous Codestream Box (jp2c): the actual JPEG2000 codestream
func WriteJP2File(w io.Writer, header *MainHeader, codestreamData []byte) error {
	if err := writeSignatureBox(w); err != nil {
		return fmt.Errorf("write signature box: %w", err)
	}
	if err := writeFileTypeBox(w); err != nil {
		return fmt.Errorf("write file type box: %w", err)
	}
	if err := writeJP2HeaderBox(w, header); err != nil {
		return fmt.Errorf("write JP2 header box: %w", err)
	}
	if err := writeCodestreamBox(w, codestreamData); err != nil {
		return fmt.Errorf("write codestream box: %w", err)
	}
	return nil
}

// writeSignatureBox writes the JP2 Signature Box (jP).
// Per ITU-T T.800 I.5.1, this is always exactly 12 bytes:
// Length(4) + Type(4) + Data(4) = 12 + "jP  " + 0x0D0A870A
func writeSignatureBox(w io.Writer) error {
	// Length: 12
	if err := writeUint32(w, 12); err != nil {
		return err
	}
	// Type: 'jP  ' (0x6A502020)
	if err := writeUint32(w, jp2BoxSignature); err != nil {
		return err
	}
	// Signature data: 0x0D0A870A
	if err := writeUint32(w, 0x0D0A870A); err != nil {
		return err
	}
	return nil
}

// writeFileTypeBox writes the FileType Box (ftyp).
// Per ITU-T T.800 I.5.2, this identifies the file as JP2:
// Length(4) + Type(4) + Brand(4) + MinVersion(4) + CL[0](4) = 20 bytes
func writeFileTypeBox(w io.Writer) error {
	// Length: 20
	if err := writeUint32(w, 20); err != nil {
		return err
	}
	// Type: 'ftyp' (0x66747970)
	if err := writeUint32(w, jp2BoxFileType); err != nil {
		return err
	}
	// Brand: 'jp2 ' (0x6A703220)
	if err := writeUint32(w, 0x6A703220); err != nil {
		return err
	}
	// MinVersion: 0
	if err := writeUint32(w, 0); err != nil {
		return err
	}
	// Compatibility list entry: 'jp2 '
	if err := writeUint32(w, 0x6A703220); err != nil {
		return err
	}
	return nil
}

// writeJP2HeaderBox writes the JP2 Header superbox (jp2h).
// Per ITU-T T.800 I.5.3, this contains the Image Header (ihdr) and
// Colour Specification (colr) boxes.
func writeJP2HeaderBox(w io.Writer, header *MainHeader) error {
	// Build the contents of the jp2h superbox: ihdr + colr
	ihdrLen := 22 // ihdr box is always 22 bytes
	colrLen := 15 // colr box with enumerated colorspace is always 15 bytes

	jp2hContentLen := ihdrLen + colrLen
	jp2hBoxLen := 8 + jp2hContentLen // box header(8) + content

	// JP2 Header Box header
	if err := writeUint32(w, uint32(jp2hBoxLen)); err != nil {
		return err
	}
	if err := writeUint32(w, jp2BoxHeader); err != nil {
		return err
	}

	// Image Header Box (ihdr)
	if err := writeImageHeaderBox(w, header); err != nil {
		return err
	}

	// Colour Specification Box (colr)
	if err := writeColorSpecBox(w, header); err != nil {
		return err
	}

	return nil
}

// writeImageHeaderBox writes the Image Header Box (ihdr).
// Per ITU-T T.800 I.5.3.1, the ihdr box is always 22 bytes:
// Length(4) + Type(4) + Height(4) + Width(4) + NC(2) + BPC(1) + C(1) + UnkC(1) + IPR(1)
func writeImageHeaderBox(w io.Writer, header *MainHeader) error {
	// Length: 22
	if err := writeUint32(w, 22); err != nil {
		return err
	}
	// Type: 'ihdr' (0x69686472)
	if err := writeUint32(w, jp2BoxImageHeader); err != nil {
		return err
	}
	// Height
	if err := writeUint32(w, uint32(header.Height)); err != nil {
		return err
	}
	// Width
	if err := writeUint32(w, uint32(header.Width)); err != nil {
		return err
	}
	// NC: number of components
	if err := writeUint16(w, uint16(header.NumComps)); err != nil {
		return err
	}

	// BPC: bits per component
	// If all components have the same bit depth and sign, use that value.
	// Otherwise, use 0xFF to indicate varying bit depths (bpcc box needed).
	bpc := computeBPC(header)
	if _, err := w.Write([]byte{bpc}); err != nil {
		return err
	}

	// C: compression type (7 = JPEG2000)
	if _, err := w.Write([]byte{7}); err != nil {
		return err
	}

	// UnkC: colorspace known (1) or unknown (0)
	// Set to 1 since we provide a colr box
	if _, err := w.Write([]byte{1}); err != nil {
		return err
	}

	// IPR: no intellectual property rights
	if _, err := w.Write([]byte{0}); err != nil {
		return err
	}

	return nil
}

// writeColorSpecBox writes the Colour Specification Box (colr).
// Per ITU-T T.800 I.5.3.3, this box specifies the colorspace.
// Length(4) + Type(4) + METH(1) + PREC(1) + APPROX(1) + EnumCS(4) = 15 bytes
func writeColorSpecBox(w io.Writer, header *MainHeader) error {
	// Length: 15
	if err := writeUint32(w, 15); err != nil {
		return err
	}
	// Type: 'colr' (0x636F6C72)
	if err := writeUint32(w, jp2BoxColorSpec); err != nil {
		return err
	}
	// METH: 1 = enumerated colorspace
	if _, err := w.Write([]byte{1}); err != nil {
		return err
	}
	// PREC: 0
	if _, err := w.Write([]byte{0}); err != nil {
		return err
	}
	// APPROX: 0
	if _, err := w.Write([]byte{0}); err != nil {
		return err
	}

	// EnumCS: colorspace identifier
	// 16 = sRGB (for 3+ component images)
	// 17 = greyscale (for 1 component images)
	var enumCS uint32
	if header.NumComps >= 3 {
		enumCS = uint32(JP2ColorSRGB) // 16
	} else {
		enumCS = uint32(JP2ColorGrayscale) // 17
	}
	if err := writeUint32(w, enumCS); err != nil {
		return err
	}

	return nil
}

// writeCodestreamBox writes the Contiguous Codestream Box (jp2c).
// Per ITU-T T.800 I.5.4, this box contains the entire JPEG2000 codestream
// from SOC to EOC.
func writeCodestreamBox(w io.Writer, codestreamData []byte) error {
	// Length: 8 + codestream length
	// Using 0 for the last box in the file is allowed per ITU-T T.800,
	// but we use the explicit length for maximum compatibility.
	boxLen := uint32(8 + len(codestreamData))
	if err := writeUint32(w, boxLen); err != nil {
		return err
	}
	// Type: 'jp2c' (0x6A703263)
	if err := writeUint32(w, jp2BoxCodestream); err != nil {
		return err
	}
	// Codestream data
	if len(codestreamData) > 0 {
		if _, err := w.Write(codestreamData); err != nil {
			return err
		}
	}
	return nil
}

// computeBPC computes the BPC field value for the ihdr box.
// If all components have the same bit depth and signedness, returns
// (bitDepth-1) | (signed ? 0x80 : 0x00). Otherwise returns 0xFF
// to indicate that bit depths vary across components.
func computeBPC(header *MainHeader) byte {
	if header.NumComps == 0 || len(header.BitDepth) == 0 {
		return 7 // default to 8-bit unsigned
	}

	bd := header.BitDepth[0]
	signed := false
	if len(header.Signed) > 0 {
		signed = header.Signed[0]
	}

	// Check if all components match
	allSame := true
	for i := 1; i < header.NumComps && i < len(header.BitDepth); i++ {
		if header.BitDepth[i] != bd {
			allSame = false
			break
		}
		if i < len(header.Signed) && header.Signed[i] != signed {
			allSame = false
			break
		}
	}

	if !allSame {
		return 0xFF
	}

	bpc := byte(bd - 1)
	if signed {
		bpc |= 0x80
	}
	return bpc
}

// waveletTypeToByte converts a WaveletType to the codestream byte value.
// Per ITU-T T.800 Table A.17:
//
//	0 = 9/7 irreversible filter
//	1 = 5/3 reversible filter
//
// In Go, the WaveletType iota order is Wavelet53=0, Wavelet97=1,
// which is the opposite of the codestream encoding.
func waveletTypeToByte(wt WaveletType) byte {
	switch wt {
	case Wavelet97:
		return 0
	case Wavelet53:
		return 1
	default:
		return 0 // default to 9/7
	}
}

// writeMarker writes a 2-byte JPEG2000 marker code in big-endian order.
func writeMarker(w io.Writer, marker uint16) error {
	return writeUint16(w, marker)
}

// writeUint16 writes a big-endian uint16 to w.
func writeUint16(w io.Writer, v uint16) error {
	var buf [2]byte
	binary.BigEndian.PutUint16(buf[:], v)
	_, err := w.Write(buf[:])
	return err
}

// writeUint32 writes a big-endian uint32 to w.
func writeUint32(w io.Writer, v uint32) error {
	var buf [4]byte
	binary.BigEndian.PutUint32(buf[:], v)
	_, err := w.Write(buf[:])
	return err
}
