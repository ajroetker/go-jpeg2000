package jpeg2000

import (
	"encoding/binary"
	"fmt"
	"math"
)

// JPEG2000 marker codes
const (
	markerSOC uint16 = 0xFF4F // Start of codestream
	markerSOT uint16 = 0xFF90 // Start of tile-part
	markerSOD uint16 = 0xFF93 // Start of data
	markerEOC uint16 = 0xFFD9 // End of codestream
	markerSIZ uint16 = 0xFF51 // Image and tile size
	markerCOD uint16 = 0xFF52 // Coding style default
	markerCOC uint16 = 0xFF53 // Coding style component
	markerRGN uint16 = 0xFF5E // Region of interest
	markerQCD uint16 = 0xFF5C // Quantization default
	markerQCC uint16 = 0xFF5D // Quantization component
	markerPOC uint16 = 0xFF5F // Progression order change
	markerTLM uint16 = 0xFF55 // Tile-part length
	markerPLM uint16 = 0xFF57 // Packet length, main header
	markerPLT uint16 = 0xFF58 // Packet length, tile-part header
	markerPPM uint16 = 0xFF60 // Packed packet headers, main header
	markerPPT uint16 = 0xFF61 // Packed packet headers, tile-part
	markerSOP uint16 = 0xFF91 // Start of packet
	markerEPH uint16 = 0xFF92 // End of packet header
	markerCRG uint16 = 0xFF63 // Component registration
	markerCOM uint16 = 0xFF64 // Comment
)

// MainHeader contains parsed image metadata
type MainHeader struct {
	// From SIZ marker
	Width      int    // Image width
	Height     int    // Image height
	XOsiz      int    // Horizontal offset
	YOsiz      int    // Vertical offset
	TileWidth  int    // Tile width
	TileHeight int    // Tile height
	XTOsiz     int    // Tile grid horizontal offset
	YTOsiz     int    // Tile grid vertical offset
	NumComps   int    // Number of components
	BitDepth   []int  // Bit depth per component
	Signed     []bool // Signed flag per component
	XRsiz      []int  // Horizontal sample separation
	YRsiz      []int  // Vertical sample separation

	// From COD marker
	ProgressionOrder byte // 0=LRCP, 1=RLCP, 2=RPCL, 3=PCRL, 4=CPRL
	NumLayers        int  // Number of quality layers
	MCT              bool // Multiple component transform used
	NumDecompLevels  int  // Number of decomposition levels
	CodeBlockWidth   int  // Code block width (power of 2)
	CodeBlockHeight  int  // Code block height (power of 2)
	CodeBlockStyle   byte // Code block coding style flags
	WaveletFilter    WaveletType
	UsesSOP          bool // SOP (Start of Packet) markers present
	UsesEPH          bool // EPH (End of Packet Header) markers present

	// Precinct sizes per resolution level (index 0 = resolution 0, lowest)
	// Each entry is [width_exponent, height_exponent] where size = 2^exponent
	// If nil, default precinct size (15,15) = 32768x32768 (effectively whole tile)
	PrecinctSizes [][2]int

	// From QCD marker (default quantization for all components)
	QuantStyle         byte      // 0=none, 1=scalar derived, 2=scalar expounded (may be overridden by QCC)
	OriginalQuantStyle byte      // Original QCD quantStyle before any QCC override
	GuardBits          int       // Number of guard bits
	StepSizes          []float64 // Quantization step sizes
	Exponents          []int     // Step size exponents (may be overridden by QCC for component 0)
	Mantissas          []int     // Step size mantissas (may be overridden by QCC for component 0)
	// Original QCD values before any QCC override (used for components without QCC)
	OriginalExponents []int // Original QCD exponents
	OriginalMantissas []int // Original QCD mantissas

	// Per-component quantization (from QCC markers)
	// If set, these override the default QCD values for specific components
	// Index = component number
	CompQuantStyle []byte  // Per-component quant style
	CompExponents  [][]int // Per-component exponents
	CompMantissas  [][]int // Per-component mantissas

	// Per-component coding style (from COC markers)
	// If set, these override the default COD values for specific components
	// Index = component number, value is number of decomposition levels
	// A value of -1 means use default (NumDecompLevels)
	CompDecompLevels    []int         // Per-component decomposition levels
	CompPrecinctSizes   [][][2]int    // Per-component precinct sizes [comp][res] (nil = use default)
	CompCodeBlockWidth  []int         // Per-component code block width (0 = use default)
	CompCodeBlockHeight []int         // Per-component code block height (0 = use default)
	CompCodeBlockStyle  []byte        // Per-component code block style (0xFF = use default)
	CompWaveletFilter   []WaveletType // Per-component wavelet filter (0xFF = use default)

	// From POC marker (if present, overrides COD progression order)
	POCEntries []POCEntry // Progression order change entries

	// From TLM marker (tile-part lengths for random access)
	TLMEntries []TLMEntry // Tile-part length entries

	// PPM (Packed Packet Headers in Main header) data
	// When present, packet headers are stored here instead of in the tile data stream
	// Per ITU-T T.800, multiple PPM segments are concatenated in order of Zppm index
	PPMHeaders []byte

	// Raw PPM segments indexed by Zppm for correct ordering.
	// After all PPM markers are parsed, these are sorted and concatenated into PPMHeaders.
	ppmSegments map[int][]byte

	// TilePartOrder records the tile index for each tile-part in codestream order.
	// Used by distributePPMHeaders to correctly assign PPM data per tile-part.
	TilePartOrder []int

	// Computed
	NumXTiles int
	NumYTiles int
	NumTiles  int
}

// POCEntry represents a single progression order change entry
type POCEntry struct {
	RSpoc  int  // Resolution level start
	CSpoc  int  // Component start
	LYEpoc int  // Layer end (exclusive)
	REpoc  int  // Resolution level end (exclusive)
	CEpoc  int  // Component end (exclusive)
	Ppoc   byte // Progression order for this entry
}

// TLMEntry represents a tile-part length entry from TLM marker.
// Per ITU-T T.800, TLM markers provide tile-part lengths for random access.
type TLMEntry struct {
	TileIndex   int // Tile index (0-65534, or -1 if not specified in TLM)
	TilePartLen int // Length of tile-part in bytes (includes SOT marker)
}

// ComponentWidth returns the width of a component accounting for subsampling
// Per ITU-T T.800 Annex B, equation B-2:
//
//	tcx0 = ceil(XOsiz / XRsiz)
//	tcx1 = ceil(Xsiz / XRsiz)
//	compWidth = tcx1 - tcx0
//
// This is NOT the same as ceil((Xsiz - XOsiz) / XRsiz) when XOsiz > 0.
func (h *MainHeader) ComponentWidth(comp int) int {
	if comp < 0 || comp >= h.NumComps || comp >= len(h.XRsiz) {
		return h.Width - h.XOsiz
	}
	xrsiz := h.XRsiz[comp]
	if xrsiz <= 0 {
		xrsiz = 1
	}
	tcx0 := (h.XOsiz + xrsiz - 1) / xrsiz // ceil(XOsiz / XRsiz)
	tcx1 := (h.Width + xrsiz - 1) / xrsiz // ceil(Xsiz / XRsiz)
	return tcx1 - tcx0
}

// ComponentHeight returns the height of a component accounting for subsampling
// Per ITU-T T.800 Annex B, equation B-2:
//
//	tcy0 = ceil(YOsiz / YRsiz)
//	tcy1 = ceil(Ysiz / YRsiz)
//	compHeight = tcy1 - tcy0
//
// This is NOT the same as ceil((Ysiz - YOsiz) / YRsiz) when YOsiz > 0.
func (h *MainHeader) ComponentHeight(comp int) int {
	if comp < 0 || comp >= h.NumComps || comp >= len(h.YRsiz) {
		return h.Height - h.YOsiz
	}
	yrsiz := h.YRsiz[comp]
	if yrsiz <= 0 {
		yrsiz = 1
	}
	tcy0 := (h.YOsiz + yrsiz - 1) / yrsiz  // ceil(YOsiz / YRsiz)
	tcy1 := (h.Height + yrsiz - 1) / yrsiz // ceil(Ysiz / YRsiz)
	return tcy1 - tcy0
}

// ComponentDecompLevels returns the number of decomposition levels for a component.
// This accounts for per-component COC marker overrides.
// If no override exists, returns the default from COD marker.
func (h *MainHeader) ComponentDecompLevels(comp int) int {
	if h.CompDecompLevels != nil && comp >= 0 && comp < len(h.CompDecompLevels) {
		if h.CompDecompLevels[comp] >= 0 {
			return h.CompDecompLevels[comp]
		}
	}
	return h.NumDecompLevels
}

// ComponentCodeBlockSize returns the code block width and height for a component.
// Per-component COC overrides take precedence over the default COD values.
func (h *MainHeader) ComponentCodeBlockSize(comp int) (int, int) {
	w := h.CodeBlockWidth
	ht := h.CodeBlockHeight
	if h.CompCodeBlockWidth != nil && comp >= 0 && comp < len(h.CompCodeBlockWidth) {
		if h.CompCodeBlockWidth[comp] > 0 {
			w = h.CompCodeBlockWidth[comp]
		}
	}
	if h.CompCodeBlockHeight != nil && comp >= 0 && comp < len(h.CompCodeBlockHeight) {
		if h.CompCodeBlockHeight[comp] > 0 {
			ht = h.CompCodeBlockHeight[comp]
		}
	}
	if w == 0 {
		w = 32
	}
	if ht == 0 {
		ht = 32
	}
	return w, ht
}

// ComponentCodeBlockStyle returns the code block style for a component.
func (h *MainHeader) ComponentCodeBlockStyle(comp int) byte {
	if h.CompCodeBlockStyle != nil && comp >= 0 && comp < len(h.CompCodeBlockStyle) {
		if h.CompCodeBlockStyle[comp] != 0xFF {
			return h.CompCodeBlockStyle[comp]
		}
	}
	return h.CodeBlockStyle
}

// ComponentWaveletFilter returns the wavelet filter for a component.
func (h *MainHeader) ComponentWaveletFilter(comp int) WaveletType {
	if h.CompWaveletFilter != nil && comp >= 0 && comp < len(h.CompWaveletFilter) {
		if h.CompWaveletFilter[comp] != 0xFF {
			return h.CompWaveletFilter[comp]
		}
	}
	return h.WaveletFilter
}

// PrecinctSizeForRes returns the precinct size exponents (PPx, PPy) for a given
// resolution level and component. Per-component COC overrides take precedence
// over the default COD precinct sizes.
func (h *MainHeader) PrecinctSizeForRes(res, comp int) (int, int) {
	// Check per-component override first
	if h.CompPrecinctSizes != nil && comp >= 0 && comp < len(h.CompPrecinctSizes) {
		if cps := h.CompPrecinctSizes[comp]; cps != nil && res >= 0 && res < len(cps) {
			return cps[res][0], cps[res][1]
		}
	}
	// Fall back to default
	if res >= 0 && res < len(h.PrecinctSizes) {
		return h.PrecinctSizes[res][0], h.PrecinctSizes[res][1]
	}
	return 15, 15 // default: 2^15 = 32768 (effectively whole tile)
}

// Tile represents a single tile
type Tile struct {
	Index     int
	X0, Y0    int // Tile origin in image coords
	X1, Y1    int // Tile end in image coords
	TileParts []*TilePart

	// ROI (Region of Interest) shift values per component
	// From RGN marker in tile-part header
	// If ROIShift[c] > 0, coefficients for component c need to be de-shifted
	ROIShift []int

	// Tile-specific coding style overrides from tile-part header COD marker
	// If HasTileCOD is true, use these instead of main header values
	HasTileCOD             bool
	TileProgressionOrder   byte // SGcod: progression order (0=LRCP, 1=RLCP, 2=RPCL, 3=PCRL, 4=CPRL)
	TileNumLayers          int  // SGcod: number of layers
	TileCodeBlockStyle     byte
	TileNumDecompLevels    int
	TileCodeBlockWidthExp  byte // SPcod: code block width exponent (offset from 2)
	TileCodeBlockHeightExp byte // SPcod: code block height exponent (offset from 2)
	TileWaveletFilter      WaveletType
	TilePrecinctSizes      [][2]byte // Per-resolution precinct sizes (PPx, PPy) from tile COD

	// Tile-specific quantization overrides from tile-part header QCD marker
	// If HasTileQCD is true, use these instead of main header QCD values
	HasTileQCD     bool
	TileQuantStyle byte
	TileGuardBits  int
	TileExponents  []int
	TileMantissas  []int

	// PPT (Packed Packet Headers) data
	// When present, packet headers are stored here instead of in the tile data stream
	// Per ITU-T T.800, multiple PPT segments are concatenated in order of Zppt index
	PPTHeaders []byte

	// Raw PPT segments indexed by Zppt for correct ordering.
	pptSegments map[int][]byte

	// POC (Progression Order Change) entries from tile-part header
	// When present, overrides main header POC/COD progression order for this tile
	POCEntries []POCEntry
}

// TilePart represents one tile-part
type TilePart struct {
	TileIndex int
	PartIndex int
	Length    int
	Data      []byte // Coded bitstream data
}

// SubbandType represents different subband orientations
type SubbandType int

const (
	SubbandLL SubbandType = iota
	SubbandHL
	SubbandLH
	SubbandHH
)

func (s SubbandType) String() string {
	switch s {
	case SubbandLL:
		return "LL"
	case SubbandHL:
		return "HL"
	case SubbandLH:
		return "LH"
	case SubbandHH:
		return "HH"
	default:
		return "UNKNOWN"
	}
}

// CodeBlock represents a code block within a subband
type CodeBlock struct {
	X, Y               int    // Position in subband
	Width, Height      int    // Dimensions
	Data               []byte // MQ-coded data
	NumPasses          int    // Number of coding passes
	ZeroBitPlanes      int    // Number of zero bit planes
	Lblock             int    // Log2 of block size indicator
	MagnitudeBitPlanes int    // Number of magnitude bit planes (Mb = GuardBits + Exp - 1)
	CodeBlockStyle     byte   // Codeblock coding style flags (from COD marker)
	SegmentLengths     []int  // Per-segment lengths (used when ERTERM is set for pass termination)
}

// parseCodestream parses a JPEG2000 codestream and returns header and tiles
func parseCodestream(data []byte) (*MainHeader, []*Tile, error) {
	if len(data) < 2 {
		return nil, nil, ErrTruncatedData
	}

	// Verify SOC marker
	marker := binary.BigEndian.Uint16(data[0:2])
	if marker != markerSOC {
		return nil, nil, ErrInvalidMarker
	}

	pos := 2
	header := &MainHeader{
		BitDepth: make([]int, 0),
		Signed:   make([]bool, 0),
	}
	var tiles []*Tile
	mainHeaderDone := false

	// Parse marker segments
	for pos+2 <= len(data) {
		marker := binary.BigEndian.Uint16(data[pos : pos+2])
		pos += 2

		switch marker {
		case markerEOC:
			// End of codestream
			finalizePPMSegments(header)
			finalizePPTSegments(tiles)
			return header, tiles, nil

		case markerSIZ:
			if mainHeaderDone {
				pos += int(binary.BigEndian.Uint16(data[pos : pos+2]))
				continue
			}
			// Image and tile size
			newPos, err := parseSIZ(data, pos, header)
			if err != nil {
				return nil, nil, err
			}
			pos = newPos

		case markerCOD:
			if mainHeaderDone {
				pos += int(binary.BigEndian.Uint16(data[pos : pos+2]))
				continue
			}
			// Coding style default
			newPos, err := parseCOD(data, pos, header)
			if err != nil {
				return nil, nil, err
			}
			pos = newPos

		case markerCOC:
			if mainHeaderDone {
				pos += int(binary.BigEndian.Uint16(data[pos : pos+2]))
				continue
			}
			// Coding style component - overrides COD for a specific component
			newPos, err := parseCOC(data, pos, header)
			if err != nil {
				return nil, nil, err
			}
			pos = newPos

		case markerQCD:
			if mainHeaderDone {
				pos += int(binary.BigEndian.Uint16(data[pos : pos+2]))
				continue
			}
			// Quantization default
			newPos, err := parseQCD(data, pos, header)
			if err != nil {
				return nil, nil, err
			}
			pos = newPos

		case markerQCC:
			if mainHeaderDone {
				pos += int(binary.BigEndian.Uint16(data[pos : pos+2]))
				continue
			}
			// Quantization component - overrides QCD for a specific component
			newPos, err := parseQCC(data, pos, header)
			if err != nil {
				return nil, nil, err
			}
			pos = newPos

		case markerPOC:
			if mainHeaderDone {
				pos += int(binary.BigEndian.Uint16(data[pos : pos+2]))
				continue
			}
			// Progression order change - overrides COD progression order
			newPos, err := parsePOC(data, pos, header)
			if err != nil {
				return nil, nil, err
			}
			pos = newPos

		case markerTLM:
			if mainHeaderDone {
				pos += int(binary.BigEndian.Uint16(data[pos : pos+2]))
				continue
			}
			// Tile-part length marker - provides tile-part lengths for random access
			newPos, err := parseTLM(data, pos, header)
			if err != nil {
				return nil, nil, err
			}
			pos = newPos

		case markerPPM:
			if mainHeaderDone {
				pos += int(binary.BigEndian.Uint16(data[pos : pos+2]))
				continue
			}
			// Packed packet headers in main header
			// Per ITU-T T.800, PPM segments contain packet headers for all tiles
			newPos, err := parsePPM(data, pos, header)
			if err != nil {
				return nil, nil, err
			}
			pos = newPos

		case markerSOT:
			mainHeaderDone = true
			// Start of tile-part
			if pos+8 > len(data) {
				return nil, nil, ErrTruncatedData
			}
			segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
			if pos+segLen > len(data) {
				return nil, nil, ErrTruncatedData
			}

			tileIndex := int(binary.BigEndian.Uint16(data[pos+2 : pos+4]))
			tilePartLen := int(binary.BigEndian.Uint32(data[pos+4 : pos+8]))

			// Calculate end of tile-part (SOT marker position - 2 + tilePartLen)
			sotMarkerPos := pos - 2 // We already moved past marker
			tilePartEnd := min(sotMarkerPos+tilePartLen, len(data))

			// Find or create tile
			var tile *Tile
			for _, t := range tiles {
				if t.Index == tileIndex {
					tile = t
					break
				}
			}
			if tile == nil {
				// Compute tile coordinates from tile index
				// tileIndex = ty * NumXTiles + tx
				tx := tileIndex % header.NumXTiles
				ty := tileIndex / header.NumXTiles

				// Tile bounds in image coordinates
				// Per ITU-T T.800, tile (p,q) covers:
				//   x: [max(XTOsiz + p*TileWidth, XOsiz), min(XTOsiz + (p+1)*TileWidth, Xsiz))
				//   y: [max(YTOsiz + q*TileHeight, YOsiz), min(YTOsiz + (q+1)*TileHeight, Ysiz))
				x0 := max(header.XTOsiz+tx*header.TileWidth, header.XOsiz)
				y0 := max(header.YTOsiz+ty*header.TileHeight, header.YOsiz)
				x1 := min(header.XTOsiz+(tx+1)*header.TileWidth, header.Width)
				y1 := min(header.YTOsiz+(ty+1)*header.TileHeight, header.Height)

				tile = &Tile{
					Index:     tileIndex,
					X0:        x0,
					Y0:        y0,
					X1:        x1,
					Y1:        y1,
					TileParts: make([]*TilePart, 0),
				}
				tiles = append(tiles, tile)
			}

			// Record tile-part order for PPM distribution
			header.TilePartOrder = append(header.TilePartOrder, tileIndex)

			pos += segLen

			// Scan for SOD marker, parsing relevant tile-part header markers
			// (COD, QCD, COC, QCC, RGN, POC, PPT, PLT, COM can appear in tile-part header)
			for pos+2 <= len(data) && pos < tilePartEnd {
				tphMarker := binary.BigEndian.Uint16(data[pos : pos+2])
				if tphMarker == markerSOD {
					pos += 2 // Skip SOD marker

					// Extract tile data - everything from here to end of tile-part
					dataEnd := tilePartEnd

					// Create tile part
					tilePart := &TilePart{
						TileIndex: tileIndex,
						PartIndex: len(tile.TileParts),
						Length:    dataEnd - pos,
						Data:      data[pos:dataEnd],
					}
					tile.TileParts = append(tile.TileParts, tilePart)

					pos = dataEnd
					break
				}

				// Parse COD marker to get tile-specific coding style
				// Per ITU-T T.800, COD in tile-part header overrides main header COD
				if tphMarker == markerCOD {
					pos += 2 // Skip marker
					newPos, err := parseTileCOD(data, pos, tile)
					if err != nil {
						// Non-fatal: just skip on error
						if pos+2 <= len(data) {
							tphSegLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
							if tphSegLen >= 2 && pos+tphSegLen <= len(data) {
								pos += tphSegLen
							}
						}
					} else {
						pos = newPos
					}
					continue
				}

				// Parse RGN marker to get ROI shift value for this tile
				if tphMarker == markerRGN {
					pos += 2 // Skip marker
					newPos, err := parseRGN(data, pos, header, tile)
					if err != nil {
						// Non-fatal: just skip on error
						if pos+2 <= len(data) {
							tphSegLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
							if tphSegLen >= 2 && pos+tphSegLen <= len(data) {
								pos += tphSegLen
							}
						}
					} else {
						pos = newPos
					}
					continue
				}

				// Parse QCD marker to get tile-specific quantization
				// Per ITU-T T.800, QCD in tile-part header overrides main header QCD
				if tphMarker == markerQCD {
					pos += 2 // Skip marker
					newPos, err := parseTileQCD(data, pos, tile)
					if err != nil {
						// Non-fatal: just skip on error
						if pos+2 <= len(data) {
							tphSegLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
							if tphSegLen >= 2 && pos+tphSegLen <= len(data) {
								pos += tphSegLen
							}
						}
					} else {
						pos = newPos
					}
					continue
				}

				// Parse PPT marker to get packed packet headers
				// Per ITU-T T.800, PPT segments contain packet headers separated from tile data
				if tphMarker == markerPPT {
					pos += 2 // Skip marker
					if pos+2 > len(data) {
						break
					}
					pptSegLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
					if pptSegLen < 3 || pos+pptSegLen > len(data) {
						pos += pptSegLen
						continue
					}
					// Zppt is the index of this PPT segment
					// Per ITU-T T.800, segments must be ordered by Zppt index
					zppt := int(data[pos+2])
					pptData := data[pos+3 : pos+pptSegLen]
					if tile.pptSegments == nil {
						tile.pptSegments = make(map[int][]byte)
					}
					tile.pptSegments[zppt] = append(tile.pptSegments[zppt], pptData...)
					pos += pptSegLen
					continue
				}

				// Parse POC marker to get tile-specific progression order
				// Per ITU-T T.800, POC in tile-part header overrides main header POC/COD
				if tphMarker == markerPOC {
					pos += 2 // Skip marker
					newPos, entries, err := parseTilePOC(data, pos, header)
					if err != nil {
						// Non-fatal: just skip on error
						if pos+2 <= len(data) {
							tphSegLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
							if tphSegLen >= 2 && pos+tphSegLen <= len(data) {
								pos += tphSegLen
							}
						}
					} else {
						tile.POCEntries = append(tile.POCEntries, entries...)
						pos = newPos
					}
					continue
				}

				// Skip other tile-part header markers
				pos += 2
				if pos+2 > len(data) {
					break
				}
				tphSegLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
				if tphSegLen < 2 || pos+tphSegLen > len(data) {
					break
				}
				pos += tphSegLen
			}

		default:
			// Reserved markers 0xFF30-0xFF3F have no length field per ITU-T T.800
			// Some encoders use these as delimiters
			if marker >= 0xFF30 && marker <= 0xFF3F {
				// No length field - just continue to next marker
				continue
			}

			// Unknown marker - try to skip by reading length
			if pos+2 > len(data) {
				return nil, nil, ErrTruncatedData
			}
			segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))

			// Sanity check: length should be reasonable
			// If not, this might be a delimiter marker we don't recognize - skip 0 bytes
			if segLen < 2 || segLen > 65535 || pos+segLen > len(data) {
				// Check if the next 2 bytes look like a valid marker
				if pos+2 <= len(data) {
					nextMarker := binary.BigEndian.Uint16(data[pos : pos+2])
					if nextMarker >= 0xFF00 && nextMarker <= 0xFFFF && nextMarker != 0xFFFF {
						// The "length" is actually another marker - previous marker had no length
						continue
					}
				}
				return nil, nil, fmt.Errorf("invalid marker segment length: %d", segLen)
			}
			pos += segLen
		}
	}

	// If we get here without finding EOC, still return what we have
	finalizePPMSegments(header)
	finalizePPTSegments(tiles)
	return header, tiles, nil
}

// parseSIZ parses image size marker (using binary.BigEndian directly)
func parseSIZ(data []byte, pos int, h *MainHeader) (int, error) {
	if pos+38 > len(data) {
		return pos, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if pos+segLen > len(data) {
		return pos, ErrTruncatedData
	}

	// Rsiz - capabilities (skip)
	// pos+2: Rsiz

	// Image dimensions
	h.Width = int(binary.BigEndian.Uint32(data[pos+4 : pos+8]))
	h.Height = int(binary.BigEndian.Uint32(data[pos+8 : pos+12]))

	// Image offset
	h.XOsiz = int(binary.BigEndian.Uint32(data[pos+12 : pos+16]))
	h.YOsiz = int(binary.BigEndian.Uint32(data[pos+16 : pos+20]))

	// Tile dimensions
	h.TileWidth = int(binary.BigEndian.Uint32(data[pos+20 : pos+24]))
	h.TileHeight = int(binary.BigEndian.Uint32(data[pos+24 : pos+28]))

	// Tile offset
	h.XTOsiz = int(binary.BigEndian.Uint32(data[pos+28 : pos+32]))
	h.YTOsiz = int(binary.BigEndian.Uint32(data[pos+32 : pos+36]))

	// Number of components
	h.NumComps = int(binary.BigEndian.Uint16(data[pos+36 : pos+38]))

	if h.NumComps < 1 || h.NumComps > 16384 {
		return pos, fmt.Errorf("%w: invalid component count: %d", ErrInvalidHeader, h.NumComps)
	}

	// Component parameters
	h.BitDepth = make([]int, h.NumComps)
	h.Signed = make([]bool, h.NumComps)
	h.XRsiz = make([]int, h.NumComps)
	h.YRsiz = make([]int, h.NumComps)

	for i := 0; i < h.NumComps; i++ {
		offset := pos + 38 + 3*i
		if offset+3 > len(data) {
			return pos, ErrTruncatedData
		}

		ssiz := data[offset]
		h.Signed[i] = (ssiz & 0x80) != 0
		h.BitDepth[i] = int((ssiz & 0x7F) + 1)
		h.XRsiz[i] = int(data[offset+1])
		h.YRsiz[i] = int(data[offset+2])
	}

	// Compute tile grid dimensions per ITU-T T.800:
	// numXtiles = ceil((Xsiz - XTOsiz) / XTsiz)
	// numYtiles = ceil((Ysiz - YTOsiz) / YTsiz)
	h.NumXTiles = (h.Width - h.XTOsiz + h.TileWidth - 1) / h.TileWidth
	h.NumYTiles = (h.Height - h.YTOsiz + h.TileHeight - 1) / h.TileHeight
	h.NumTiles = h.NumXTiles * h.NumYTiles

	if h.NumTiles < 1 || h.NumTiles > 65535 {
		return pos, fmt.Errorf("%w: tile count %d", ErrTooManyTiles, h.NumTiles)
	}

	return pos + segLen, nil
}

// parseCOD parses coding style default marker
func parseCOD(data []byte, pos int, h *MainHeader) (int, error) {
	if pos+12 > len(data) {
		return pos, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if pos+segLen > len(data) {
		return pos, ErrTruncatedData
	}

	// Scod - coding style flags
	// Bit 0: Defines precincts with sizes other than default (PPx=15, PPy=15)
	// Bit 1: Use SOP markers
	// Bit 2: Use EPH markers
	scod := data[pos+2]
	hasCustomPrecincts := (scod & 0x01) != 0
	h.UsesSOP = (scod & 0x02) != 0
	h.UsesEPH = (scod & 0x04) != 0

	// SGcod - progression order
	progOrder := data[pos+3]
	if progOrder > 4 {
		return pos, fmt.Errorf("%w: invalid progression order: %d", ErrInvalidHeader, progOrder)
	}
	h.ProgressionOrder = progOrder

	// SGcod - number of layers
	h.NumLayers = int(binary.BigEndian.Uint16(data[pos+4 : pos+6]))

	// SGcod - MCT flag
	h.MCT = data[pos+6] != 0

	// SPcod - number of decomposition levels
	h.NumDecompLevels = int(data[pos+7])

	if h.NumDecompLevels > 32 {
		return pos, fmt.Errorf("%w: too many decomposition levels: %d", ErrInvalidHeader, h.NumDecompLevels)
	}

	// SPcod - code block width
	cbWidthExp := data[pos+8]
	h.CodeBlockWidth = 1 << (cbWidthExp + 2)

	// SPcod - code block height
	cbHeightExp := data[pos+9]
	h.CodeBlockHeight = 1 << (cbHeightExp + 2)

	// SPcod - code block style
	h.CodeBlockStyle = data[pos+10]

	// SPcod - wavelet transform
	wavelet := data[pos+11]
	switch wavelet {
	case 0:
		h.WaveletFilter = Wavelet97
	case 1:
		h.WaveletFilter = Wavelet53
	default:
		return pos, fmt.Errorf("%w: wavelet type %d", ErrUnsupportedWavelet, wavelet)
	}

	// SPcod - precinct sizes (if Scod bit 0 is set)
	// Per ITU-T T.800: one byte per resolution level (numDecompLevels + 1 bytes)
	// Each byte: PPy (high 4 bits) x PPx (low 4 bits), where size = 2^PP
	if hasCustomPrecincts {
		numResLevels := h.NumDecompLevels + 1
		precinctOffset := pos + 12 // After the fixed SPcod fields

		if precinctOffset+numResLevels > pos+segLen {
			return pos, fmt.Errorf("%w: COD segment too short for precinct sizes", ErrTruncatedData)
		}

		h.PrecinctSizes = make([][2]int, numResLevels)
		for r := range numResLevels {
			ppByte := data[precinctOffset+r]
			ppx := int(ppByte & 0x0F)        // Low 4 bits: width exponent
			ppy := int((ppByte >> 4) & 0x0F) // High 4 bits: height exponent
			h.PrecinctSizes[r] = [2]int{ppx, ppy}
		}
	}

	return pos + segLen, nil
}

// parseTileCOD parses a COD marker in a tile-part header to get tile-specific CodeBlockStyle
// Per ITU-T T.800, COD in tile-part header overrides main header COD for that tile
func parseTileCOD(data []byte, pos int, tile *Tile) (int, error) {
	if pos+12 > len(data) {
		return pos, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if pos+segLen > len(data) {
		return pos, ErrTruncatedData
	}

	// SPcod - code block style is at offset 10 (after Scod, SGcod, and first 4 SPcod fields)
	// Structure:
	// - Lcod (2 bytes): segment length
	// - Scod (1 byte): coding style flags
	// - SGcod (4 bytes): progression order, num layers, MCT
	// - SPcod: decomp levels (1), cb width exp (1), cb height exp (1), cb style (1), wavelet (1)
	if pos+11 > len(data) {
		return pos, ErrTruncatedData
	}

	// Extract info from tile-part COD
	// Structure (offsets from pos, which is after the marker bytes):
	// - Lcod (2 bytes): pos+0..pos+1 = segment length
	// - Scod (1 byte): pos+2 = coding style flags (SOP/EPH/precinct sizes)
	// - SGcod (4 bytes): pos+3 = progression order, pos+4..pos+5 = num layers, pos+6 = MCT
	// - SPcod: pos+7 = decomp levels, pos+8 = cb width exp, pos+9 = cb height exp,
	//          pos+10 = cb style, pos+11 = wavelet transform
	//          pos+12.. = precinct sizes (if Scod bit 0 set)
	tile.HasTileCOD = true

	// SGcod: progression order
	tile.TileProgressionOrder = data[pos+3]

	// SGcod: number of layers
	tile.TileNumLayers = int(binary.BigEndian.Uint16(data[pos+4 : pos+6]))

	// SPcod: decomp levels
	tile.TileNumDecompLevels = int(data[pos+7])

	// SPcod: code block size exponents
	tile.TileCodeBlockWidthExp = data[pos+8]
	tile.TileCodeBlockHeightExp = data[pos+9]

	// SPcod: code block style
	tile.TileCodeBlockStyle = data[pos+10]

	// SPcod: wavelet transform
	wavelet := data[pos+11]
	switch wavelet {
	case 0:
		tile.TileWaveletFilter = Wavelet97
	case 1:
		tile.TileWaveletFilter = Wavelet53
	}

	// Parse precinct sizes if Scod bit 0 is set
	scod := data[pos+2]
	if scod&0x01 != 0 {
		numLevels := int(data[pos+7]) + 1 // decomp levels + 1
		tile.TilePrecinctSizes = make([][2]byte, numLevels)
		for i := 0; i < numLevels && pos+12+i < pos+segLen; i++ {
			ppByte := data[pos+12+i]
			tile.TilePrecinctSizes[i] = [2]byte{ppByte & 0x0F, (ppByte >> 4) & 0x0F}
		}
	}

	return pos + segLen, nil
}

// parseTileQCD parses a QCD marker in a tile-part header to get tile-specific quantization.
// Per ITU-T T.800, QCD in tile-part header overrides main header QCD for that tile.
func parseTileQCD(data []byte, pos int, tile *Tile) (int, error) {
	if pos+3 > len(data) {
		return pos, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if pos+segLen > len(data) {
		return pos, ErrTruncatedData
	}

	sqcd := data[pos+2]
	tile.HasTileQCD = true
	tile.TileQuantStyle = sqcd & 0x1F
	tile.TileGuardBits = int(sqcd >> 5)

	numBands := segLen - 3

	switch tile.TileQuantStyle {
	case 0: // No quantization
		tile.TileExponents = make([]int, numBands)
		for i := range numBands {
			if pos+3+i >= len(data) {
				return pos, ErrTruncatedData
			}
			tile.TileExponents[i] = int(data[pos+3+i] >> 3)
		}

	case 1: // Scalar derived
		if numBands < 2 {
			return pos, fmt.Errorf("%w: tile QCD segment too short for scalar derived", ErrInvalidHeader)
		}
		if pos+5 > len(data) {
			return pos, ErrTruncatedData
		}
		val := binary.BigEndian.Uint16(data[pos+3 : pos+5])
		tile.TileExponents = []int{int(val >> 11)}
		tile.TileMantissas = []int{int(val & 0x7FF)}

	case 2: // Scalar expounded
		numValues := numBands / 2
		tile.TileExponents = make([]int, numValues)
		tile.TileMantissas = make([]int, numValues)
		for i := range numValues {
			offset := pos + 3 + 2*i
			if offset+2 > len(data) {
				return pos, ErrTruncatedData
			}
			val := binary.BigEndian.Uint16(data[offset : offset+2])
			tile.TileExponents[i] = int(val >> 11)
			tile.TileMantissas[i] = int(val & 0x7FF)
		}

	default:
		return pos, fmt.Errorf("%w: tile quantization style %d", ErrUnsupportedQuant, tile.TileQuantStyle)
	}

	return pos + segLen, nil
}

// parseCOC parses coding style component marker (overrides COD for specific component)
func parseCOC(data []byte, pos int, h *MainHeader) (int, error) {
	if pos+8 > len(data) {
		return pos, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if pos+segLen > len(data) {
		return pos, ErrTruncatedData
	}

	// Ccoc - component index
	// If NumComps <= 256, uses 1 byte; otherwise 2 bytes
	compOffset := 1
	compIdx := int(data[pos+2])
	if h.NumComps > 256 {
		compOffset = 2
		compIdx = int(binary.BigEndian.Uint16(data[pos+2 : pos+4]))
	}

	// Scoc at pos+2+compOffset
	// SPcoc starts at pos+2+compOffset+1 = pos+3+compOffset
	//
	// SPcoc structure (same as SPcod):
	// - Number of decomposition levels: 1 byte (offset 0)
	// - Code block width exponent: 1 byte (offset 1)
	// - Code block height exponent: 1 byte (offset 2)
	// - Code block style: 1 byte (offset 3)
	// - Wavelet transformation: 1 byte (offset 4)
	// - Precinct sizes (if enabled in Scoc): variable

	// SPcoc starts at pos+2+compOffset+1
	spcPos := pos + 2 + compOffset + 1

	// Verify we have enough data for all SPcoc fields
	if spcPos+5 > pos+segLen {
		return pos + segLen, nil
	}

	// Initialize per-component storage if needed
	if h.CompDecompLevels == nil {
		h.CompDecompLevels = make([]int, h.NumComps)
		for i := range h.CompDecompLevels {
			h.CompDecompLevels[i] = -1 // -1 means use default
		}
	}

	// Store the per-component decomposition level
	// SPcoc offset 0: Number of decomposition levels
	if compIdx >= 0 && compIdx < len(h.CompDecompLevels) {
		h.CompDecompLevels[compIdx] = int(data[spcPos])
	}

	// Store per-component code block sizes, style, and wavelet filter
	if compIdx >= 0 && compIdx < h.NumComps {
		// Initialize per-component storage if needed
		if h.CompCodeBlockWidth == nil {
			h.CompCodeBlockWidth = make([]int, h.NumComps)
			h.CompCodeBlockHeight = make([]int, h.NumComps)
			h.CompCodeBlockStyle = make([]byte, h.NumComps)
			h.CompWaveletFilter = make([]WaveletType, h.NumComps)
			for i := range h.NumComps {
				h.CompCodeBlockStyle[i] = 0xFF // sentinel: use default
				h.CompWaveletFilter[i] = 0xFF  // sentinel: use default
			}
		}

		// SPcoc offset 1: Code block width exponent
		cbWidthExp := data[spcPos+1]
		h.CompCodeBlockWidth[compIdx] = 1 << (cbWidthExp + 2)

		// SPcoc offset 2: Code block height exponent
		cbHeightExp := data[spcPos+2]
		h.CompCodeBlockHeight[compIdx] = 1 << (cbHeightExp + 2)

		// SPcoc offset 3: Code block style
		h.CompCodeBlockStyle[compIdx] = data[spcPos+3]

		// SPcoc offset 4: Wavelet transformation
		wavelet := data[spcPos+4]
		switch wavelet {
		case 0:
			h.CompWaveletFilter[compIdx] = Wavelet97
		case 1:
			h.CompWaveletFilter[compIdx] = Wavelet53
		}

		// Also update global values for component 0 for backward compatibility
		if compIdx == 0 {
			h.CodeBlockWidth = h.CompCodeBlockWidth[0]
			h.CodeBlockHeight = h.CompCodeBlockHeight[0]
			h.CodeBlockStyle = h.CompCodeBlockStyle[0]
			h.WaveletFilter = h.CompWaveletFilter[0]
		}
	}

	// Parse per-component precinct sizes if Scoc bit 0 is set
	scoc := data[pos+2+compOffset]
	if scoc&0x01 != 0 {
		numDecomp := int(data[spcPos])
		numResLevels := numDecomp + 1
		precinctBase := spcPos + 5 // after numDecomp, cbW, cbH, style, wavelet
		if precinctBase+numResLevels <= pos+segLen {
			// Initialize CompPrecinctSizes if needed
			if h.CompPrecinctSizes == nil {
				h.CompPrecinctSizes = make([][][2]int, h.NumComps)
			}
			if compIdx >= 0 && compIdx < len(h.CompPrecinctSizes) {
				sizes := make([][2]int, numResLevels)
				for r := range numResLevels {
					ppByte := data[precinctBase+r]
					ppx := int(ppByte & 0x0F)
					ppy := int((ppByte >> 4) & 0x0F)
					sizes[r] = [2]int{ppx, ppy}
				}
				h.CompPrecinctSizes[compIdx] = sizes
			}
		}
	}

	return pos + segLen, nil
}

// parseQCD parses quantization default marker
func parseQCD(data []byte, pos int, h *MainHeader) (int, error) {
	if pos+3 > len(data) {
		return pos, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if pos+segLen > len(data) {
		return pos, ErrTruncatedData
	}

	// Sqcd - quantization style
	sqcd := data[pos+2]
	h.QuantStyle = sqcd & 0x1F
	h.OriginalQuantStyle = h.QuantStyle // Save original before any QCC override
	h.GuardBits = int(sqcd >> 5)

	numBands := segLen - 3

	switch h.QuantStyle {
	case 0: // No quantization
		// One byte per subband: 5 bits exponent, 3 bits unused
		h.Exponents = make([]int, numBands)
		for i := range numBands {
			if pos+3+i >= len(data) {
				return pos, ErrTruncatedData
			}
			h.Exponents[i] = int(data[pos+3+i] >> 3)
		}
		// Save original values for components without QCC override
		h.OriginalExponents = make([]int, len(h.Exponents))
		copy(h.OriginalExponents, h.Exponents)

	case 1: // Scalar derived
		// One value for LL band (2 bytes: 5 bits exp, 11 bits mantissa)
		if numBands < 2 {
			return pos, fmt.Errorf("%w: QCD segment too short for scalar derived", ErrInvalidHeader)
		}
		if pos+5 > len(data) {
			return pos, ErrTruncatedData
		}
		val := binary.BigEndian.Uint16(data[pos+3 : pos+5])
		exp := int(val >> 11)
		mant := int(val & 0x7FF)

		h.Exponents = []int{exp}
		h.Mantissas = []int{mant}
		// Save original values for components without QCC override
		h.OriginalExponents = []int{exp}
		h.OriginalMantissas = []int{mant}

		// Compute step size per ITU-T T.800 equation E.2:
		// Δ_b = 2^(R_b - ε_b) × (1 + μ_b / 2^11)
		// R_b = nominal dynamic range = bit depth from SIZ marker
		// Note: Guard bits are for EBCOT precision, not step size calculation
		Rb := 8 // Default to 8-bit if SIZ not yet parsed
		if len(h.BitDepth) > 0 && h.BitDepth[0] > 0 {
			Rb = h.BitDepth[0]
		}
		stepSize := (1.0 + float64(mant)/2048.0) * math.Pow(2, float64(Rb-exp))
		h.StepSizes = []float64{stepSize}

	case 2: // Scalar expounded
		// Two bytes per subband: 5 bits exp, 11 bits mantissa
		numValues := numBands / 2
		h.Exponents = make([]int, numValues)
		h.Mantissas = make([]int, numValues)
		h.StepSizes = make([]float64, numValues)

		// Per ITU-T T.800 equation E.2:
		// Δ_b = 2^(R_b - ε_b) × (1 + μ_b / 2^11)
		// R_b = nominal dynamic range = bit depth from SIZ marker
		// Note: Guard bits are for EBCOT precision, not step size calculation
		Rb := 8 // Default to 8-bit if SIZ not yet parsed
		if len(h.BitDepth) > 0 && h.BitDepth[0] > 0 {
			Rb = h.BitDepth[0]
		}

		for i := range numValues {
			offset := pos + 3 + 2*i
			if offset+2 > len(data) {
				return pos, ErrTruncatedData
			}
			val := binary.BigEndian.Uint16(data[offset : offset+2])
			exp := int(val >> 11)
			mant := int(val & 0x7FF)

			h.Exponents[i] = exp
			h.Mantissas[i] = mant

			// Compute step size
			stepSize := (1.0 + float64(mant)/2048.0) * math.Pow(2, float64(Rb-exp))
			h.StepSizes[i] = stepSize
		}
		// Save original values for components without QCC override
		h.OriginalExponents = make([]int, len(h.Exponents))
		h.OriginalMantissas = make([]int, len(h.Mantissas))
		copy(h.OriginalExponents, h.Exponents)
		copy(h.OriginalMantissas, h.Mantissas)

	default:
		return pos, fmt.Errorf("%w: quantization style %d", ErrUnsupportedQuant, h.QuantStyle)
	}

	return pos + segLen, nil
}

// parseQCC parses quantization component marker
// QCC overrides QCD for a specific component.
func parseQCC(data []byte, pos int, h *MainHeader) (int, error) {
	if pos+4 > len(data) {
		return pos, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if pos+segLen > len(data) {
		return pos, ErrTruncatedData
	}

	// Cqcc - component index (1 byte if NumComps < 257, else 2 bytes)
	compBytes := 1
	if h.NumComps >= 257 {
		compBytes = 2
	}
	if pos+2+compBytes >= len(data) {
		return pos, ErrTruncatedData
	}

	var compIdx int
	if compBytes == 1 {
		compIdx = int(data[pos+2])
	} else {
		compIdx = int(binary.BigEndian.Uint16(data[pos+2 : pos+4]))
	}

	// Validate component index
	if compIdx < 0 || compIdx >= h.NumComps {
		return pos + segLen, nil // Skip invalid component
	}

	// Sqcc - same format as Sqcd
	sqcc := data[pos+2+compBytes]
	quantStyle := sqcc & 0x1F

	// Initialize per-component arrays if needed
	if h.CompQuantStyle == nil {
		h.CompQuantStyle = make([]byte, h.NumComps)
		h.CompExponents = make([][]int, h.NumComps)
		h.CompMantissas = make([][]int, h.NumComps)
		// Mark all as uninitialized (255 = use default QCD)
		for i := range h.CompQuantStyle {
			h.CompQuantStyle[i] = 255
		}
	}

	// Store per-component quantization style
	h.CompQuantStyle[compIdx] = quantStyle

	numBands := segLen - 3 - compBytes // Remaining bytes for step sizes

	var exponents, mantissas []int

	switch quantStyle {
	case 0: // No quantization
		// One byte per subband: 5 bits exponent, 3 bits unused
		exponents = make([]int, numBands)
		mantissas = make([]int, numBands)
		for i := range numBands {
			offset := pos + 3 + compBytes + i
			if offset >= len(data) {
				break
			}
			exponents[i] = int(data[offset] >> 3)
			mantissas[i] = 0 // No mantissa for no quantization
		}

	case 1: // Scalar derived
		// One value for LL band (2 bytes)
		if pos+5+compBytes > len(data) {
			return pos, ErrTruncatedData
		}
		val := binary.BigEndian.Uint16(data[pos+3+compBytes : pos+5+compBytes])
		exp := int(val >> 11)
		mant := int(val & 0x7FF)
		exponents = []int{exp}
		mantissas = []int{mant}

	case 2: // Scalar expounded
		// Two bytes per subband
		numValues := numBands / 2
		exponents = make([]int, numValues)
		mantissas = make([]int, numValues)
		for i := range numValues {
			offset := pos + 3 + compBytes + 2*i
			if offset+2 > len(data) {
				break
			}
			val := binary.BigEndian.Uint16(data[offset : offset+2])
			exponents[i] = int(val >> 11)
			mantissas[i] = int(val & 0x7FF)
		}
	}

	// Store per-component values
	h.CompExponents[compIdx] = exponents
	h.CompMantissas[compIdx] = mantissas

	// For backward compatibility, also update main header values for component 0
	if compIdx == 0 {
		guardBits := int(sqcc >> 5)
		h.QuantStyle = quantStyle
		h.GuardBits = guardBits
		h.Exponents = exponents
		h.Mantissas = mantissas
	}

	return pos + segLen, nil
}

// parseRGN parses region of interest marker for a tile
// Per ITU-T T.800 Table A.20: RGN marker specifies ROI shift value
// The ROI shift indicates how many bits the encoder shifted up ROI coefficients.
// During decoding, we must shift them back down.
func parseRGN(data []byte, pos int, h *MainHeader, tile *Tile) (int, error) {
	if pos+4 > len(data) {
		return pos, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if pos+segLen > len(data) {
		return pos, ErrTruncatedData
	}

	// Crgn - component index (1 byte if NumComps < 257, else 2 bytes)
	compBytes := 1
	if h.NumComps >= 257 {
		compBytes = 2
	}
	if pos+2+compBytes+2 > len(data) {
		return pos, ErrTruncatedData
	}

	var compIdx int
	if compBytes == 1 {
		compIdx = int(data[pos+2])
	} else {
		compIdx = int(binary.BigEndian.Uint16(data[pos+2 : pos+4]))
	}

	// Srgn - ROI style (must be 0 for max-shift method per ITU-T T.800)
	// We skip reading it since only style 0 is defined

	// SPrgn - ROI shift value (1 byte)
	sprgn := int(data[pos+2+compBytes+1])

	// Initialize ROIShift slice if needed
	if tile.ROIShift == nil {
		tile.ROIShift = make([]int, h.NumComps)
	}

	// Store the ROI shift for this component
	if compIdx >= 0 && compIdx < len(tile.ROIShift) {
		tile.ROIShift[compIdx] = sprgn
	}

	return pos + segLen, nil
}

// parsePOC parses progression order change marker
// Per ITU-T T.800 Table A.22-A.23
func parsePOC(data []byte, pos int, h *MainHeader) (int, error) {
	if pos+2 > len(data) {
		return pos, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if pos+segLen > len(data) {
		return pos, ErrTruncatedData
	}

	// Entry size depends on number of components
	// For Csiz < 257: entry is 7 bytes (RSpoc 1, CSpoc 1, LYEpoc 2, REpoc 1, CEpoc 1, Ppoc 1)
	// For Csiz >= 257: entry is 9 bytes (CSpoc and CEpoc are 2 bytes each)
	compBytes := 1
	if h.NumComps >= 257 {
		compBytes = 2
	}
	entrySize := 5 + 2*compBytes // 1 + compBytes + 2 + 1 + compBytes + 1

	contentLen := segLen - 2 // Lpoc includes itself
	numEntries := contentLen / entrySize

	h.POCEntries = make([]POCEntry, 0, numEntries)

	for i := range numEntries {
		offset := pos + 2 + i*entrySize
		if offset+entrySize > len(data) {
			break
		}

		entry := POCEntry{}

		// RSpoc - resolution level start
		entry.RSpoc = int(data[offset])
		offset++

		// CSpoc - component start
		if compBytes == 1 {
			entry.CSpoc = int(data[offset])
			offset++
		} else {
			entry.CSpoc = int(binary.BigEndian.Uint16(data[offset : offset+2]))
			offset += 2
		}

		// LYEpoc - layer end (always 2 bytes)
		entry.LYEpoc = int(binary.BigEndian.Uint16(data[offset : offset+2]))
		offset += 2

		// REpoc - resolution level end
		entry.REpoc = int(data[offset])
		offset++

		// CEpoc - component end
		if compBytes == 1 {
			entry.CEpoc = int(data[offset])
			offset++
		} else {
			entry.CEpoc = int(binary.BigEndian.Uint16(data[offset : offset+2]))
			offset += 2
		}

		// Ppoc - progression order
		entry.Ppoc = data[offset]

		h.POCEntries = append(h.POCEntries, entry)
	}

	return pos + segLen, nil
}

// parseTilePOC parses a POC marker in tile-part header.
// Returns the parsed entries separately (not modifying main header).
// Per ITU-T T.800 Table A.22-A.23
func parseTilePOC(data []byte, pos int, h *MainHeader) (int, []POCEntry, error) {
	if pos+2 > len(data) {
		return pos, nil, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if pos+segLen > len(data) {
		return pos, nil, ErrTruncatedData
	}

	// Entry size depends on number of components
	compBytes := 1
	if h.NumComps >= 257 {
		compBytes = 2
	}
	entrySize := 5 + 2*compBytes

	contentLen := segLen - 2
	numEntries := contentLen / entrySize

	entries := make([]POCEntry, 0, numEntries)

	for i := range numEntries {
		offset := pos + 2 + i*entrySize
		if offset+entrySize > len(data) {
			break
		}

		entry := POCEntry{}

		entry.RSpoc = int(data[offset])
		offset++

		if compBytes == 1 {
			entry.CSpoc = int(data[offset])
			offset++
		} else {
			entry.CSpoc = int(binary.BigEndian.Uint16(data[offset : offset+2]))
			offset += 2
		}

		entry.LYEpoc = int(binary.BigEndian.Uint16(data[offset : offset+2]))
		offset += 2

		entry.REpoc = int(data[offset])
		offset++

		if compBytes == 1 {
			entry.CEpoc = int(data[offset])
			offset++
		} else {
			entry.CEpoc = int(binary.BigEndian.Uint16(data[offset : offset+2]))
			offset += 2
		}

		entry.Ppoc = data[offset]

		entries = append(entries, entry)
	}

	return pos + segLen, entries, nil
}

// parseTLM parses tile-part length marker for random access.
// Per ITU-T T.800, TLM provides tile-part lengths without reading SOT markers.
func parseTLM(data []byte, pos int, h *MainHeader) (int, error) {
	if pos+4 > len(data) {
		return pos, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if segLen < 4 || pos+segLen > len(data) {
		return pos, ErrTruncatedData
	}

	// Ztlm - index of this TLM marker segment (0-255)
	// Used when multiple TLM segments are present
	_ = data[pos+2] // ztlm, not used for parsing

	// Stlm - size parameters
	stlm := data[pos+3]
	// ST (bits 5-4): Size of Ttlm field
	//   0 = Ttlm not present (tile index implied by order)
	//   1 = Ttlm is 8-bit
	//   2 = Ttlm is 16-bit
	//   3 = reserved
	st := (stlm >> 4) & 0x03
	// SP (bit 6): Size of Ptlm field
	//   0 = Ptlm is 16-bit
	//   1 = Ptlm is 32-bit
	sp := (stlm >> 6) & 0x01

	// Calculate entry size
	var ttlmSize int
	switch st {
	case 0:
		ttlmSize = 0 // No tile index
	case 1:
		ttlmSize = 1 // 8-bit tile index
	case 2:
		ttlmSize = 2 // 16-bit tile index
	default:
		// Reserved value, skip this TLM
		return pos + segLen, nil
	}

	ptlmSize := 2 // 16-bit length
	if sp == 1 {
		ptlmSize = 4 // 32-bit length
	}

	entrySize := ttlmSize + ptlmSize
	if entrySize == 0 {
		return pos + segLen, nil
	}

	// Content starts after Ltlm (2) + Ztlm (1) + Stlm (1) = 4 bytes
	contentStart := pos + 4
	contentLen := segLen - 4
	numEntries := contentLen / entrySize

	// Track implied tile index when ST=0
	impliedTileIndex := len(h.TLMEntries)

	for i := range numEntries {
		offset := contentStart + i*entrySize
		if offset+entrySize > pos+segLen {
			break
		}

		entry := TLMEntry{TileIndex: -1}

		// Read tile index if present
		switch st {
		case 0:
			// Tile index implied by order
			entry.TileIndex = impliedTileIndex
			impliedTileIndex++
		case 1:
			entry.TileIndex = int(data[offset])
			offset++
		case 2:
			entry.TileIndex = int(binary.BigEndian.Uint16(data[offset : offset+2]))
			offset += 2
		}

		// Read tile-part length
		if sp == 0 {
			entry.TilePartLen = int(binary.BigEndian.Uint16(data[offset : offset+2]))
		} else {
			entry.TilePartLen = int(binary.BigEndian.Uint32(data[offset : offset+4]))
		}

		h.TLMEntries = append(h.TLMEntries, entry)
	}

	return pos + segLen, nil
}

// parsePPM parses packed packet headers in main header marker.
// Per ITU-T T.800 Table A.28, PPM marker contains packet headers for all tiles.
// Multiple PPM segments are concatenated in order of Zppm index.
func parsePPM(data []byte, pos int, h *MainHeader) (int, error) {
	if pos+3 > len(data) {
		return pos, ErrTruncatedData
	}

	segLen := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if segLen < 3 || pos+segLen > len(data) {
		return pos, ErrTruncatedData
	}

	// Zppm - index of this PPM marker segment (0-255)
	// Per ITU-T T.800, segments must be concatenated in Zppm order
	zppm := int(data[pos+2])

	// PPM marker segment structure per ITU-T T.800 A.7.4:
	// - Lppm (2 bytes): marker segment length (includes Lppm but not marker code)
	// - Zppm (1 byte): index of this marker segment
	// - Ippm (variable): packet header information
	//
	// Ippm contains:
	// - Nppm (4 bytes): number of bytes of packet header info for tile
	// - Ippmi (Nppm bytes): packet header data
	// This repeats for each tile-part that follows.
	//
	// Store by Zppm index; they will be sorted and concatenated later.
	ppmData := data[pos+3 : pos+segLen]
	if h.ppmSegments == nil {
		h.ppmSegments = make(map[int][]byte)
	}
	h.ppmSegments[zppm] = append(h.ppmSegments[zppm], ppmData...)

	return pos + segLen, nil
}

// finalizePPMSegments concatenates PPM segments in Zppm order into PPMHeaders.
// Per ITU-T T.800 A.7.4, PPM segments must be ordered by Zppm index.
// Codestreams may emit them in any order, so we sort before concatenating.
func finalizePPMSegments(h *MainHeader) {
	if len(h.ppmSegments) == 0 {
		return
	}

	// Find max Zppm index
	maxZppm := 0
	for z := range h.ppmSegments {
		if z > maxZppm {
			maxZppm = z
		}
	}

	// Concatenate in order
	h.PPMHeaders = nil
	for z := range maxZppm + 1 {
		if seg, ok := h.ppmSegments[z]; ok {
			h.PPMHeaders = append(h.PPMHeaders, seg...)
		}
	}

	// Free segment storage
	h.ppmSegments = nil
}

// finalizePPTSegments concatenates PPT segments in Zppt order for each tile.
// Per ITU-T T.800, PPT segments must be ordered by Zppt index.
func finalizePPTSegments(tiles []*Tile) {
	for _, tile := range tiles {
		if len(tile.pptSegments) == 0 {
			continue
		}

		maxZppt := 0
		for z := range tile.pptSegments {
			if z > maxZppt {
				maxZppt = z
			}
		}

		tile.PPTHeaders = nil
		for z := range maxZppt + 1 {
			if seg, ok := tile.pptSegments[z]; ok {
				tile.PPTHeaders = append(tile.PPTHeaders, seg...)
			}
		}

		tile.pptSegments = nil
	}
}

// distributePPMHeaders distributes PPM (main header) packet headers to tiles.
// Per ITU-T T.800 A.7.4, PPM data is structured as:
//
//	(Nppm_0 || Ippmi_0) || (Nppm_1 || Ippmi_1) || ...
//
// where Nppm_i (4 bytes) is the number of packet header bytes for tile-part i,
// and Ippmi_i is the actual packet header data.
//
// We distribute this to tiles so the existing PPT infrastructure can be used.
// The tile-parts appear in codestream order, which typically matches tile index order.
func distributePPMHeaders(header *MainHeader, tiles []*Tile) {
	if len(header.PPMHeaders) == 0 || len(tiles) == 0 {
		return
	}

	ppmData := header.PPMHeaders
	pos := 0

	// Create a map from tile index to tile pointer
	tileByIndex := make(map[int]*Tile)
	for _, t := range tiles {
		tileByIndex[t.Index] = t
	}

	// Per ITU-T T.800 A.7.4, PPM data contains one (Nppm, Ippmi) pair per tile-part,
	// in the order tile-parts appear in the codestream. Use TilePartOrder to distribute
	// data to the correct tiles.
	if len(header.TilePartOrder) > 0 {
		for _, tileIdx := range header.TilePartOrder {
			if pos+4 > len(ppmData) {
				break
			}

			// Read Nppm (4 bytes) - number of bytes for this tile-part's packet headers
			nppm := int(binary.BigEndian.Uint32(ppmData[pos : pos+4]))
			pos += 4

			if nppm <= 0 {
				continue
			}
			if pos+nppm > len(ppmData) {
				nppm = len(ppmData) - pos
			}

			tile := tileByIndex[tileIdx]
			if tile != nil {
				tile.PPTHeaders = append(tile.PPTHeaders, ppmData[pos:pos+nppm]...)
			}
			pos += nppm
		}
	}
}

// parseSOT parses start of tile-part marker
func parseSOT(data []byte, pos int) (tileIndex, tilePartIndex, length, newPos int, err error) {
	if pos+10 > len(data) {
		return 0, 0, 0, pos, ErrTruncatedData
	}

	lsot := int(binary.BigEndian.Uint16(data[pos : pos+2]))
	if lsot != 10 {
		return 0, 0, 0, pos, fmt.Errorf("%w: invalid SOT length: %d", ErrInvalidHeader, lsot)
	}

	// Isot - tile index
	tileIndex = int(binary.BigEndian.Uint16(data[pos+2 : pos+4]))

	// Psot - tile-part length
	length = int(binary.BigEndian.Uint32(data[pos+4 : pos+8]))

	// TPsot - tile-part index
	tilePartIndex = int(data[pos+8])

	// TNsot - number of tile-parts (we don't use this currently)
	// _ = data[pos+9]

	return tileIndex, tilePartIndex, length, pos + lsot, nil
}
