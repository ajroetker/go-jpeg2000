package jpeg2000

import (
	"fmt"
	"math"
)

// Packet represents a single quality layer packet for a precinct
type Packet struct {
	Layer      int
	Resolution int
	Component  int
	Precinct   int
}

// ResolutionLevel represents one resolution level in the wavelet decomposition
type ResolutionLevel struct {
	Level     int // 0 = LL band only, 1+ = HL, LH, HH subbands
	Width     int
	Height    int
	X0, Y0    int // Image-space origin (trX0, trY0) for DWT parity calculation
	Subbands  []*Subband
	Precincts []*Precinct
}

// Subband within a resolution level
type Subband struct {
	Type          SubbandType
	Width, Height int
	// X0, Y0 are the subband origin in image-space coordinates (tbX0, tbY0).
	// Per ITU-T T.800 equation B-16, these are derived from resolution bounds (trX0, trY0).
	X0, Y0      int
	CodeBlocksX int // Number of code blocks horizontally
	CodeBlocksY int // Number of code blocks vertically
	CodeBlocks  [][]*CodeBlockInfo

	// Precinct grid parameters.
	// PrcGridX0/Y0: absolute index of the first precinct overlapping the resolution.
	// Computed as floor(trX0 / 2^PPx). This is the RESOLUTION-level grid origin,
	// shared by all subbands within the resolution. Using the subband origin instead
	// would give incorrect results when sbX0/prcW != trX0/(2*prcW).
	PrcGridX0, PrcGridY0 int
	// PrecinctWidth/Height: precinct size in subband coordinates.
	// For detail subbands: 2^(PPx-1) × 2^(PPy-1). For LL: 2^PPx × 2^PPy.
	PrecinctWidth  int
	PrecinctHeight int

	// Per-precinct tag trees (JPEG2000 spec: each precinct has its own tag trees)
	// Indexed by [precinctY * numPrecinctsX + precinctX]
	PrecinctInclusionTrees []*tagTree
	PrecinctZBPTrees       []*tagTree
	NumPrecinctsX          int
	NumPrecinctsY          int
}

// Precinct is a rectangular region containing code blocks
type Precinct struct {
	X, Y          int // Precinct index within resolution level
	Width, Height int // Precinct dimensions in samples
	// Code block range within each subband for this precinct
	// For subband sb: iterate cb[cbY0:cbY1][cbX0:cbX1]
	CBX0, CBY0 int // Start code block indices (inclusive)
	CBX1, CBY1 int // End code block indices (exclusive)
}

// CodeBlockInfo tracks decoding state of a code block
type CodeBlockInfo struct {
	X, Y          int    // Position in subband's code block grid
	Width, Height int    // Actual dimensions
	ZeroBitPlanes int    // Number of leading zero bit planes
	TotalPasses   int    // Total coding passes received so far
	Data          []byte // Accumulated compressed data
	Lblock        int    // Length indicator exponent (starts at 3)
	IncludedLayer int    // First layer where included (-1 if not included)

	// Per-segment lengths when ERTERM or BYPASS mode is enabled.
	// For ERTERM: one entry per pass.
	// For BYPASS: one entry per segment (segments group passes by coding mode).
	SegmentLengths []int

	// BYPASS segment tracking state (persistent across layers).
	// These track the current segment's fill state so that subsequent layers
	// can correctly continue adding passes to an existing segment or create new ones.
	bypassSegIdx      int // Current segment index (for segment creation across layers)
	bypassPassesInSeg int // Passes consumed in current segment
	bypassPrevMaxPass int // Previous segment's maxPasses (for alternation pattern)

	// Per-packet temporary state
	newPasses         int
	newLength         int
	newSegmentLengths []int // Per-segment lengths for current packet
}

// cbEntry tracks a code block pending data read
type cbEntry struct {
	cb     *CodeBlockInfo
	length int
}

// PacketTrace captures parsing state for debugging
type PacketTrace struct {
	Layer          int    // Quality layer
	Resolution     int    // Resolution level
	Component      int    // Component index
	PrecinctX      int    // Precinct X index
	PrecinctY      int    // Precinct Y index
	BytePosBefore  int    // Byte position before parsing
	BitPosBefore   int    // Bit position before parsing
	BytePosAfter   int    // Byte position after parsing
	BitPosAfter    int    // Bit position after parsing
	EmptyBit       int    // Value of empty packet indicator (0 or 1)
	NumEntries     int    // Number of code block entries
	DataRead       int    // Bytes of code block data read
	FirstBytes     []byte // First few bytes at parse start (for debugging)
	SubbandEntries []int  // Per-subband entry counts (for debugging)
}

// TileDecoder handles tile-level packet parsing and decoding
type TileDecoder struct {
	header *MainHeader
	tile   *Tile

	// Per-component resolution levels
	compResolutions [][]*ResolutionLevel // [component][resolution]

	// Current parsing state
	currentLayer int

	// Progressive decoding options
	maxLayers int // Maximum layers to decode (0 = all)
	maxRes    int // Maximum resolution level to decode (accounts for Reduce)

	// PPT (Packed Packet Headers) reader
	// When non-nil, packet headers are read from pptReader instead of tile data
	pptReader *bitReader

	// Trace collects packet parsing traces when non-nil (for debugging)
	Trace              []PacketTrace
	lastSubbandEntries []int // temp: per-subband entries from last packet parse
}

// newTileDecoder creates a tile decoder (decodes all layers and resolutions)
func newTileDecoder(header *MainHeader, tile *Tile) *TileDecoder {
	return newTileDecoderWithOptions(header, tile, 0, 0)
}

// newTileDecoderWithOptions creates a tile decoder with progressive options.
// maxLayers: maximum layers to decode (0 = all layers)
// reduce: resolution reduction factor (0 = full resolution, 1 = half, etc.)
func newTileDecoderWithOptions(header *MainHeader, tile *Tile, maxLayers, reduce int) *TileDecoder {
	// Determine effective number of layers from tile COD or main header
	numLayers := header.NumLayers
	if tile.HasTileCOD && tile.TileNumLayers > 0 {
		numLayers = tile.TileNumLayers
	}

	// Effective max layers
	effectiveMaxLayers := maxLayers
	if effectiveMaxLayers <= 0 || effectiveMaxLayers > numLayers {
		effectiveMaxLayers = numLayers
	}

	td := &TileDecoder{
		header:    header,
		tile:      tile,
		maxLayers: effectiveMaxLayers,
		maxRes:    0, // Will be calculated after building resolutions
	}

	// Build resolution structure for each component
	// The number of resolutions depends on the effective decomposition level,
	// which may be less than header.NumDecompLevels for small tiles.
	td.compResolutions = make([][]*ResolutionLevel, header.NumComps)
	maxRes := 0
	for c := 0; c < header.NumComps; c++ {
		td.compResolutions[c] = td.buildResolutions(c)
		// Track the maximum number of resolutions across all components
		if len(td.compResolutions[c])-1 > maxRes {
			maxRes = len(td.compResolutions[c]) - 1
		}
	}

	// Apply reduce factor to maxRes
	// With reduce=0, we process all effective levels
	// With reduce=1, we skip the finest level, etc.
	maxRes = max(maxRes-reduce, 0)
	td.maxRes = maxRes

	return td
}

// EnableTracing enables packet parsing trace collection
func (td *TileDecoder) EnableTracing() {
	td.Trace = make([]PacketTrace, 0, 64)
}

// getCodeBlockStyle returns the effective CodeBlockStyle for this tile
// If the tile has a tile-specific COD marker, use that; otherwise use main header
func (td *TileDecoder) getCodeBlockStyle() byte {
	if td.tile.HasTileCOD {
		return td.tile.TileCodeBlockStyle
	}
	return td.header.CodeBlockStyle
}

// getWaveletFilter returns the effective WaveletFilter for this tile
func (td *TileDecoder) getWaveletFilter() WaveletType {
	if td.tile.HasTileCOD {
		return td.tile.TileWaveletFilter
	}
	return td.header.WaveletFilter
}

// getComponentDecompLevels returns the decomposition levels for a component in this tile.
// Per ITU-T T.800, the encoder produces packets for all resolution levels specified in the
// COD marker, even if some subbands have 0 dimensions. Empty packets are still present
// in the bitstream and must be parsed to stay synchronized with PPT/PPM headers.
func (td *TileDecoder) getComponentDecompLevels(comp int) int {
	// Get the requested decomposition levels from header or tile-specific override
	if td.tile.HasTileCOD {
		return td.tile.TileNumDecompLevels
	}
	return td.header.ComponentDecompLevels(comp)
}

// buildResolutions creates resolution levels for a component
func (td *TileDecoder) buildResolutions(comp int) []*ResolutionLevel {
	h := td.header
	// Use effective decomposition level for this tile/component
	numRes := td.getComponentDecompLevels(comp) + 1
	resolutions := make([]*ResolutionLevel, numRes)

	// Calculate tile-component dimensions accounting for subsampling
	// Per ITU-T T.800 Annex B.6, tile-component bounds are:
	//   tcx0 = floor(tx0 / XRsiz)  where tx0 = max(XTOsiz + p*TileWidth, XOsiz)
	//   tcx1 = ceil(tx1 / XRsiz)   where tx1 = min(XTOsiz + (p+1)*TileWidth, Xsiz)
	// Start uses floor, end uses ceil.
	// The tile.X0/Y0/X1/Y1 values already have max/min applied in codestream.go
	xrsiz := 1
	yrsiz := 1
	if comp < len(h.XRsiz) && h.XRsiz[comp] > 0 {
		xrsiz = h.XRsiz[comp]
	}
	if comp < len(h.YRsiz) && h.YRsiz[comp] > 0 {
		yrsiz = h.YRsiz[comp]
	}

	// Per OpenJPEG tcd.c: ceil for all tile-component bounds
	tcX0 := (td.tile.X0 + xrsiz - 1) / xrsiz // ceil
	tcY0 := (td.tile.Y0 + yrsiz - 1) / yrsiz // ceil
	tcX1 := (td.tile.X1 + xrsiz - 1) / xrsiz // ceil
	tcY1 := (td.tile.Y1 + yrsiz - 1) / yrsiz // ceil
	// Clamp dimensions to non-negative.
	// Tiles can extend beyond image bounds (X0 >= X1 or Y0 >= Y1) when the tile grid
	// doesn't align with image dimensions. Such tiles have no valid pixels.
	tileWidth := max(tcX1-tcX0, 0)
	tileHeight := max(tcY1-tcY0, 0)
	// Suppress unused variable warnings - these are used for documentation purposes
	_ = tileWidth
	_ = tileHeight

	for r := range numRes {
		res := &ResolutionLevel{Level: r}

		// Dimensions at this resolution level
		// Resolution 0 is coarsest (LL only), resolution numDecomp is finest
		scale := 1 << (numRes - 1 - r)
		if scale == 0 {
			scale = 1
		}

		// Calculate resolution-level tile-component bounds in image coordinates
		// Per ITU-T T.800 equation B-15:
		//   trX0 = ceil(tcX0 / 2^(NL - r))
		//   trX1 = ceil(tcX1 / 2^(NL - r))
		// These bounds are used for precinct positioning.
		trX0 := (tcX0 + scale - 1) / scale // ceil(tcX0 / scale)
		trY0 := (tcY0 + scale - 1) / scale // ceil(tcY0 / scale)
		trX1 := (tcX1 + scale - 1) / scale // ceil(tcX1 / scale)
		trY1 := (tcY1 + scale - 1) / scale // ceil(tcY1 / scale)

		// Resolution dimensions per ITU-T T.800 B-15: use image-space bounds
		// OpenJPEG uses tr->x1 - tr->x0 for DWT synthesis dimensions
		res.Width = trX1 - trX0
		res.Height = trY1 - trY0
		if res.Width < 0 {
			res.Width = 0
		}
		if res.Height < 0 {
			res.Height = 0
		}
		// Store origin for DWT parity calculation (cas = x0 % 2)
		res.X0 = trX0
		res.Y0 = trY0

		if r == 0 {
			// Resolution 0: LL band only
			res.Subbands = []*Subband{
				td.createSubband(SubbandLL, res.Width, res.Height, r, trX0, trY0, trX1, trY1, numRes, comp),
			}
		} else {
			// Resolution r > 0: HL, LH, HH subbands
			// Per ITU-T T.800 and OpenJPEG, subband dimensions depend on parity (cas).
			// The cas value is determined by the resolution origin: cas = X0 % 2.
			//
			// When cas=0 (even origin):
			//   - LL/LH width = ceil(width/2), HL/HH width = floor(width/2)
			//   - LL/HL height = ceil(height/2), LH/HH height = floor(height/2)
			//
			// When cas=1 (odd origin):
			//   - LL/LH width = floor(width/2), HL/HH width = ceil(width/2)
			//   - (height similar based on Y0 parity)
			//
			// This matches OpenJPEG's formula: sn = (width + (cas==0 ? 1 : 0)) >> 1
			casH := trX0 % 2
			casV := trY0 % 2

			var llWidth, hlWidth, llHeight, lhHeight int
			if casH == 0 {
				llWidth = (res.Width + 1) / 2 // ceil
				hlWidth = res.Width / 2       // floor
			} else {
				llWidth = res.Width / 2       // floor
				hlWidth = (res.Width + 1) / 2 // ceil
			}
			if casV == 0 {
				llHeight = (res.Height + 1) / 2 // ceil
				lhHeight = res.Height / 2       // floor
			} else {
				llHeight = res.Height / 2       // floor
				lhHeight = (res.Height + 1) / 2 // ceil
			}

			// Note: When resolution is 1x1, floor(1/2)=0, so detail subbands
			// have 0-sized dimensions. This is correct - the encoder produces
			// empty packets for these subbands. We still create subbands with
			// 0 size so the packet parsing can consume the empty packet headers.
			res.Subbands = []*Subband{
				td.createSubband(SubbandHL, hlWidth, llHeight, r, trX0, trY0, trX1, trY1, numRes, comp),
				td.createSubband(SubbandLH, llWidth, lhHeight, r, trX0, trY0, trX1, trY1, numRes, comp),
				td.createSubband(SubbandHH, hlWidth, lhHeight, r, trX0, trY0, trX1, trY1, numRes, comp),
			}
		}

		resolutions[r] = res
	}

	return resolutions
}

// precinctCodeBlockRange computes the code block index range [cbX0,cbX1) x [cbY0,cbY1)
// for a given precinct within a subband, per ITU-T T.800 B.7.
//
// The precinct grid is defined at the resolution level and shared by all subbands.
// prcGridX0/Y0 = floor(trX0 / 2^PPx) is the absolute precinct grid origin from
// the resolution level. Each resolution precinct (px, py) maps to subband coordinates
// [(prcGridX0+px)*prcW, (prcGridX0+px+1)*prcW), intersected with the subband bounds.
//
// px, py: precinct index (0-based, relative to resolution level)
// prcGridX0, prcGridY0: resolution-level precinct grid origin
// prcW, prcH: precinct size in subband samples
// sbX0, sbY0, sbX1, sbY1: subband bounds in absolute coordinates
// cbW, cbH: code block dimensions
// cbsX, cbsY: total code blocks in subband
func precinctCodeBlockRange(px, py, prcGridX0, prcGridY0, prcW, prcH, sbX0, sbY0, sbX1, sbY1, cbW, cbH, cbsX, cbsY int) (cbX0, cbX1, cbY0, cbY1 int) {
	// Precinct bounds in absolute subband coordinates.
	// The grid origin (prcGridX0, prcGridY0) is computed from the resolution level:
	// floor(trX0 / 2^PPx). This is the same for all subbands in the resolution.
	prcAbsX0 := (prcGridX0 + px) * prcW
	prcAbsX1 := prcAbsX0 + prcW
	prcAbsY0 := (prcGridY0 + py) * prcH
	prcAbsY1 := prcAbsY0 + prcH

	// Intersect with subband bounds
	prcAbsX0 = max(prcAbsX0, sbX0)
	prcAbsX1 = min(prcAbsX1, sbX1)
	prcAbsY0 = max(prcAbsY0, sbY0)
	prcAbsY1 = min(prcAbsY1, sbY1)

	if prcAbsX0 >= prcAbsX1 || prcAbsY0 >= prcAbsY1 {
		return 0, 0, 0, 0
	}

	// Convert absolute coordinates to code block grid indices.
	// The code block grid is also anchored at absolute (0,0).
	// gridX0 = floor(sbX0 / cbW) is the absolute index of the first code block
	// in the subband. Code block local index = absolute CB index - gridX0.
	gridX0 := sbX0 / cbW
	gridY0 := sbY0 / cbH

	cbX0 = prcAbsX0/cbW - gridX0
	cbX1 = (prcAbsX1+cbW-1)/cbW - gridX0
	cbY0 = prcAbsY0/cbH - gridY0
	cbY1 = (prcAbsY1+cbH-1)/cbH - gridY0

	// Clamp to valid range
	cbX0 = max(cbX0, 0)
	cbY0 = max(cbY0, 0)
	cbX1 = min(cbX1, cbsX)
	cbY1 = min(cbY1, cbsY)

	return cbX0, cbX1, cbY0, cbY1
}

// createSubband creates a subband with code blocks and per-precinct tag trees
// trX0, trY0, trX1, trY1: resolution-level tile-component bounds
// numRes: total number of resolution levels for this component
// comp: component index (for per-component precinct sizes from COC)
func (td *TileDecoder) createSubband(sbType SubbandType, width, height, resLevel, trX0, trY0, trX1, trY1, numRes, comp int) *Subband {
	h := td.header

	cbWidth, cbHeight := h.ComponentCodeBlockSize(comp)

	// Get precinct size for this resolution level and component
	// Per-component COC sizes override the default COD sizes
	ppx, ppy := h.PrecinctSizeForRes(resLevel, comp)

	// Per ITU-T T.800 Annex A.6.1, the code block width and height shall not
	// exceed the precinct width and height respectively. For detail subbands
	// (resolution > 0), the effective precinct size in the subband is halved,
	// so the effective code block size is min(cbWidth, 2^(ppx-1)).
	// For resolution 0 (LL band), it's min(cbWidth, 2^ppx).
	if sbType == SubbandLL {
		cbWidth = min(cbWidth, 1<<ppx)
		cbHeight = min(cbHeight, 1<<ppy)
	} else {
		if ppx > 0 {
			cbWidth = min(cbWidth, 1<<(ppx-1))
		}
		if ppy > 0 {
			cbHeight = min(cbHeight, 1<<(ppy-1))
		}
	}

	// Calculate subband origin in image-space coordinates (tbX0, tbY0).
	// Per ITU-T T.800 Annex B equations B-15, B-16:
	// For LL and LH (low-pass horizontal): tbx0 = ceil(trx0 / 2)
	// For HL and HH (high-pass horizontal): tbx0 = ceil((trx0 - 1) / 2)
	// These values are stored in sb.X0/Y0 for informational purposes but are
	// not used for coefficient placement (which uses code block grid indices).
	// Calculate subband origin in image-space coordinates (tbX0, tbY0).
	// Per ITU-T T.800 Annex B equation B-16:
	//   Low-pass:  tbx0 = ceil(trx0 / 2)
	//   High-pass: tbx0 = ceil((trx0 - 1) / 2) = floor(trx0 / 2)
	// The high-pass formula ceil((x-1)/2) equals floor(x/2) for all x >= 0.
	var sbX0, sbY0 int
	switch sbType {
	case SubbandLL:
		// LL band at resolution 0 - subband bounds equal resolution bounds
		sbX0 = trX0
		sbY0 = trY0
	case SubbandLH:
		// Low-pass horizontal, high-pass vertical
		// Per ITU-T T.800 B-16: tbx0 = ceil(trx0/2), tby0 = floor(try0/2)
		sbX0 = (trX0 + 1) / 2 // ceil(trX0 / 2)
		sbY0 = trY0 / 2       // floor(trY0 / 2)
	case SubbandHL:
		// High-pass horizontal, low-pass vertical
		// Per ITU-T T.800 B-16: tbx0 = floor(trx0/2), tby0 = ceil(try0/2)
		sbX0 = trX0 / 2       // floor(trX0 / 2)
		sbY0 = (trY0 + 1) / 2 // ceil(trY0 / 2)
	case SubbandHH:
		// High-pass both directions
		// Per ITU-T T.800 B-16: tbx0 = floor(trx0/2), tby0 = floor(try0/2)
		sbX0 = trX0 / 2 // floor(trX0 / 2)
		sbY0 = trY0 / 2 // floor(trY0 / 2)
	}

	// Number of code blocks in subband
	// Per ITU-T T.800 Annex B.7, the code block grid is anchored at absolute
	// coordinate (0,0), NOT at the subband boundary. The correct formula uses:
	//   cbsX = ceil(tbx1 / cbWidth) - floor(tbx0 / cbWidth)
	// where tbx0 = sbX0, tbx1 = sbX0 + width.
	// This matters when the subband origin is not aligned to the code block grid
	// (e.g., Tile 1 in a multi-tile image where sbX0 != 0).
	cbsX := 0
	cbsY := 0
	sbX1 := sbX0 + width
	sbY1 := sbY0 + height
	if width > 0 && height > 0 {
		cbsX = (sbX1+cbWidth-1)/cbWidth - sbX0/cbWidth
		cbsY = (sbY1+cbHeight-1)/cbHeight - sbY0/cbHeight
	}

	// Calculate precinct dimensions in samples at the resolution level
	// Per ITU-T T.800: precinct size = 2^PP
	precinctWidth := 1 << ppx
	precinctHeight := 1 << ppy

	// Number of precincts at this resolution level
	// Per ITU-T T.800 Annex B.6, the precinct count is computed from resolution-level bounds:
	//   numPrecinctsX = ceil(trX1 / 2^ppx) - floor(trX0 / 2^ppx)
	//   numPrecinctsY = ceil(trY1 / 2^ppy) - floor(trY0 / 2^ppy)
	// This is DIFFERENT from ceil(width / precinctWidth) when trX0 != 0.
	//
	// IMPORTANT: Check resolution dimensions (trX1-trX0, trY1-trY0), not subband dimensions.
	// A subband might have 0 width (e.g., HL at a resolution with width=1), but the
	// resolution still has positive dimensions and therefore has precincts.
	// Empty subbands within a non-empty resolution still participate in packet iteration.
	resWidth := trX1 - trX0
	resHeight := trY1 - trY0
	numPrecinctsX := 0
	numPrecinctsY := 0
	if resWidth > 0 && resHeight > 0 {
		// ceil(trX1 / precinctWidth) - floor(trX0 / precinctWidth)
		numPrecinctsX = (trX1+precinctWidth-1)/precinctWidth - trX0/precinctWidth
		numPrecinctsY = (trY1+precinctHeight-1)/precinctHeight - trY0/precinctHeight
		if numPrecinctsX <= 0 {
			numPrecinctsX = 1
		}
		if numPrecinctsY <= 0 {
			numPrecinctsY = 1
		}
	}

	// For detail subbands, precinct dimensions in subband samples are halved
	// (used for code block per precinct calculation below)
	sbPrecinctWidth := precinctWidth
	sbPrecinctHeight := precinctHeight
	if resLevel > 0 && sbType != SubbandLL {
		sbPrecinctWidth = (precinctWidth + 1) / 2
		sbPrecinctHeight = (precinctHeight + 1) / 2
	}

	// Resolution-level precinct grid origin. This is the absolute index of the first
	// precinct overlapping the resolution, shared by ALL subbands in this resolution.
	// floor(trX0 / 2^PPx) — using the full resolution-level precinct size.
	prcGridX0 := trX0 / precinctWidth
	prcGridY0 := trY0 / precinctHeight

	sb := &Subband{
		Type:           sbType,
		Width:          width,
		Height:         height,
		X0:             sbX0,
		Y0:             sbY0,
		CodeBlocksX:    cbsX,
		CodeBlocksY:    cbsY,
		CodeBlocks:     make([][]*CodeBlockInfo, cbsY),
		PrcGridX0:      prcGridX0,
		PrcGridY0:      prcGridY0,
		PrecinctWidth:  sbPrecinctWidth,
		PrecinctHeight: sbPrecinctHeight,
		NumPrecinctsX:  numPrecinctsX,
		NumPrecinctsY:  numPrecinctsY,
	}

	// Create per-precinct tag trees
	// Each precinct has its own inclusion and ZBP tag trees
	numPrecincts := numPrecinctsX * numPrecinctsY
	sb.PrecinctInclusionTrees = make([]*tagTree, numPrecincts)
	sb.PrecinctZBPTrees = make([]*tagTree, numPrecincts)

	for py := 0; py < numPrecinctsY; py++ {
		for px := 0; px < numPrecinctsX; px++ {
			precinctIdx := py*numPrecinctsX + px

			// Calculate code block range for this precinct using ITU-T T.800 B.7.
			// Uses the resolution-level precinct grid origin (prcGridX0, prcGridY0),
			// NOT the subband origin divided by precinct size.
			cbX0, cbX1, cbY0, cbY1 := precinctCodeBlockRange(
				px, py, prcGridX0, prcGridY0, sbPrecinctWidth, sbPrecinctHeight,
				sbX0, sbY0, sbX0+width, sbY0+height,
				cbWidth, cbHeight, cbsX, cbsY,
			)

			// Tag tree dimensions = code blocks in this precinct
			ttWidth := cbX1 - cbX0
			ttHeight := cbY1 - cbY0
			if ttWidth <= 0 {
				ttWidth = 1
			}
			if ttHeight <= 0 {
				ttHeight = 1
			}

			sb.PrecinctInclusionTrees[precinctIdx] = newTagTree(ttWidth, ttHeight)
			sb.PrecinctZBPTrees[precinctIdx] = newTagTree(ttWidth, ttHeight)
		}
	}

	// Create code blocks
	// Per ITU-T T.800 B.7, code blocks are cells in a grid anchored at absolute (0,0).
	// For local index (x, y), the absolute grid index is (gridX0+x, gridY0+y).
	// Each code block's dimensions are the intersection of its grid cell with the subband.
	gridX0 := sbX0 / cbWidth
	gridY0 := sbY0 / cbHeight
	for y := 0; y < cbsY; y++ {
		sb.CodeBlocks[y] = make([]*CodeBlockInfo, cbsX)
		for x := 0; x < cbsX; x++ {
			// Absolute grid cell bounds
			cellLeft := (gridX0 + x) * cbWidth
			cellRight := cellLeft + cbWidth
			cellTop := (gridY0 + y) * cbHeight
			cellBottom := cellTop + cbHeight

			// Intersect with subband bounds [sbX0, sbX1) x [sbY0, sbY1)
			cbW := min(cellRight, sbX1) - max(cellLeft, sbX0)
			cbH := min(cellBottom, sbY1) - max(cellTop, sbY0)
			if cbW <= 0 {
				cbW = 1
			}
			if cbH <= 0 {
				cbH = 1
			}

			sb.CodeBlocks[y][x] = &CodeBlockInfo{
				X:             x,
				Y:             y,
				Width:         cbW,
				Height:        cbH,
				Lblock:        3, // Initial value per spec
				IncludedLayer: -1,
			}
		}
	}

	return sb
}

// getNumPrecincts returns the number of precincts (X, Y) for a resolution level
// Uses component 0's precinct structure. For per-component precinct counts,
// use getNumPrecinctsForComponent.
func (td *TileDecoder) getNumPrecincts(res int) (int, int) {
	return td.getNumPrecinctsForComponent(res, 0)
}

// getNumPrecinctsForComponent returns the number of precincts (X, Y) for a
// resolution level of a specific component. Different components may have
// different precinct counts when using component subsampling (e.g., YCbCr 4:2:0).
func (td *TileDecoder) getNumPrecinctsForComponent(res, comp int) (int, int) {
	if comp >= len(td.compResolutions) {
		comp = 0
	}
	if len(td.compResolutions) == 0 || res >= len(td.compResolutions[comp]) {
		return 1, 1
	}
	resLevel := td.compResolutions[comp][res]
	if len(resLevel.Subbands) == 0 {
		return 1, 1
	}
	sb := resLevel.Subbands[0]
	return sb.NumPrecinctsX, sb.NumPrecinctsY
}

// parsePackets parses all packets from tile data across all tile-parts
func (td *TileDecoder) parsePackets() error {
	h := td.header
	totalDataCollected := 0

	// Concatenate all tile-part data into a single stream
	// A tile may have multiple tile-parts, but they form a continuous packet stream
	var allData []byte
	for _, tp := range td.tile.TileParts {
		allData = append(allData, tp.Data...)
	}

	if len(allData) == 0 {
		return nil
	}

	br := newBitReader(allData)

	// Check for PPT (Packed Packet Headers)
	// When PPT is present, packet headers are stored separately from tile data
	// The tile data stream contains only SOP markers and packet body data
	var pptReader *bitReader
	if len(td.tile.PPTHeaders) > 0 {
		pptReader = newBitReader(td.tile.PPTHeaders)
	}
	td.pptReader = pptReader

	// JPEG2000 packet structure: each packet is header followed immediately by data
	// Parse packets in progression order, iterating through all layers
	// Each packet corresponds to one (layer, resolution, component, precinct) tuple

	// Use progressive limits instead of full ranges
	maxLayers := td.maxLayers
	maxRes := td.maxRes

	// Check for POC entries - tile-part POC takes precedence over main header POC
	pocEntries := td.tile.POCEntries
	if len(pocEntries) == 0 {
		pocEntries = h.POCEntries
	}

	// Determine effective progression order: tile-part COD overrides main header COD
	progressionOrder := h.ProgressionOrder
	if td.tile.HasTileCOD {
		progressionOrder = td.tile.TileProgressionOrder
	}

	// If POC entries exist, process them sequentially per ITU-T T.800 Annex B.12
	if len(pocEntries) > 0 {
		totalDataCollected += td.parsePacketsWithPOC(br, pocEntries, maxLayers, maxRes)
	} else {
		// No POC - use effective progression order
		// Note: resEnd is exclusive, so pass maxRes+1 to include all resolutions
		totalDataCollected += td.parsePacketsWithOrder(br, progressionOrder, 0, maxRes+1, 0, h.NumComps, maxLayers)
	}

	// If we had data but collected nothing, check if the tile has any code blocks
	// that were included in the packet stream. Tiles that extend beyond the image
	// boundary have zero area and no code blocks, so they legitimately collect
	// nothing even if the bitstream contains data.
	//
	// IMPORTANT: Code blocks can be "included" with zero data length - this is valid
	// JPEG2000 and means the coefficients are zero. Only return an error if we have
	// code blocks that were never included at all, which indicates a parsing failure.
	if totalDataCollected == 0 && len(allData) > 0 {
		// Check if any code blocks were included (parsed from packet headers)
		// A code block is included if cb.IncludedLayer >= 0
		hasIncludedCodeBlocks := false
		hasCodeBlocks := false
		for _, compRes := range td.compResolutions {
			for _, res := range compRes {
				for _, sb := range res.Subbands {
					if sb.CodeBlocksX > 0 && sb.CodeBlocksY > 0 {
						hasCodeBlocks = true
						// Check if any code block in this subband was included
						for y := 0; y < sb.CodeBlocksY; y++ {
							for x := 0; x < sb.CodeBlocksX; x++ {
								if sb.CodeBlocks[y][x].IncludedLayer >= 0 {
									hasIncludedCodeBlocks = true
									break
								}
							}
							if hasIncludedCodeBlocks {
								break
							}
						}
					}
				}
			}
		}

		// Only return an error if we have code blocks that weren't included
		if hasCodeBlocks && !hasIncludedCodeBlocks {
			return fmt.Errorf("no packet data read: parsed %d bytes from bitstream but got 0 bytes of data", len(allData))
		}
	}

	return nil
}

// parsePacketsWithPOC processes packets according to POC (Progression Order Change) entries.
// Per ITU-T T.800 Annex B.12, POC entries are processed sequentially, each defining
// a range of resolutions/components/layers to iterate in a specific order.
// When POC entries have overlapping ranges, packets that were already read by a previous
// entry are skipped (not present in the bitstream again).
func (td *TileDecoder) parsePacketsWithPOC(br *bitReader, pocEntries []POCEntry, maxLayers, maxRes int) int {
	h := td.header
	totalDataCollected := 0

	// Track which packets have been processed to handle overlapping POC ranges
	processedPackets := make(map[packetKey]bool)

	for _, poc := range pocEntries {
		// Clamp POC bounds to actual tile limits
		resStart := poc.RSpoc
		resEnd := min(poc.REpoc, maxRes+1)

		compStart := poc.CSpoc
		compEnd := min(poc.CEpoc, h.NumComps)

		layerEnd := min(poc.LYEpoc, maxLayers)

		// Parse packets in this POC entry's order, skipping already-processed packets
		totalDataCollected += td.parsePacketsInRange(br, poc.Ppoc, resStart, resEnd, compStart, compEnd, layerEnd, processedPackets)
	}

	return totalDataCollected
}

// packetKey uniquely identifies a packet by its (layer, resolution, component, precinct) tuple
type packetKey struct {
	layer, res, comp, px, py int
}

// parsePacketsInRange parses packets within specified bounds using the given progression order.
// Skips packets that have already been processed (tracked in processedPackets).
func (td *TileDecoder) parsePacketsInRange(br *bitReader, order byte, resStart, resEnd, compStart, compEnd, layerEnd int, processed map[packetKey]bool) int {
	totalDataCollected := 0

	switch order {
	case 0: // LRCP
		for l := range layerEnd {
			for r := resStart; r < resEnd; r++ {
				for c := compStart; c < compEnd; c++ {
					// Use per-component precinct count to handle subsampled components
					numPX, numPY := td.getNumPrecinctsForComponent(r, c)
					for py := range numPY {
						for px := range numPX {
							key := packetKey{l, r, c, px, py}
							if processed[key] {
								continue
							}
							processed[key] = true
							dataRead := td.parseAndReadPacket(br, l, r, c, px, py)
							totalDataCollected += dataRead
						}
					}
				}
			}
		}

	case 1: // RLCP
		for r := resStart; r < resEnd; r++ {
			for l := range layerEnd {
				for c := compStart; c < compEnd; c++ {
					// Use per-component precinct count to handle subsampled components
					numPX, numPY := td.getNumPrecinctsForComponent(r, c)
					for py := range numPY {
						for px := range numPX {
							key := packetKey{l, r, c, px, py}
							if processed[key] {
								continue
							}
							processed[key] = true
							dataRead := td.parseAndReadPacket(br, l, r, c, px, py)
							totalDataCollected += dataRead
						}
					}
				}
			}
		}

	case 2: // RPCL
		// For RPCL, precinct iteration is the outer loop so we need special handling
		// Per OpenJPEG, iterate over position coordinates in the reference grid
		totalDataCollected += td.parsePacketsRPCLBounded(br, resStart, resEnd, compStart, compEnd, layerEnd, processed)

	case 3: // PCRL
		// For PCRL with bounds, use the bounded version
		totalDataCollected += td.parsePacketsPCRLBounded(br, resStart, resEnd, compStart, compEnd, layerEnd, processed)

	case 4: // CPRL
		// For CPRL with bounds, use the bounded version
		totalDataCollected += td.parsePacketsCPRLBounded(br, resStart, resEnd, compStart, compEnd, layerEnd, processed)
	}

	return totalDataCollected
}

// parsePacketsWithOrder parses packets using a single progression order (no POC).
func (td *TileDecoder) parsePacketsWithOrder(br *bitReader, order byte, resStart, resEnd, compStart, compEnd, layerEnd int) int {
	h := td.header
	totalDataCollected := 0

	// Clamp to actual limits
	if resEnd > td.maxRes+1 {
		resEnd = td.maxRes + 1
	}
	if compEnd > h.NumComps {
		compEnd = h.NumComps
	}
	if layerEnd > td.maxLayers {
		layerEnd = td.maxLayers
	}

	switch order {
	case 0: // LRCP: Layer, Resolution, Component, Position
		for l := 0; l < layerEnd; l++ {
			for r := resStart; r < resEnd; r++ {
				for c := compStart; c < compEnd; c++ {
					// Use per-component precinct count to handle subsampled components
					numPX, numPY := td.getNumPrecinctsForComponent(r, c)
					for py := range numPY {
						for px := range numPX {
							dataRead := td.parseAndReadPacket(br, l, r, c, px, py)
							totalDataCollected += dataRead
						}
					}
				}
			}
		}

	case 1: // RLCP: Resolution, Layer, Component, Position
		for r := resStart; r < resEnd; r++ {
			for l := 0; l < layerEnd; l++ {
				for c := compStart; c < compEnd; c++ {
					// Use per-component precinct count to handle subsampled components
					numPX, numPY := td.getNumPrecinctsForComponent(r, c)
					for py := range numPY {
						for px := range numPX {
							dataRead := td.parseAndReadPacket(br, l, r, c, px, py)
							totalDataCollected += dataRead
						}
					}
				}
			}
		}

	case 2: // RPCL: Resolution, Position, Component, Layer
		// For RPCL, position (precinct) iteration is the outer loop after resolution.
		// Use PCRL implementation approach with proper grid-based position iteration.
		totalDataCollected += td.parsePacketsRPCL(br, layerEnd, resEnd-1)

	case 3: // PCRL: Position, Component, Resolution, Layer
		// Use the existing PCRL implementation
		totalDataCollected += td.parsePacketsPCRL(br, layerEnd, resEnd-1)

	case 4: // CPRL: Component, Position, Resolution, Layer
		// Use the existing CPRL implementation
		totalDataCollected += td.parsePacketsCPRL(br, layerEnd, resEnd-1)
	}

	return totalDataCollected
}

// parsePacketsRPCLBounded parses RPCL packets with bounds and duplicate tracking.
// RPCL = Resolution, Position, Component, Layer
// Per OpenJPEG, position iteration uses the reference grid coordinate system.
func (td *TileDecoder) parsePacketsRPCLBounded(br *bitReader, resStart, resEnd, compStart, compEnd, layerEnd int, processed map[packetKey]bool) int {
	h := td.header
	totalDataCollected := 0

	for r := resStart; r < resEnd; r++ {
		// Find minimum precinct step across all components at this resolution.
		// Per ITU-T T.800 B.12.1.4.3, RPCL iterates positions in reference grid
		// coordinates, so we need the reference-grid step (accounts for subsampling).
		minStepX, minStepY := 1<<30, 1<<30
		for c := compStart; c < compEnd; c++ {
			stepX, stepY := td.getPrecinctStepComponent(r, c)
			if stepX < minStepX {
				minStepX = stepX
			}
			if stepY < minStepY {
				minStepY = stepY
			}
		}

		// Get the tile bounds (reference grid) for position iteration.
		// Per ITU-T T.800 B.12.1.4.3, RPCL iterates positions in reference grid coordinates.
		tcX0, tcY0, tcX1, tcY1 := td.getComponentBounds()
		if tcX1 <= tcX0 || tcY1 <= tcY0 {
			continue
		}

		// Iterate over positions in reference grid coordinates.
		for y := tcY0; y < tcY1; y += posStep(y, minStepY) {
			for x := tcX0; x < tcX1; x += posStep(x, minStepX) {
				for c := compStart; c < compEnd; c++ {
					if c >= h.NumComps || c >= len(td.compResolutions) {
						continue
					}
					if r >= len(td.compResolutions[c]) {
						continue
					}

					// Get precinct step in reference grid coordinates for this component
					compStepX, compStepY := td.getPrecinctStepComponent(r, c)

					// Get resolution-level bounds and precinct step
					resX0, resY0, resX1, resY1 := td.getResBounds(c, r)
					if resX1 <= resX0 || resY1 <= resY0 {
						continue
					}
					prcStepX, prcStepY := td.getPrecinctStep(r, c)

					// Check grid alignment in reference grid coordinates.
					// Per OpenJPEG: allow the tile origin even when not grid-aligned,
					// if the resolution origin itself is not grid-aligned.
					xAligned := x%compStepX == 0 || (x == tcX0 && resX0%prcStepX != 0)
					yAligned := y%compStepY == 0 || (y == tcY0 && resY0%prcStepY != 0)
					if !xAligned || !yAligned {
						continue
					}

					// Convert from reference grid to resolution-level coordinates
					xrsiz := 1
					yrsiz := 1
					if c < len(h.XRsiz) && h.XRsiz[c] > 0 {
						xrsiz = h.XRsiz[c]
					}
					if c < len(h.YRsiz) && h.YRsiz[c] > 0 {
						yrsiz = h.YRsiz[c]
					}
					numRes := td.getComponentDecompLevels(c) + 1
					scale := 1
					if r < numRes-1 {
						scale = 1 << (numRes - 1 - r)
					}
					// Per ITU-T T.800 B-15: trX = ceil(tcX / 2^(NL-r))
					// Use ceil to match getResBounds() resolution origin computation.
					compX := x / xrsiz // floor per B-12
					compY := y / yrsiz
					resX := (compX + scale - 1) / scale // ceil per B-15
					resY := (compY + scale - 1) / scale

					// Calculate precinct indices in resolution-level coordinates
					px := resX/prcStepX - resX0/prcStepX
					py := resY/prcStepY - resY0/prcStepY

					// Validate precinct exists
					numPX, numPY := td.getNumPrecinctsForComponent(r, c)
					if px < 0 || px >= numPX || py < 0 || py >= numPY {
						continue
					}

					for l := range layerEnd {
						key := packetKey{l, r, c, px, py}
						if processed[key] {
							continue
						}
						processed[key] = true
						dataRead := td.parseAndReadPacket(br, l, r, c, px, py)
						totalDataCollected += dataRead
					}
				}
			}
		}
	}

	return totalDataCollected
}

// parsePacketsPCRLBounded parses PCRL packets with bounds and duplicate tracking.
// PCRL = Position, Component, Resolution, Layer
// Per ITU-T T.800 B.12.1.4.4, position iteration uses COMPONENT coordinates
// and the precinct step for each resolution is scaled to component coordinates.
func (td *TileDecoder) parsePacketsPCRLBounded(br *bitReader, resStart, resEnd, compStart, compEnd, layerEnd int, processed map[packetKey]bool) int {
	h := td.header
	totalDataCollected := 0

	// Find the minimum precinct step across all resolutions and components in COMPONENT coordinates.
	// Per ITU-T T.800 B.12.1.4.4, the position step for resolution r is
	// precinctSize(r) * scaleFactor(r) where scaleFactor = 2^(NL - r).
	minStepX, minStepY := 1<<30, 1<<30
	for c := compStart; c < compEnd; c++ {
		for r := resStart; r < resEnd; r++ {
			stepX, stepY := td.getPrecinctStepComponent(r, c)
			if stepX < minStepX {
				minStepX = stepX
			}
			if stepY < minStepY {
				minStepY = stepY
			}
		}
	}

	// Get the tile bounds in component coordinates.
	tcX0, tcY0, tcX1, tcY1 := td.getComponentBounds()
	if tcX1 <= tcX0 || tcY1 <= tcY0 {
		return 0
	}

	// Iterate over positions starting from the tile-component origin.
	// Per ITU-T T.800 / OpenJPEG, start at the tile origin and step to the
	// next grid-aligned position using posStep.
	for y := tcY0; y < tcY1; y += posStep(y, minStepY) {
		for x := tcX0; x < tcX1; x += posStep(x, minStepX) {
			for c := compStart; c < compEnd; c++ {
				if c >= h.NumComps || c >= len(td.compResolutions) {
					continue
				}
				for r := resStart; r < resEnd; r++ {
					if r >= len(td.compResolutions[c]) {
						continue
					}

					// Get precinct step in reference grid coordinates (accounts for subsampling)
					stepX, stepY := td.getPrecinctStepComponent(r, c)

					// Get resolution-level bounds
					resX0, resY0, resX1, resY1 := td.getResBounds(c, r)
					if resX1 <= resX0 || resY1 <= resY0 {
						continue
					}

					numRes := td.getComponentDecompLevels(c) + 1
					scale := 1
					if r < numRes-1 {
						scale = 1 << (numRes - 1 - r)
					}

					// Get precinct step in resolution-level coordinates
					prcStepX, prcStepY := td.getPrecinctStep(r, c)

					// Check grid alignment in reference grid coordinates.
					xAligned := x%stepX == 0 || (x == tcX0 && resX0%prcStepX != 0)
					yAligned := y%stepY == 0 || (y == tcY0 && resY0%prcStepY != 0)
					if !xAligned || !yAligned {
						continue
					}

					// Convert from reference grid to component coords, then to resolution level
					xrsiz := 1
					yrsiz := 1
					if c < len(h.XRsiz) && h.XRsiz[c] > 0 {
						xrsiz = h.XRsiz[c]
					}
					if c < len(h.YRsiz) && h.YRsiz[c] > 0 {
						yrsiz = h.YRsiz[c]
					}
					// Per ITU-T T.800 B-15: trX = ceil(tcX / 2^(NL-r))
					// Use ceil to match getResBounds() resolution origin computation.
					compX := x / xrsiz // floor per B-12
					compY := y / yrsiz
					resX := (compX + scale - 1) / scale // ceil per B-15
					resY := (compY + scale - 1) / scale

					// Calculate precinct indices
					px := resX/prcStepX - resX0/prcStepX
					py := resY/prcStepY - resY0/prcStepY

					// Validate precinct exists
					numPX, numPY := td.getNumPrecinctsForComponent(r, c)
					if px < 0 || px >= numPX || py < 0 || py >= numPY {
						continue
					}

					for l := range layerEnd {
						key := packetKey{l, r, c, px, py}
						if processed[key] {
							continue
						}
						processed[key] = true
						dataRead := td.parseAndReadPacket(br, l, r, c, px, py)
						totalDataCollected += dataRead
					}
				}
			}
		}
	}

	return totalDataCollected
}

// parsePacketsCPRLBounded parses CPRL packets with bounds and duplicate tracking.
// CPRL = Component, Position, Resolution, Layer
// Per ITU-T T.800 B.12.1.4.5, position iteration uses component coordinates.
func (td *TileDecoder) parsePacketsCPRLBounded(br *bitReader, resStart, resEnd, compStart, compEnd, layerEnd int, processed map[packetKey]bool) int {
	h := td.header
	totalDataCollected := 0

	for c := compStart; c < compEnd; c++ {
		if c >= len(td.compResolutions) {
			continue
		}

		// Get the component-specific max resolution level
		compMaxRes := min(len(td.compResolutions[c])-1, resEnd-1)
		if compMaxRes < resStart {
			continue
		}

		// Get subsampling factors
		xrsiz := 1
		yrsiz := 1
		if c < len(h.XRsiz) && h.XRsiz[c] > 0 {
			xrsiz = h.XRsiz[c]
		}
		if c < len(h.YRsiz) && h.YRsiz[c] > 0 {
			yrsiz = h.YRsiz[c]
		}

		// Find the minimum precinct step across all resolutions in component coordinates
		minStepX, minStepY := 1<<30, 1<<30
		for r := resStart; r <= compMaxRes; r++ {
			refStepX, refStepY := td.getPrecinctStepComponent(r, c)
			// Convert from reference grid to component coordinates
			sX := refStepX / xrsiz
			sY := refStepY / yrsiz
			if sX < minStepX {
				minStepX = sX
			}
			if sY < minStepY {
				minStepY = sY
			}
		}

		// Get the tile-component bounds in component coordinates (ceil for all)
		tcX0 := (td.tile.X0 + xrsiz - 1) / xrsiz
		tcY0 := (td.tile.Y0 + yrsiz - 1) / yrsiz
		tcX1 := (td.tile.X1 + xrsiz - 1) / xrsiz
		tcY1 := (td.tile.Y1 + yrsiz - 1) / yrsiz
		if tcX1 <= tcX0 || tcY1 <= tcY0 {
			continue
		}

		// Iterate over positions in component coordinates.
		for y := tcY0; y < tcY1; y += posStep(y, minStepY) {
			for x := tcX0; x < tcX1; x += posStep(x, minStepX) {
				for r := resStart; r <= compMaxRes; r++ {
					// Get precinct step in component coordinates
					refStepX, refStepY := td.getPrecinctStepComponent(r, c)
					stepX := refStepX / xrsiz
					stepY := refStepY / yrsiz

					// Get resolution-level bounds
					resX0, resY0, resX1, resY1 := td.getResBounds(c, r)
					if resX1 <= resX0 || resY1 <= resY0 {
						continue
					}

					numRes := td.getComponentDecompLevels(c) + 1
					scale := 1
					if r < numRes-1 {
						scale = 1 << (numRes - 1 - r)
					}

					// Get precinct step in resolution-level coordinates
					prcStepX, prcStepY := td.getPrecinctStep(r, c)

					// Check grid alignment in component coordinates.
					xAligned := x%stepX == 0 || (x == tcX0 && resX0%prcStepX != 0)
					yAligned := y%stepY == 0 || (y == tcY0 && resY0%prcStepY != 0)
					if !xAligned || !yAligned {
						continue
					}

					// x is already in component coords, convert to resolution level.
					// Per ITU-T T.800 B-15: use ceil to match getResBounds().
					resX := (x + scale - 1) / scale // ceil per B-15
					resY := (y + scale - 1) / scale

					// Calculate precinct indices in resolution-level coordinates
					px := resX/prcStepX - resX0/prcStepX
					py := resY/prcStepY - resY0/prcStepY

					// Validate precinct exists
					numPX, numPY := td.getNumPrecinctsForComponent(r, c)
					if px < 0 || px >= numPX || py < 0 || py >= numPY {
						continue
					}

					for l := range layerEnd {
						key := packetKey{l, r, c, px, py}
						if processed[key] {
							continue
						}
						processed[key] = true
						dataRead := td.parseAndReadPacket(br, l, r, c, px, py)
						totalDataCollected += dataRead
					}
				}
			}
		}
	}

	return totalDataCollected
}

// getPrecinctStep returns the precinct step size (in resolution-level coordinates) for a resolution.
// Per ITU-T T.800, precinct size = 2^PP, and at each resolution the precinct step in the
// reference grid coordinate system needs to account for the wavelet decomposition.
func (td *TileDecoder) getPrecinctStep(res, comp int) (int, int) {
	h := td.header
	// Get precinct size exponents for this resolution level and component
	ppx, ppy := h.PrecinctSizeForRes(res, comp)
	return 1 << ppx, 1 << ppy
}

// getPrecinctStepComponent returns the precinct step in REFERENCE GRID coordinates
// for resolution r of the given component. Per ITU-T T.800 B.12 and OpenJPEG,
// the step is XRsiz[comp] * 2^(PPx[r,comp] + NL - r) where NL = numDecompLevels.
// This accounts for component subsampling so the position iteration in PCRL/CPRL
// uses reference grid (image-space) coordinates.
func (td *TileDecoder) getPrecinctStepComponent(res, comp int) (int, int) {
	h := td.header
	ppx, ppy := h.PrecinctSizeForRes(res, comp)
	numRes := td.getComponentDecompLevels(comp) + 1
	scale := max(numRes-1-res, 0)
	// Multiply by subsampling factor to get reference grid step
	xrsiz := 1
	yrsiz := 1
	if comp < len(h.XRsiz) && h.XRsiz[comp] > 0 {
		xrsiz = h.XRsiz[comp]
	}
	if comp < len(h.YRsiz) && h.YRsiz[comp] > 0 {
		yrsiz = h.YRsiz[comp]
	}
	return xrsiz * (1 << (ppx + scale)), yrsiz * (1 << (ppy + scale))
}

// posStep advances position p to the next multiple of step.
// Per ITU-T T.800 / OpenJPEG, position iteration starts at the tile origin
// and advances by (step - p%step) to reach the next grid-aligned position.
func posStep(p, step int) int {
	rem := p % step
	if rem == 0 {
		return step
	}
	return step - rem
}

// getComponentBounds returns the tile bounds in component coordinates (using component 0).
func (td *TileDecoder) getComponentBounds() (int, int, int, int) {
	return td.tile.X0, td.tile.Y0, td.tile.X1, td.tile.Y1
}

// getResBounds returns the resolution-level bounds (trX0, trY0, trX1, trY1) for a component
func (td *TileDecoder) getResBounds(comp, res int) (int, int, int, int) {
	h := td.header
	// Get component subsampling
	xrsiz := 1
	yrsiz := 1
	if comp < len(h.XRsiz) && h.XRsiz[comp] > 0 {
		xrsiz = h.XRsiz[comp]
	}
	if comp < len(h.YRsiz) && h.YRsiz[comp] > 0 {
		yrsiz = h.YRsiz[comp]
	}

	// Per OpenJPEG tcd.c: ceil for all tile-component bounds
	tcX0 := (td.tile.X0 + xrsiz - 1) / xrsiz
	tcY0 := (td.tile.Y0 + yrsiz - 1) / yrsiz
	tcX1 := (td.tile.X1 + xrsiz - 1) / xrsiz
	tcY1 := (td.tile.Y1 + yrsiz - 1) / yrsiz

	// Get number of resolutions for this component
	numRes := td.getComponentDecompLevels(comp) + 1
	if res >= numRes {
		return 0, 0, 0, 0
	}

	// Calculate scale factor for this resolution level
	scale := 1 << (numRes - 1 - res)
	if scale == 0 {
		scale = 1
	}

	// Per ITU-T T.800 equation B-15
	trX0 := (tcX0 + scale - 1) / scale
	trY0 := (tcY0 + scale - 1) / scale
	trX1 := (tcX1 + scale - 1) / scale
	trY1 := (tcY1 + scale - 1) / scale

	return trX0, trY0, trX1, trY1
}

// parsePacketsRPCL parses packets in RPCL (Resolution, Position, Component, Layer) order.
// Per ITU-T T.800 Annex B.12.1.4.3, this iterates resolution first, then global position
// coordinates in reference grid using the smallest precinct step for each resolution.
func (td *TileDecoder) parsePacketsRPCL(br *bitReader, maxLayers, maxRes int) int {
	h := td.header
	totalDataCollected := 0

	for r := 0; r <= maxRes; r++ {
		// Find minimum precinct step across all components at this resolution
		// in reference grid coordinates (accounts for subsampling).
		minStepX, minStepY := 1<<30, 1<<30
		for c := 0; c < h.NumComps; c++ {
			stepX, stepY := td.getPrecinctStepComponent(r, c)
			if stepX < minStepX {
				minStepX = stepX
			}
			if stepY < minStepY {
				minStepY = stepY
			}
		}

		// Get tile bounds (reference grid) for position iteration.
		tcX0, tcY0, tcX1, tcY1 := td.getComponentBounds()
		if tcX1 <= tcX0 || tcY1 <= tcY0 {
			continue
		}

		// Iterate over positions in reference grid coordinates.
		for y := tcY0; y < tcY1; y += posStep(y, minStepY) {
			for x := tcX0; x < tcX1; x += posStep(x, minStepX) {
				for c := 0; c < h.NumComps; c++ {
					if c >= len(td.compResolutions) || r >= len(td.compResolutions[c]) {
						continue
					}

					// Get precinct step in reference grid coordinates for this component
					compStepX, compStepY := td.getPrecinctStepComponent(r, c)

					// Get resolution-level bounds and precinct step
					resX0, resY0, resX1, resY1 := td.getResBounds(c, r)
					if resX1 <= resX0 || resY1 <= resY0 {
						continue
					}
					prcStepX, prcStepY := td.getPrecinctStep(r, c)

					// Check grid alignment in reference grid coordinates.
					xAligned := x%compStepX == 0 || (x == tcX0 && resX0%prcStepX != 0)
					yAligned := y%compStepY == 0 || (y == tcY0 && resY0%prcStepY != 0)
					if !xAligned || !yAligned {
						continue
					}

					// Convert from reference grid to resolution-level coordinates
					xrsiz := 1
					yrsiz := 1
					if c < len(h.XRsiz) && h.XRsiz[c] > 0 {
						xrsiz = h.XRsiz[c]
					}
					if c < len(h.YRsiz) && h.YRsiz[c] > 0 {
						yrsiz = h.YRsiz[c]
					}
					numRes := td.getComponentDecompLevels(c) + 1
					scale := 1
					if r < numRes-1 {
						scale = 1 << (numRes - 1 - r)
					}
					// Per ITU-T T.800 B-15: trX = ceil(tcX / 2^(NL-r))
					// Use ceil to match getResBounds() resolution origin computation.
					compX := x / xrsiz // floor per B-12
					compY := y / yrsiz
					resX := (compX + scale - 1) / scale // ceil per B-15
					resY := (compY + scale - 1) / scale

					// Calculate precinct indices in resolution-level coordinates
					px := resX/prcStepX - resX0/prcStepX
					py := resY/prcStepY - resY0/prcStepY

					// Validate precinct exists
					numPX, numPY := td.getNumPrecinctsForComponent(r, c)
					if px < 0 || px >= numPX || py < 0 || py >= numPY {
						continue
					}

					for l := range maxLayers {
						dataRead := td.parseAndReadPacket(br, l, r, c, px, py)
						totalDataCollected += dataRead
					}
				}
			}
		}
	}

	return totalDataCollected
}

// parsePacketsPCRL parses packets in PCRL (Position, Component, Resolution, Layer) order.
// Per ITU-T T.800 Annex B.12.1.4.4, this iterates over global position coordinates
// using the smallest precinct step across all resolutions.
// All position iteration uses COMPONENT coordinates (tcX0..tcX1), and the precinct
// step for each resolution is scaled to component coordinates.
func (td *TileDecoder) parsePacketsPCRL(br *bitReader, maxLayers, maxRes int) int {
	h := td.header
	totalDataCollected := 0

	// Find the minimum precinct step across all resolutions and components
	// in COMPONENT coordinates (not resolution-level coordinates).
	// Per ITU-T T.800 B.12.1.4.4, the position step for resolution r is
	// precinctSize(r) * scaleFactor(r) where scaleFactor = 2^(NL - r).
	minStepX, minStepY := 1<<30, 1<<30
	for c := 0; c < h.NumComps; c++ {
		for r := 0; r <= maxRes; r++ {
			stepX, stepY := td.getPrecinctStepComponent(r, c)
			if stepX < minStepX {
				minStepX = stepX
			}
			if stepY < minStepY {
				minStepY = stepY
			}
		}
	}

	// Get the tile bounds in component coordinates.
	// Use the maximum extent across all components.
	tcX0, tcY0, tcX1, tcY1 := td.getComponentBounds()
	if tcX1 <= tcX0 || tcY1 <= tcY0 {
		return 0
	}

	// Iterate over positions starting from the tile-component origin.
	for y := tcY0; y < tcY1; y += posStep(y, minStepY) {
		for x := tcX0; x < tcX1; x += posStep(x, minStepX) {
			for c := 0; c < h.NumComps; c++ {
				for r := 0; r <= maxRes; r++ {
					// Get precinct step in reference grid coordinates (accounts for subsampling)
					stepX, stepY := td.getPrecinctStepComponent(r, c)

					// Get resolution-level bounds
					resX0, resY0, resX1, resY1 := td.getResBounds(c, r)
					if resX1 <= resX0 || resY1 <= resY0 {
						continue
					}

					numRes := td.getComponentDecompLevels(c) + 1
					scale := 1
					if r < numRes-1 {
						scale = 1 << (numRes - 1 - r)
					}

					// Get precinct step in resolution-level coordinates
					prcStepX, prcStepY := td.getPrecinctStep(r, c)

					// Check grid alignment in reference grid coordinates.
					// Per OpenJPEG: allow the tile origin even when not grid-aligned,
					// if the resolution origin itself is not grid-aligned.
					xAligned := x%stepX == 0 || (x == tcX0 && resX0%prcStepX != 0)
					yAligned := y%stepY == 0 || (y == tcY0 && resY0%prcStepY != 0)
					if !xAligned || !yAligned {
						continue
					}

					// Convert from reference grid to component coords, then to resolution level
					xrsiz := 1
					yrsiz := 1
					if c < len(h.XRsiz) && h.XRsiz[c] > 0 {
						xrsiz = h.XRsiz[c]
					}
					if c < len(h.YRsiz) && h.YRsiz[c] > 0 {
						yrsiz = h.YRsiz[c]
					}
					// Per ITU-T T.800 B-15: trX = ceil(tcX / 2^(NL-r))
					// Use ceil to match getResBounds() resolution origin computation.
					compX := x / xrsiz // floor per B-12
					compY := y / yrsiz
					resX := (compX + scale - 1) / scale // ceil per B-15
					resY := (compY + scale - 1) / scale

					// Calculate precinct indices from resolution-level position.
					px := resX/prcStepX - resX0/prcStepX
					py := resY/prcStepY - resY0/prcStepY

					// Validate precinct exists
					numPX, numPY := td.getNumPrecinctsForComponent(r, c)
					if px < 0 || px >= numPX || py < 0 || py >= numPY {
						continue
					}

					for l := range maxLayers {
						dataRead := td.parseAndReadPacket(br, l, r, c, px, py)
						totalDataCollected += dataRead
					}
				}
			}
		}
	}

	return totalDataCollected
}

// parsePacketsCPRL parses packets in CPRL (Component, Position, Resolution, Layer) order.
// Per ITU-T T.800 Annex B.12.1.4.5, this is the same as PCRL but with component as
// the outermost loop. Position iteration uses component coordinates.
func (td *TileDecoder) parsePacketsCPRL(br *bitReader, maxLayers, maxRes int) int {
	h := td.header
	totalDataCollected := 0

	for c := 0; c < h.NumComps; c++ {
		// Get the component-specific max resolution level
		compMaxRes := min(len(td.compResolutions[c])-1, maxRes)
		if compMaxRes < 0 {
			continue
		}

		// Get subsampling factors
		xrsiz := 1
		yrsiz := 1
		if c < len(h.XRsiz) && h.XRsiz[c] > 0 {
			xrsiz = h.XRsiz[c]
		}
		if c < len(h.YRsiz) && h.YRsiz[c] > 0 {
			yrsiz = h.YRsiz[c]
		}

		// Find the minimum precinct step across all resolutions in component coordinates
		minStepX, minStepY := 1<<30, 1<<30
		for r := 0; r <= compMaxRes; r++ {
			refStepX, refStepY := td.getPrecinctStepComponent(r, c)
			// Convert from reference grid to component coordinates
			sX := refStepX / xrsiz
			sY := refStepY / yrsiz
			if sX < minStepX {
				minStepX = sX
			}
			if sY < minStepY {
				minStepY = sY
			}
		}

		// Get the tile-component bounds in component coordinates (ceil for all)
		tcX0 := (td.tile.X0 + xrsiz - 1) / xrsiz
		tcY0 := (td.tile.Y0 + yrsiz - 1) / yrsiz
		tcX1 := (td.tile.X1 + xrsiz - 1) / xrsiz
		tcY1 := (td.tile.Y1 + yrsiz - 1) / yrsiz
		if tcX1 <= tcX0 || tcY1 <= tcY0 {
			continue
		}

		// Iterate over positions starting from the tile-component origin.
		for y := tcY0; y < tcY1; y += posStep(y, minStepY) {
			for x := tcX0; x < tcX1; x += posStep(x, minStepX) {
				for r := 0; r <= compMaxRes; r++ {
					// Get precinct step in component coordinates (divide ref-grid step by xrsiz)
					refStepX, refStepY := td.getPrecinctStepComponent(r, c)
					stepX := refStepX / xrsiz
					stepY := refStepY / yrsiz

					// Get resolution-level bounds
					resX0, resY0, resX1, resY1 := td.getResBounds(c, r)
					if resX1 <= resX0 || resY1 <= resY0 {
						continue
					}

					numRes := td.getComponentDecompLevels(c) + 1
					scale := 1
					if r < numRes-1 {
						scale = 1 << (numRes - 1 - r)
					}

					// Get precinct step in resolution-level coordinates
					prcStepX, prcStepY := td.getPrecinctStep(r, c)

					// Check grid alignment in component coordinates.
					// Per OpenJPEG: allow the tile origin even when not grid-aligned,
					// if the resolution origin itself is not grid-aligned.
					xAligned := x%stepX == 0 || (x == tcX0 && resX0%prcStepX != 0)
					yAligned := y%stepY == 0 || (y == tcY0 && resY0%prcStepY != 0)
					if !xAligned || !yAligned {
						continue
					}

					// x is already in component coords, convert to resolution level.
					// Per ITU-T T.800 B-15: use ceil to match getResBounds().
					resX := (x + scale - 1) / scale // ceil per B-15
					resY := (y + scale - 1) / scale

					// Calculate precinct indices from resolution-level position.
					px := resX/prcStepX - resX0/prcStepX
					py := resY/prcStepY - resY0/prcStepY

					// Validate precinct exists
					numPX, numPY := td.getNumPrecinctsForComponent(r, c)
					if px < 0 || px >= numPX || py < 0 || py >= numPY {
						continue
					}

					for l := range maxLayers {
						dataRead := td.parseAndReadPacket(br, l, r, c, px, py)
						totalDataCollected += dataRead
					}
				}
			}
		}
	}

	return totalDataCollected
}

// parseAndReadPacket parses a packet header and immediately reads its data
// This is the correct JPEG2000 structure: header followed by data for each packet
// precinctX, precinctY: precinct indices within the resolution level
func (td *TileDecoder) parseAndReadPacket(br *bitReader, layer, res, comp, precinctX, precinctY int) int {
	// Per ITU-T T.800, packet data is byte-aligned.
	// Ensure we start at a byte boundary before each packet.
	br.ByteAlign()

	// Check if this resolution exists for this component (per-component COC support)
	if comp >= 0 && comp < len(td.compResolutions) {
		if res >= len(td.compResolutions[comp]) {
			// Resolution doesn't exist for this component, skip
			return 0
		}
	}

	// Check for SOP (Start of Packet) marker: 0xFF91
	// Per ITU-T T.800, SOP marker is followed by 4-byte segment:
	// - Lsop (2 bytes): segment length = 4
	// - Nsop (2 bytes): packet sequence number (mod 65536)
	// SOP markers are always in the tile data stream (br), even when PPT is used
	sopSkipped := false
	sopNsop := -1 // packet sequence number from SOP, -1 if not present
	if br.Remaining() >= 6 {
		// Peek at next two bytes to check for SOP marker
		b0, _ := br.PeekByte(0)
		b1, _ := br.PeekByte(1)
		if b0 == 0xFF && b1 == 0x91 {
			// Read SOP marker (2 bytes)
			br.ReadByte()
			br.ReadByte()
			// Read Lsop (2 bytes) - should be 4
			br.ReadByte()
			br.ReadByte()
			// Read Nsop (2 bytes) - packet sequence number
			nsopHi, _ := br.ReadByte()
			nsopLo, _ := br.ReadByte()
			sopNsop = int(nsopHi)<<8 | int(nsopLo)
			sopSkipped = true
		}
	}
	_ = sopNsop    // Nsop can be used for packet sequence validation if needed
	_ = sopSkipped // SOP presence tracked for potential validation

	// Determine which reader to use for packet headers
	// When PPT is present, packet headers are in pptReader, otherwise in tile data (br)
	headerReader := br
	if td.pptReader != nil {
		headerReader = td.pptReader
	}

	// Capture trace state before parsing (if tracing enabled)
	var trace *PacketTrace
	if td.Trace != nil {
		trace = &PacketTrace{
			Layer:         layer,
			Resolution:    res,
			Component:     comp,
			PrecinctX:     precinctX,
			PrecinctY:     precinctY,
			BytePosBefore: headerReader.Position(),
			BitPosBefore:  headerReader.BitPosition(),
		}
		// Capture first few bytes for debugging
		trace.FirstBytes = make([]byte, 0, 4)
		for i := 0; i < 4 && headerReader.Position()+i < headerReader.Len(); i++ {
			trace.FirstBytes = append(trace.FirstBytes, headerReader.ByteAt(headerReader.Position()+i))
		}
	}

	// Enable bit-stuffing for packet header parsing.
	// Per ITU-T T.800, after reading a 0xFF byte in the packet header,
	// the MSB of the next byte is a stuffed 0-bit that must be skipped.
	//
	// For PPT headers, use stuffOnly mode: bit-stuff without marker detection.
	// PPT concatenates all packet headers into a single byte stream. When a header
	// byte is 0xFF followed by an EPH marker byte (0x92, which has MSB=1), normal
	// bit-stuffing would incorrectly detect this as a marker and truncate the header.
	// In stuffOnly mode, we always skip the stuffed bit regardless of MSB value,
	// and EPH markers are handled explicitly after header parsing.
	headerReader.SetBitStuffing(true)
	entries, emptyBit := td.parsePacketHeaderWithEmptyBit(headerReader, layer, res, comp, precinctX, precinctY)

	// CRITICAL: Always byte-align and check for EPH after parsing the packet header,
	// even for empty packets. Per ITU-T T.800, EPH marker appears at the end of EVERY
	// packet header when csty bit 2 is set. Without this, empty packets leave EPH
	// markers unconsumed, causing all subsequent packet parsing to be misaligned.
	headerReader.ByteAlign()

	// Disable bit-stuffing before reading code block data (data has its own framing)
	headerReader.SetBitStuffing(false)

	// PPT synchronization fix: When PPT is used, we must ensure the tile data reader
	// is byte-aligned before reading packet body data. While br.ByteAlign() was called
	// at the start of this function, ensure we're still aligned after any SOP handling.
	// This is critical for multi-tile images where desync between PPT headers and
	// tile body data can cause progressive corruption.
	if td.pptReader != nil {
		br.ByteAlign()
	}

	// Check for EPH (End of Packet Header) marker: 0xFF92
	// EPH appears after the packet header is complete, before code block data.
	// Unlike SOP, EPH has no length field - it's just the 2-byte marker.
	// EPH is in the header stream (pptReader when PPT is used)
	if headerReader.Remaining() >= 2 {
		b0, _ := headerReader.PeekByte(0)
		b1, _ := headerReader.PeekByte(1)
		if b0 == 0xFF && b1 == 0x92 {
			// Skip EPH marker (2 bytes)
			headerReader.ReadByte()
			headerReader.ReadByte()
		}
	}

	// Read data for all code blocks in this packet
	dataRead := 0

	if len(entries) > 0 {
		// OpenJPEG-style graceful degradation:
		// If we don't have enough data, read full entries until data runs out.
		// When we can't read a full entry, mark the code block as incomplete and stop.
		// Do NOT read partial data - this maintains proper decoder state.

		for _, entry := range entries {
			if entry.length > 0 {
				if br.Remaining() < entry.length {
					// Mark as insufficient and stop reading more entries
					// Don't read partial data - leave this code block incomplete
					entry.cb.newLength = 0
					entry.cb.newPasses = 0
					entry.cb.newSegmentLengths = nil
					continue // Skip remaining entries
				}

				data := make([]byte, entry.length)
				for j := 0; j < entry.length; j++ {
					b, err := br.ReadByte()
					if err != nil {
						break
					}
					data[j] = b
				}
				entry.cb.Data = append(entry.cb.Data, data...)
				dataRead += len(data)
			}
			// Always count passes, even when data length is 0.
			// A layer can contribute additional coding passes without new data bytes;
			// the MQ decoder continues processing from the existing data stream.
			entry.cb.TotalPasses += entry.cb.newPasses
			// Accumulate segment lengths for ERTERM or BYPASS mode
			cbStyle := td.getCodeBlockStyle()
			isBypassMode := cbStyle&0x01 != 0 && cbStyle&0x04 == 0
			if entry.cb.newSegmentLengths != nil {
				if isBypassMode && entry.cb.bypassPassesInSeg > 0 && len(entry.cb.SegmentLengths) > 0 && len(entry.cb.newSegmentLengths) > 0 {
					// BYPASS multi-layer: first new length continues the current logical segment.
					// Merge it into the last existing segment length entry.
					entry.cb.SegmentLengths[len(entry.cb.SegmentLengths)-1] += entry.cb.newSegmentLengths[0]
					entry.cb.SegmentLengths = append(entry.cb.SegmentLengths, entry.cb.newSegmentLengths[1:]...)
				} else {
					entry.cb.SegmentLengths = append(entry.cb.SegmentLengths, entry.cb.newSegmentLengths...)
				}
			}
			// Advance BYPASS segment state for multi-layer tracking
			if isBypassMode {
				advanceBypassSegState(entry.cb, entry.cb.newPasses)
			}
			entry.cb.newLength = 0
			entry.cb.newPasses = 0
			entry.cb.newSegmentLengths = nil
		}
	}

	// Record trace after parsing (if tracing enabled)
	if trace != nil {
		trace.BytePosAfter = headerReader.Position()
		trace.BitPosAfter = headerReader.BitPosition()
		trace.EmptyBit = emptyBit
		trace.NumEntries = len(entries)
		trace.DataRead = dataRead
		trace.SubbandEntries = td.lastSubbandEntries
		td.Trace = append(td.Trace, *trace)
	}

	return dataRead
}

// parsePacketHeader parses a single packet header and returns code blocks pending data
// precinctX, precinctY: precinct indices within the resolution level
func (td *TileDecoder) parsePacketHeader(br *bitReader, layer, res, comp, precinctX, precinctY int) []cbEntry {
	entries, _ := td.parsePacketHeaderWithEmptyBit(br, layer, res, comp, precinctX, precinctY)
	return entries
}

// parsePacketHeaderWithEmptyBit parses a single packet header and returns code blocks
// pending data along with the empty packet indicator value (for tracing).
// precinctX, precinctY: precinct indices within the resolution level
func (td *TileDecoder) parsePacketHeaderWithEmptyBit(br *bitReader, layer, res, comp, precinctX, precinctY int) ([]cbEntry, int) {
	var entries []cbEntry

	if br.Remaining() < 1 {
		return entries, -1 // -1 indicates couldn't read
	}

	// Read empty packet indicator
	emptyBit, err := br.ReadBit()
	if err != nil {
		return entries, -1
	}

	if emptyBit == 0 {
		// Empty packet - no code blocks
		return entries, 0
	}

	// Non-empty packet - parse code block contributions
	if comp >= len(td.compResolutions) || res >= len(td.compResolutions[comp]) {
		return entries, 1 // Resolution/component out of bounds
	}
	resLevel := td.compResolutions[comp][res]

	// Parse each subband's code block contributions for this precinct
	var sbEntryCounts []int
	for _, sb := range resLevel.Subbands {
		sbEntries := td.parseSubbandHeader(br, layer, res, sb, comp, precinctX, precinctY)
		sbEntryCounts = append(sbEntryCounts, len(sbEntries))
		entries = append(entries, sbEntries...)
	}
	td.lastSubbandEntries = sbEntryCounts

	return entries, 1
}

// parseSubbandHeader parses code block headers for a subband within a specific precinct
// precinctX, precinctY: precinct indices within the resolution level
func (td *TileDecoder) parseSubbandHeader(br *bitReader, layer int, res int, sb *Subband, comp int, precinctX, precinctY int) []cbEntry {
	var entries []cbEntry

	// Get precinct-specific tag trees
	precinctIdx := precinctY*sb.NumPrecinctsX + precinctX
	if precinctIdx >= len(sb.PrecinctInclusionTrees) || precinctIdx >= len(sb.PrecinctZBPTrees) {
		return entries
	}
	inclusionTree := sb.PrecinctInclusionTrees[precinctIdx]
	zbpTree := sb.PrecinctZBPTrees[precinctIdx]

	if inclusionTree == nil || zbpTree == nil {
		return entries
	}

	// Calculate code block range for this precinct
	// Get code block size, capped by precinct size (stored during subband creation)
	h := td.header
	cbWidth, cbHeight := h.ComponentCodeBlockSize(comp)
	// Cap code block size by subband precinct size (same logic as createSubband)
	cbWidth = min(cbWidth, sb.PrecinctWidth)
	cbHeight = min(cbHeight, sb.PrecinctHeight)

	// Calculate code block range for this precinct using ITU-T T.800 B.7.
	// Uses the resolution-level precinct grid origin stored in the subband.
	cbX0, cbX1, cbY0, cbY1 := precinctCodeBlockRange(
		precinctX, precinctY, sb.PrcGridX0, sb.PrcGridY0,
		sb.PrecinctWidth, sb.PrecinctHeight,
		sb.X0, sb.Y0, sb.X0+sb.Width, sb.Y0+sb.Height,
		cbWidth, cbHeight, sb.CodeBlocksX, sb.CodeBlocksY,
	)

	// Iterate only code blocks within this precinct
	for y := cbY0; y < cbY1; y++ {
		for x := cbX0; x < cbX1; x++ {
			cb := sb.CodeBlocks[y][x]

			// Local coordinates within precinct (for tag tree access)
			localX := x - cbX0
			localY := y - cbY0

			// Determine if code block is included in this packet
			included := false

			if cb.IncludedLayer < 0 {
				// First time seeing this code block - use tag tree for inclusion
				// The tag tree encodes the first layer where this code block appears
				isIncluded, err := inclusionTree.decodeInclusion(localX, localY, int32(layer), br)
				if err != nil {
					return entries
				}
				if isIncluded {
					cb.IncludedLayer = layer
					included = true

					// Read ZBP only on first inclusion
					zbp, err := zbpTree.decodeZBP(localX, localY, br)
					if err != nil {
						return entries
					}
					cb.ZeroBitPlanes = int(zbp)
				}
			} else {
				// Previously included - read 1-bit inclusion flag
				bit, err := br.ReadBit()
				if err != nil {
					return entries
				}
				included = bit == 1
			}

			if !included {
				continue
			}

			// Read number of new coding passes
			numPasses, err := td.readNumPasses(br)
			if err != nil {
				return entries
			}

			// Sanity check: max passes is about 30 (3 passes * 10 bit planes)
			if numPasses > 50 || numPasses < 1 {
				// Return what we have so far - bit stream is likely out of sync
				return entries
			}

			// Per ITU-T T.800 B.10.6, comma code for Lblock increment comes after numPasses
			increment, err := td.readCommaCode(br)
			if err != nil {
				return entries
			}
			cb.Lblock += increment

			// Read code block data length
			// ERTERM (cbstyTerminate = 0x04): each pass has its own terminated segment
			// Use tile-specific CodeBlockStyle if available (from tile-part COD marker)
			var length int
			var segmentLengths []int
			cbStyle := td.getCodeBlockStyle()
			if cbStyle&0x04 != 0 {
				// ERTERM mode: read per-pass segment lengths
				length, segmentLengths, err = td.readDataLengthsERTERM(br, cb, numPasses)
				if err != nil {
					return entries
				}
			} else if cbStyle&0x01 != 0 {
				// BYPASS mode (without ERTERM): read per-segment lengths.
				// Uses persistent segment tracking state on cb to handle
				// multi-layer scenarios where a segment can span quality layers.
				numSegs := computeBypassNewSegments(cb, numPasses)
				if numSegs <= 1 {
					// Single segment contribution - same as normal mode
					length, err = td.readDataLength(br, cb, numPasses)
					if err != nil {
						return entries
					}
					if numSegs == 1 {
						segmentLengths = []int{length}
					}
				} else {
					length, segmentLengths, err = td.readDataLengthsBypass(br, cb, numPasses)
					if err != nil {
						return entries
					}
				}
			} else {
				// Normal mode: read single total length
				length, err = td.readDataLength(br, cb, numPasses)
				if err != nil {
					return entries
				}
			}

			// Sanity check: length shouldn't exceed a reasonable amount
			if length > 100000 {
				return entries
			}

			cb.newPasses = numPasses
			cb.newLength = length
			cb.newSegmentLengths = segmentLengths

			// Add to pending data list
			entries = append(entries, cbEntry{cb: cb, length: length})
		}
	}

	return entries
}

// readCommaCode reads a comma code value (count of consecutive 1-bits until 0)
// Per OpenJPEG: opj_t2_getcommacode counts consecutive 1-bits until encountering a 0
func (td *TileDecoder) readCommaCode(br *bitReader) (int, error) {
	n := 0
	for {
		bit, err := br.ReadBit()
		if err != nil {
			return n, nil
		}
		if bit == 0 {
			break
		}
		n++
		if n > 16 { // Safety limit
			break
		}
	}
	return n, nil
}

// readZeroBitPlanes reads the number of leading zero bit-planes
// Uses a simple unary+binary encoding
func (td *TileDecoder) readZeroBitPlanes(br *bitReader) (int, error) {
	// Count leading 1s (unary part)
	count := 0
	for {
		bit, err := br.ReadBit()
		if err != nil {
			return count, nil
		}
		if bit == 0 {
			break
		}
		count++
		if count > 31 { // Safety limit
			break
		}
	}
	return count, nil
}

// readNumPasses reads the number of new coding passes
// NOTE: This follows OpenJPEG's implementation which differs from ITU-T T.800 B.10.5.
// Most real-world JPEG2000 encoders follow OpenJPEG's behavior.
//
// OpenJPEG encoding:
// 0                  → 1 pass
// 10                 → 2 passes
// 1100               → 3 passes (11 + 00)
// 1101               → 4 passes (11 + 01)
// 1110               → 5 passes (11 + 10)
// 1111 xxxxx         → 6-37 passes (11 + 11 + 5 bits, add to 6)
// 1111 11111 xxxxxxx → 37+ passes (all 1s prefix + 7 bits, add to 37)
func (td *TileDecoder) readNumPasses(br *bitReader) (int, error) {
	bit1, err := br.ReadBit()
	if err != nil {
		return 1, nil
	}
	if bit1 == 0 {
		return 1, nil // Pattern: 0
	}

	bit2, err := br.ReadBit()
	if err != nil {
		return 1, nil
	}
	if bit2 == 0 {
		return 2, nil // Pattern: 10
	}

	// We have "11", read bits 3-4 as a 2-bit value
	n, err := br.ReadBits(2)
	if err != nil {
		return 3, nil
	}

	// OpenJPEG: if n != 3, return 3 + n
	// 00 → 3, 01 → 4, 10 → 5, 11 → continue
	if n != 3 {
		return 3 + int(n), nil
	}

	// Pattern: 1111, read 5 more bits
	extra, err := br.ReadBits(5)
	if err != nil {
		return 6, nil
	}

	// OpenJPEG: if extra != 31, return 6 + extra
	if extra != 31 {
		return 6 + int(extra), nil
	}

	// Pattern: 1111 11111, read 7 more bits
	extra7, err := br.ReadBits(7)
	if err != nil {
		return 37, nil
	}
	return 37 + int(extra7), nil
}

// readDataLength reads the code block data length
// For normal mode, reads a single total length for all passes
// Returns (totalLength, nil, nil) for normal mode
func (td *TileDecoder) readDataLength(br *bitReader, cb *CodeBlockInfo, numPasses int) (int, error) {
	// Calculate number of bits for length: Lblock + log2(numPasses)
	numBits := cb.Lblock
	if numPasses > 1 {
		numBits += ilog2(numPasses)
	}

	length, err := br.ReadBits(numBits)
	if err != nil {
		return 0, err
	}

	return int(length), nil
}

// readDataLengthsERTERM reads per-pass segment lengths when ERTERM is enabled
// With ERTERM, each coding pass is independently terminated, so each pass has its own
// data segment. The length for each segment is encoded using Lblock bits.
// Per ITU-T T.800 B.10.6 and OpenJPEG t2.c opj_t2_read_packet_header.
func (td *TileDecoder) readDataLengthsERTERM(br *bitReader, cb *CodeBlockInfo, numPasses int) (int, []int, error) {
	segmentLengths := make([]int, numPasses)
	totalLength := 0

	// With ERTERM, each pass is its own segment
	// Each segment's length uses Lblock bits (since each segment has exactly 1 pass)
	for i := range numPasses {
		length, err := br.ReadBits(cb.Lblock)
		if err != nil {
			return totalLength, segmentLengths, err
		}
		segmentLengths[i] = int(length)
		totalLength += int(length)
	}

	return totalLength, segmentLengths, nil
}

// bypassSegment describes a segment in BYPASS mode.
// Each segment groups consecutive passes that share the same coding mode (MQ or RAW).
type bypassSegment struct {
	passCount int  // Number of passes in this segment
	isRaw     bool // true = raw (bypass) coding, false = MQ arithmetic coding
}

// computeBypassSegments computes the segment structure for BYPASS mode.
// Per OpenJPEG t2.c opj_t2_init_seg, BYPASS mode uses a fixed segment pattern:
//   - First segment: max 10 passes
//   - Subsequent segments alternate: max 2 passes, max 1 pass, max 2, max 1, ...
//
// The isRaw flag tracks whether each segment uses raw (bypass) or MQ coding.
// The first segment (up to 10 passes) always uses MQ.
// After that, 2-pass segments are RAW (SPP+MRP), 1-pass segments are MQ (cleanup).
//
// This function computes segments for ALL passes from the beginning (existingPasses=0)
// and is used by the decoder to determine the complete segment structure.
//
// Parameters:
//   - numPasses: total number of passes
//   - numBitPlanes: unused (kept for API compatibility)
func computeBypassSegments(numPasses, numBitPlanes int) []bypassSegment {
	if numPasses <= 0 {
		return nil
	}

	var segments []bypassSegment
	passesRemaining := numPasses
	prevMaxPasses := 0
	segIdx := 0

	for passesRemaining > 0 {
		// Determine max passes for this segment per OpenJPEG pattern
		var maxPasses int
		if segIdx == 0 {
			maxPasses = 10
		} else if prevMaxPasses == 1 || prevMaxPasses == 10 {
			maxPasses = 2
		} else {
			maxPasses = 1
		}

		actualPasses := min(maxPasses, passesRemaining)

		// First segment is MQ, then alternates: RAW(2), MQ(1), RAW(2), MQ(1)
		isRaw := segIdx > 0 && maxPasses == 2

		segments = append(segments, bypassSegment{
			passCount: actualPasses,
			isRaw:     isRaw,
		})

		passesRemaining -= actualPasses
		prevMaxPasses = maxPasses
		segIdx++
	}

	return segments
}

// computeBypassNewSegments computes the segment lengths to read from the packet header
// for new passes being added to a codeblock in BYPASS mode.
//
// This uses the persistent segment tracking state on cb to handle multi-layer scenarios
// where a segment can span multiple quality layers.
//
// Per OpenJPEG t2.c: new passes fill the current segment first, then overflow into
// new segments. Each new segment (or continuation of a partially-filled segment)
// gets its own length in the packet header.
//
// Returns the number of segment lengths to read and whether the current segment
// needs a new length (if it was already started in a previous layer and is getting
// more passes, it still needs a length entry for this layer's contribution).
func computeBypassNewSegments(cb *CodeBlockInfo, numNewPasses int) int {
	if numNewPasses <= 0 {
		return 0
	}

	// Track current segment state
	segIdx := cb.bypassSegIdx
	passesInSeg := cb.bypassPassesInSeg
	prevMaxPasses := cb.bypassPrevMaxPass

	numSegLengths := 0
	passesRemaining := numNewPasses

	for passesRemaining > 0 {
		// Determine max passes for current segment
		var maxPasses int
		if segIdx == 0 {
			maxPasses = 10
		} else if prevMaxPasses == 1 || prevMaxPasses == 10 {
			maxPasses = 2
		} else {
			maxPasses = 1
		}

		// How many passes can still fit in this segment?
		available := maxPasses - passesInSeg
		if available <= 0 {
			// Current segment is full, start a new one
			segIdx++
			passesInSeg = 0
			prevMaxPasses = maxPasses
			continue
		}

		// Assign passes to this segment
		toAssign := min(available, passesRemaining)

		// Each segment with new passes gets a length entry
		numSegLengths++

		passesInSeg += toAssign
		passesRemaining -= toAssign

		// If segment is full, advance to next
		if passesInSeg >= maxPasses {
			segIdx++
			passesInSeg = 0
			prevMaxPasses = maxPasses
		}
	}

	return numSegLengths
}

// advanceBypassSegState updates the codeblock's bypass segment tracking
// after new passes have been assigned. Must be called after reading segment lengths.
func advanceBypassSegState(cb *CodeBlockInfo, numNewPasses int) {
	passesRemaining := numNewPasses

	for passesRemaining > 0 {
		// Determine max passes for current segment
		var maxPasses int
		if cb.bypassSegIdx == 0 {
			maxPasses = 10
		} else if cb.bypassPrevMaxPass == 1 || cb.bypassPrevMaxPass == 10 {
			maxPasses = 2
		} else {
			maxPasses = 1
		}

		available := maxPasses - cb.bypassPassesInSeg
		if available <= 0 {
			cb.bypassPrevMaxPass = maxPasses
			cb.bypassSegIdx++
			cb.bypassPassesInSeg = 0
			continue
		}

		toAssign := min(available, passesRemaining)

		cb.bypassPassesInSeg += toAssign
		passesRemaining -= toAssign

		if cb.bypassPassesInSeg >= maxPasses {
			cb.bypassPrevMaxPass = maxPasses
			cb.bypassSegIdx++
			cb.bypassPassesInSeg = 0
		}
	}
}

// readDataLengthsBypass reads per-segment lengths when BYPASS mode is enabled
// (without ERTERM). Each segment with new passes in this layer gets its own length.
// Per OpenJPEG t2.c, each segment length uses Lblock + floor(log2(passesInSegment)) bits,
// where passesInSegment is the number of NEW passes being added to that segment
// in this layer.
func (td *TileDecoder) readDataLengthsBypass(br *bitReader, cb *CodeBlockInfo, numNewPasses int) (int, []int, error) {
	// Walk through the segment pattern to determine how many passes go into each segment
	// and read a length for each segment that receives new passes.
	segIdx := cb.bypassSegIdx
	passesInSeg := cb.bypassPassesInSeg
	prevMaxPasses := cb.bypassPrevMaxPass

	var segmentLengths []int
	totalLength := 0
	passesRemaining := numNewPasses

	for passesRemaining > 0 {
		// Determine max passes for current segment
		var maxPasses int
		if segIdx == 0 {
			maxPasses = 10
		} else if prevMaxPasses == 1 || prevMaxPasses == 10 {
			maxPasses = 2
		} else {
			maxPasses = 1
		}

		available := maxPasses - passesInSeg
		if available <= 0 {
			prevMaxPasses = maxPasses
			segIdx++
			passesInSeg = 0
			continue
		}

		toAssign := min(available, passesRemaining)

		// Read length for this segment's new passes
		numBits := cb.Lblock
		if toAssign > 1 {
			numBits += ilog2(toAssign)
		}

		length, err := br.ReadBits(numBits)
		if err != nil {
			return totalLength, segmentLengths, err
		}
		segmentLengths = append(segmentLengths, int(length))
		totalLength += int(length)

		passesInSeg += toAssign
		passesRemaining -= toAssign

		if passesInSeg >= maxPasses {
			prevMaxPasses = maxPasses
			segIdx++
			passesInSeg = 0
		}
	}

	return totalLength, segmentLengths, nil
}

// ilog2 returns floor(log2(n)) for data length calculation per ITU-T T.800 B.10.6
func ilog2(n int) int {
	if n <= 1 {
		return 0
	}
	result := 0
	for n > 1 {
		result++
		n >>= 1
	}
	return result
}

// decode decodes all code blocks and returns coefficients.
// For 9/7 irreversible wavelet, also returns float64 coefficients with full
// dequantization precision (no premature rounding to int32).
func (td *TileDecoder) decode() ([][][]int32, [][][]float64, error) {
	h := td.header
	tile := td.tile

	// Calculate tile dimensions (for multi-tile images)
	tileWidth := tile.X1 - tile.X0
	tileHeight := tile.Y1 - tile.Y0
	if tileWidth <= 0 {
		tileWidth = h.TileWidth
		if tileWidth == 0 || tileWidth > h.Width {
			tileWidth = h.Width
		}
	}
	if tileHeight <= 0 {
		tileHeight = h.TileHeight
		if tileHeight == 0 || tileHeight > h.Height {
			tileHeight = h.Height
		}
	}

	waveletType := td.getWaveletFilter()

	// Allocate coefficient arrays for tile dimensions
	coeffs := make([][][]int32, h.NumComps)
	var floatCoeffs [][][]float64
	if waveletType == Wavelet97 {
		floatCoeffs = make([][][]float64, h.NumComps)
	}
	// Store tile-component dimensions for each component
	tileCompWidths := make([]int, h.NumComps)
	tileCompHeights := make([]int, h.NumComps)

	for c := 0; c < h.NumComps; c++ {
		// For subsampled components, calculate tile-component dimensions
		// Per ITU-T T.800 Annex B.6:
		//   tcx0 = floor(tx0 / XRsiz), tcx1 = ceil(tx1 / XRsiz)
		//   compWidth = tcx1 - tcx0
		xrsiz := 1
		yrsiz := 1
		if c < len(h.XRsiz) && h.XRsiz[c] > 0 {
			xrsiz = h.XRsiz[c]
		}
		if c < len(h.YRsiz) && h.YRsiz[c] > 0 {
			yrsiz = h.YRsiz[c]
		}
		// Per ITU-T T.800 Annex B.6, tile-component bounds use:
		//   tcx0 = floor(tx0/XRsiz), tcy0 = floor(ty0/YRsiz)
		//   tcx1 = ceil(tx1/XRsiz),  tcy1 = ceil(ty1/YRsiz)
		tcX0 := tile.X0 / xrsiz               // floor per ITU-T T.800 Annex B.6
		tcY0 := tile.Y0 / yrsiz               // floor per ITU-T T.800 Annex B.6
		tcX1 := (tile.X1 + xrsiz - 1) / xrsiz // ceil per ITU-T T.800 Annex B.6
		tcY1 := (tile.Y1 + yrsiz - 1) / yrsiz // ceil per ITU-T T.800 Annex B.6
		// Clamp dimensions to non-negative.
		// Tiles can extend beyond image bounds, resulting in no valid pixels.
		compWidth := max(tcX1-tcX0, 0)
		compHeight := max(tcY1-tcY0, 0)

		tileCompWidths[c] = compWidth
		tileCompHeights[c] = compHeight

		coeffs[c] = make([][]int32, compHeight)
		for y := range compHeight {
			coeffs[c][y] = make([]int32, compWidth)
		}
		if waveletType == Wavelet97 {
			floatCoeffs[c] = make([][]float64, compHeight)
			for y := range compHeight {
				floatCoeffs[c][y] = make([]float64, compWidth)
			}
		}
	}

	// Create EBCOT decoder with max code block size across all components
	maxCBW, maxCBH := h.CodeBlockWidth, h.CodeBlockHeight
	for c := 0; c < h.NumComps; c++ {
		w, ht := h.ComponentCodeBlockSize(c)
		maxCBW = max(maxCBW, w)
		maxCBH = max(maxCBH, ht)
	}
	ebcot := newEBCOTDecoder(maxCBW, maxCBH)
	if ebcot == nil {
		return nil, nil, fmt.Errorf("failed to create EBCOT decoder")
	}

	// Decode each component
	for c := 0; c < h.NumComps; c++ {
		// Use per-component decomposition levels
		numCompRes := len(td.compResolutions[c])
		for r := range numCompRes {
			res := td.compResolutions[c][r]
			for _, sb := range res.Subbands {
				var fc [][]float64
				if floatCoeffs != nil {
					fc = floatCoeffs[c]
				}
				if err := td.decodeSubband(sb, coeffs[c], fc, ebcot, r, c, tileCompWidths[c], tileCompHeights[c]); err != nil {
					// Continue on error - partial decode is better than nothing
					continue
				}
			}
		}
	}

	return coeffs, floatCoeffs, nil
}

// decodeSubband decodes all code blocks in a subband.
// tileCompWidth and tileCompHeight are the tile-component dimensions (for multi-tile images).
// floatCoeffs is non-nil for 9/7 wavelet: dequantized values are stored with full float64
// precision (no rounding), for use by the DWT synthesis.
func (td *TileDecoder) decodeSubband(sb *Subband, coeffs [][]int32, floatCoeffs [][]float64, ebcot *ebcotDecoder, res int, comp int, tileCompWidth, tileCompHeight int) error {
	h := td.header

	// Code block grid parameters for coefficient placement.
	// Per ITU-T T.800 B.7, the grid is anchored at absolute (0,0).
	cbWidth, cbHeight := h.ComponentCodeBlockSize(comp)

	// Per ITU-T T.800 A.6.1, code block size must not exceed precinct size.
	// This must be consistent with createSubband.
	ppx, ppy := h.PrecinctSizeForRes(res, comp)
	if sb.Type == SubbandLL {
		cbWidth = min(cbWidth, 1<<ppx)
		cbHeight = min(cbHeight, 1<<ppy)
	} else {
		if ppx > 0 {
			cbWidth = min(cbWidth, 1<<(ppx-1))
		}
		if ppy > 0 {
			cbHeight = min(cbHeight, 1<<(ppy-1))
		}
	}

	gridX0 := sb.X0 / cbWidth
	gridY0 := sb.Y0 / cbHeight

	for y := 0; y < sb.CodeBlocksY; y++ {
		for x := 0; x < sb.CodeBlocksX; x++ {
			cb := sb.CodeBlocks[y][x]

			if cb.IncludedLayer < 0 || len(cb.Data) == 0 {
				continue
			}

			// Calculate Mb (number of magnitude bit planes) for this subband
			// Use tile-specific QCD if present, then per-component QCC, then main header QCD.
			var mbExponents []int
			guardBits := h.GuardBits
			if td.tile.HasTileQCD {
				mbExponents = td.tile.TileExponents
				guardBits = td.tile.TileGuardBits
			} else if h.CompExponents != nil && comp < len(h.CompExponents) && h.CompExponents[comp] != nil {
				mbExponents = h.CompExponents[comp]
			} else if h.OriginalExponents != nil {
				mbExponents = h.OriginalExponents
			} else {
				mbExponents = h.Exponents
			}
			mb := 8 // Default for 8-bit data without guard bits
			expIdx := 0
			if len(mbExponents) > 0 {
				if res == 0 && sb.Type == SubbandLL {
					expIdx = 0
				} else if res > 0 {
					subbandOffset := 0
					switch sb.Type {
					case SubbandHL:
						subbandOffset = 0
					case SubbandLH:
						subbandOffset = 1
					case SubbandHH:
						subbandOffset = 2
					}
					expIdx = 1 + 3*(res-1) + subbandOffset
				}
				if expIdx < len(mbExponents) {
					mb = guardBits + mbExponents[expIdx] - 1
				} else if len(mbExponents) == 1 {
					baseExp := mbExponents[0]
					var derivedExp int
					if res == 0 {
						derivedExp = baseExp
					} else {
						derivedExp = baseExp - (res - 1)
					}
					mb = guardBits + derivedExp - 1
				}
			}

			// Add ROI shift to Mb calculation
			roiShift := 0
			if comp < len(td.tile.ROIShift) {
				roiShift = td.tile.ROIShift[comp]
			}
			mb += roiShift

			if cb.TotalPasses > 0 {
				minStartBP := (cb.TotalPasses - 1 + 2) / 3 // round up
				minMb := minStartBP + 1 + cb.ZeroBitPlanes
				if minMb > mb {
					mb = minMb
				}
			}

			codeBlock := &CodeBlock{
				X:                  x,
				Y:                  y,
				Width:              cb.Width,
				Height:             cb.Height,
				Data:               cb.Data,
				NumPasses:          cb.TotalPasses,
				ZeroBitPlanes:      cb.ZeroBitPlanes,
				MagnitudeBitPlanes: mb,
				CodeBlockStyle:     td.getCodeBlockStyle(),
				SegmentLengths:     cb.SegmentLengths,
			}
			blockCoeffs, err := ebcot.DecodeCodeBlock(codeBlock, sb.Type)
			if err != nil {
				continue
			}

			// Apply ROI de-shifting to raw quantization indices BEFORE dequantization.
			// Per ITU-T T.800 Annex H, the max-shift method shifts ROI coefficient
			// indices up by SPrgn bits during encoding. We must shift them back down
			// before dequantization so the step size is applied to the correct values.
			// (roiShift was already computed above for the Mb calculation)
			if roiShift > 0 {
				threshold := int32(1 << roiShift)
				for ry := range blockCoeffs {
					for rx := range blockCoeffs[ry] {
						val := blockCoeffs[ry][rx]
						if val >= threshold {
							blockCoeffs[ry][rx] = val >> roiShift
						} else if val <= -threshold {
							mag := (-val) >> roiShift
							blockCoeffs[ry][rx] = -mag
						}
						// Values with abs(val) < threshold are background - leave unchanged
					}
				}
			}

			// Determine step size for dequantization scaling
			stepIdx := 0
			if res == 0 && sb.Type == SubbandLL {
				stepIdx = 0
			} else if res > 0 {
				subbandOffset := 0
				switch sb.Type {
				case SubbandHL:
					subbandOffset = 0
				case SubbandLH:
					subbandOffset = 1
				case SubbandHH:
					subbandOffset = 2
				}
				stepIdx = 1 + 3*(res-1) + subbandOffset
			}

			var compExponents, compMantissas []int
			if td.tile.HasTileQCD {
				// Tile-specific QCD overrides everything
				compExponents = td.tile.TileExponents
				compMantissas = td.tile.TileMantissas
			} else if h.CompQuantStyle != nil && comp < len(h.CompQuantStyle) && h.CompQuantStyle[comp] != 255 {
				if h.CompExponents != nil && comp < len(h.CompExponents) {
					compExponents = h.CompExponents[comp]
				}
				if h.CompMantissas != nil && comp < len(h.CompMantissas) {
					compMantissas = h.CompMantissas[comp]
				}
			} else {
				compExponents = h.OriginalExponents
				compMantissas = h.OriginalMantissas
				if compExponents == nil {
					compExponents = h.Exponents
				}
				if compMantissas == nil {
					compMantissas = h.Mantissas
				}
			}

			bitDepth := 8
			if comp < len(h.BitDepth) {
				bitDepth = h.BitDepth[comp]
			}

			// Rb is the nominal dynamic range of the subband.
			// Per ITU-T T.800 Table E.1: Rb = bitDepth + gain_b
			// For 9/7 wavelet: use gain=0 for ALL subbands (OpenJPEG BUG_WEIRD_TWO_INVK).
			// The DWT uses 2/K instead of 1/K for high-pass scaling to compensate.
			// For 5/3 wavelet: use standard gains (0 for LL, 1 for HL/LH, 2 for HH).
			Rb := bitDepth
			waveletType := td.getWaveletFilter()
			if waveletType == Wavelet53 {
				switch sb.Type {
				case SubbandHL, SubbandLH:
					Rb += 1
				case SubbandHH:
					Rb += 2
				}
			}
			// For Wavelet97: gain=0 for all subbands (Rb = bitDepth)

			// Formula per ITU-T T.800 E.1.1: stepSize = (1 + mant/2048) * 2^(Rb - exp)
			stepSize := 1.0
			if len(compExponents) > 0 {
				if stepIdx < len(compExponents) {
					exp := compExponents[stepIdx]
					mant := 0
					if stepIdx < len(compMantissas) {
						mant = compMantissas[stepIdx]
					}
					stepSize = (1.0 + float64(mant)/2048.0) * math.Pow(2, float64(Rb-exp))
				} else if len(compExponents) == 1 {
					baseExp := compExponents[0]
					baseMant := 0
					if len(compMantissas) > 0 {
						baseMant = compMantissas[0]
					}
					var derivedExp int
					if res == 0 {
						derivedExp = baseExp
					} else {
						derivedExp = baseExp - (res - 1)
					}
					stepSize = (1.0 + float64(baseMant)/2048.0) * math.Pow(2, float64(Rb-derivedExp))
				}
			}

			// Place coefficients in output array.
			// Per ITU-T T.800 B.7, code block (x,y) maps to absolute grid cell
			// (gridX0+x, gridY0+y). The start position within the subband is the
			// grid cell's left edge minus sbX0, clamped to 0 for the first block.
			startX := max((gridX0+x)*cbWidth-sb.X0, 0)
			startY := max((gridY0+y)*cbHeight-sb.Y0, 0)

			for cy := 0; cy < cb.Height; cy++ {
				for cx := 0; cx < cb.Width; cx++ {
					imgX, imgY := td.getCoeffPositionForTile(sb.Type, res, startX+cx, startY+cy, comp)
					if imgX >= 0 && imgX < tileCompWidth && imgY >= 0 && imgY < tileCompHeight {
						// Dequantization:
						// - Reversible 5/3 (QuantStyle=0): always divide by 2 (truncation).
						// - Irreversible 9/7 or Scaled 5/3: multiply by stepSize and round.
						quantStyle := h.QuantStyle
						if td.tile.HasTileQCD {
							quantStyle = td.tile.TileQuantStyle
						}
						if waveletType == Wavelet53 && quantStyle == 0 {
							// Reversible 5/3: integer division by 2 (truncation toward zero)
							coeffs[imgY][imgX] = blockCoeffs[cy][cx] / 2
						} else {
							// Lossy (9/7 or Scaled 5/3): floating-point dequantization.
							//
							// The EBCOT decoder outputs quantization indices that include an
							// implicit factor of 2 from the bit-plane coding process. This is
							// because the decoder accumulates magnitude bits starting from the
							// MSB, and the least significant coded bit represents 2^1, not 2^0.
							// We divide by 2 to convert to the true quantization index before
							// multiplying by step size.
							coeff := float64(blockCoeffs[cy][cx]) * stepSize / 2.0
							if floatCoeffs != nil {
								// For 9/7 wavelet: store full-precision float64 for DWT.
								// Rounding to int32 before DWT loses fractional precision
								// that accumulates into errors of 2-7 levels.
								floatCoeffs[imgY][imgX] = coeff
							}
							coeffs[imgY][imgX] = int32(math.RoundToEven(coeff))
						}
					}
				}
			}
		}
	}

	return nil
}

// getCoeffPositionForTile maps subband coordinates to coefficient array coordinates
// Uses tile-component dimensions for correct placement in multi-tile images.
// For DWT synthesis, coefficients must be in quadrant layout:
// - LL: top-left quadrant
// - HL: top-right quadrant
// - LH: bottom-left quadrant
// - HH: bottom-right quadrant
//
// CRITICAL: Per OpenJPEG t1.c lines 1721-1730, the offset for HL/LH/HH bands
// must be the PREVIOUS resolution's actual dimensions, not a calculated "half".
// This correctly handles odd dimensions where subbands are asymmetric:
// - LL/HL have width = ceil(res.Width/2), height = ceil(res.Height/2)
// - LH/HH have width = ceil(res.Width/2), height = floor(res.Height/2)
func (td *TileDecoder) getCoeffPositionForTile(sbType SubbandType, res int, x, y int, comp int) (int, int) {
	// Resolution 0 is the lowest resolution (smallest LL band)
	if res == 0 {
		// LL0 subband - just place directly
		return x, y
	}

	// For resolution > 0, we have HL, LH, HH subbands
	// These need to be placed in the correct quadrants for DWT synthesis
	//
	// The offset is the PREVIOUS resolution's dimensions (res-1).
	// This matches OpenJPEG's approach:
	//   if (band->bandno & 1) x += pres->x1 - pres->x0;
	//   if (band->bandno & 2) y += pres->y1 - pres->y0;
	prevResWidth := 0
	prevResHeight := 0
	if comp < len(td.compResolutions) && res-1 >= 0 && res-1 < len(td.compResolutions[comp]) {
		prevRes := td.compResolutions[comp][res-1]
		prevResWidth = prevRes.Width
		prevResHeight = prevRes.Height
	}

	var outX, outY int
	switch sbType {
	case SubbandLL:
		// LL goes in top-left (only at res=0, handled above)
		outX, outY = x, y
	case SubbandHL:
		// HL goes in top-right quadrant: x offset by previous resolution width
		outX, outY = prevResWidth+x, y
	case SubbandLH:
		// LH goes in bottom-left quadrant: y offset by previous resolution height
		outX, outY = x, prevResHeight+y
	case SubbandHH:
		// HH goes in bottom-right quadrant: both offsets
		outX, outY = prevResWidth+x, prevResHeight+y
	default:
		outX, outY = x, y
	}

	return outX, outY
}

// getCoeffPositionForComp maps subband coordinates to image coordinates for a specific component
// For DWT synthesis, coefficients must be in quadrant layout:
// - LL: top-left quadrant
// - HL: top-right quadrant
// - LH: bottom-left quadrant
// - HH: bottom-right quadrant
func (td *TileDecoder) getCoeffPositionForComp(sbType SubbandType, res int, comp int, x, y int) (int, int) {
	return td.getCoeffPositionForTile(sbType, res, x, y, comp)
}

// getCoeffPosition maps subband coordinates to image coordinates (uses component 0)
// For DWT synthesis, coefficients must be in quadrant layout:
// - LL: top-left quadrant
// - HL: top-right quadrant
// - LH: bottom-left quadrant
// - HH: bottom-right quadrant
func (td *TileDecoder) getCoeffPosition(sbType SubbandType, res int, x, y int) (int, int) {
	return td.getCoeffPositionForComp(sbType, res, 0, x, y)
}
