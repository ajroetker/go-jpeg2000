package jpeg2000

import (
	"bytes"
	"image"
	"image/color"
	"io"
	"math"
)

// FileFormat specifies the output file format.
type FileFormat int

const (
	FormatJ2K FileFormat = iota // Raw codestream (.j2k/.j2c)
	FormatJP2                   // JP2 file format with boxes (.jp2)
)

// EncodeOptions controls JPEG2000 encoding parameters.
type EncodeOptions struct {
	// Lossless selects the compression mode:
	//   true  = 5/3 reversible wavelet + RCT (exact reconstruction)
	//   false = 9/7 irreversible wavelet + ICT (lossy)
	Lossless bool

	// Quality controls the compression ratio for lossy mode (0.0 to 1.0).
	// 1.0 = highest quality (lowest compression).
	// 0.5 = moderate quality.
	// 0.0 = lowest quality (highest compression).
	// Ignored when Lossless is true.
	Quality float64

	// TargetSize sets an explicit target file size in bytes.
	// If > 0, overrides Quality. 0 means use Quality instead.
	TargetSize int

	// NumLayers sets the number of quality layers (default: 1).
	NumLayers int

	// TileWidth and TileHeight set tile dimensions.
	// 0 means single tile (entire image).
	TileWidth  int
	TileHeight int

	// NumResolutions = NumDecompLevels + 1 (default: 6, meaning 5 decomposition levels).
	NumResolutions int

	// CodeBlockWidth and CodeBlockHeight (default: 64x64).
	// Must be powers of 2, max 64.
	CodeBlockWidth  int
	CodeBlockHeight int

	// Progression order (default: LRCP = 0).
	Progression byte

	// FileFormat selects J2K (raw codestream) or JP2 (with file format boxes).
	FileFormat FileFormat
}

// Encode encodes img as a JPEG2000 image and writes it to w.
func Encode(w io.Writer, img image.Image, opts *EncodeOptions) error {
	if opts == nil {
		opts = &EncodeOptions{}
	}
	e := newEncoder(img, opts)
	return e.encode(w)
}

type encoder struct {
	img  image.Image
	opts EncodeOptions

	width, height int
	numComps      int
	bitDepth      int
	header        *MainHeader
}

func newEncoder(img image.Image, opts *EncodeOptions) *encoder {
	bounds := img.Bounds()
	e := &encoder{
		img:    img,
		opts:   *opts,
		width:  bounds.Dx(),
		height: bounds.Dy(),
	}

	// Set defaults
	if e.opts.Quality <= 0 && !e.opts.Lossless {
		e.opts.Quality = 0.8
	}
	if e.opts.NumLayers <= 0 {
		e.opts.NumLayers = 1
	}
	if e.opts.NumResolutions <= 0 {
		e.opts.NumResolutions = 6
	}
	if e.opts.NumResolutions > 32 {
		e.opts.NumResolutions = 32
	}
	if e.opts.CodeBlockWidth <= 0 {
		e.opts.CodeBlockWidth = 64
	}
	if e.opts.CodeBlockHeight <= 0 {
		e.opts.CodeBlockHeight = 64
	}

	// Determine number of components from image type
	switch img.ColorModel() {
	case color.GrayModel, color.Gray16Model:
		e.numComps = 1
	default:
		e.numComps = 3
	}
	e.bitDepth = 8

	// Limit decomposition levels to image dimensions
	numLevels := e.opts.NumResolutions - 1
	minDim := min(e.height, e.width)
	maxLevels := 0
	for d := minDim; d > 1; d = (d + 1) / 2 {
		maxLevels++
	}
	if numLevels > maxLevels {
		numLevels = maxLevels
		e.opts.NumResolutions = numLevels + 1
	}

	return e
}

func (e *encoder) buildHeader() *MainHeader {
	numLevels := e.opts.NumResolutions - 1

	tileW := e.width
	tileH := e.height
	if e.opts.TileWidth > 0 {
		tileW = e.opts.TileWidth
	}
	if e.opts.TileHeight > 0 {
		tileH = e.opts.TileHeight
	}

	wavelet := Wavelet97
	if e.opts.Lossless {
		wavelet = Wavelet53
	}

	bitDepths := make([]int, e.numComps)
	signed := make([]bool, e.numComps)
	xrsiz := make([]int, e.numComps)
	yrsiz := make([]int, e.numComps)
	for i := 0; i < e.numComps; i++ {
		bitDepths[i] = e.bitDepth
		xrsiz[i] = 1
		yrsiz[i] = 1
	}

	numXTiles := (e.width + tileW - 1) / tileW
	numYTiles := (e.height + tileH - 1) / tileH

	// Quantization parameters
	quantStyle := byte(0) // No quantization for reversible
	numSubbands := 3*numLevels + 1
	exponents := make([]int, numSubbands)
	mantissas := make([]int, numSubbands)

	if e.opts.Lossless {
		// Reversible: exponents only, no mantissa
		// Exponent = guard bits + bit depth + gain
		guardBits := 1
		for i := range numSubbands {
			gain := subbandGain53(i, numLevels)
			exponents[i] = guardBits + e.bitDepth + gain
		}
	} else {
		quantStyle = 2 // Scalar expounded
		stepSizes := defaultStepSizes(e.opts.Quality, numLevels, e.bitDepth)
		for i := range numSubbands {
			exponents[i], mantissas[i] = computeExpMantissa(stepSizes[i], e.bitDepth)
		}
	}

	h := &MainHeader{
		Width:            e.width,
		Height:           e.height,
		TileWidth:        tileW,
		TileHeight:       tileH,
		NumComps:         e.numComps,
		BitDepth:         bitDepths,
		Signed:           signed,
		XRsiz:            xrsiz,
		YRsiz:            yrsiz,
		ProgressionOrder: e.opts.Progression,
		NumLayers:        e.opts.NumLayers,
		MCT:              e.numComps >= 3,
		NumDecompLevels:  numLevels,
		CodeBlockWidth:   e.opts.CodeBlockWidth,
		CodeBlockHeight:  e.opts.CodeBlockHeight,
		WaveletFilter:    wavelet,
		QuantStyle:       quantStyle,
		GuardBits:        1,
		Exponents:        exponents,
		Mantissas:        mantissas,
		NumXTiles:        numXTiles,
		NumYTiles:        numYTiles,
		NumTiles:         numXTiles * numYTiles,
	}

	e.header = h
	return h
}

func (e *encoder) encode(w io.Writer) error {
	header := e.buildHeader()
	numLevels := header.NumDecompLevels

	// Extract components from image
	components := e.extractComponents()

	// Apply forward color transform
	if header.MCT && e.numComps >= 3 {
		components = e.applyForwardColorTransform(components)
	}

	// Process each tile
	var allTileData [][]byte
	for tileY := 0; tileY < header.NumYTiles; tileY++ {
		for tileX := 0; tileX < header.NumXTiles; tileX++ {
			tileData := e.encodeTile(components, tileX, tileY, numLevels)
			allTileData = append(allTileData, tileData)
		}
	}

	// Write output
	if e.opts.FileFormat == FormatJP2 {
		return e.writeJP2(w, header, allTileData)
	}
	return e.writeJ2K(w, header, allTileData)
}

func (e *encoder) extractComponents() [][][]int32 {
	bounds := e.img.Bounds()
	comps := make([][][]int32, e.numComps)
	for c := 0; c < e.numComps; c++ {
		comps[c] = make([][]int32, e.height)
		for y := 0; y < e.height; y++ {
			comps[c][y] = make([]int32, e.width)
		}
	}

	for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
		for x := bounds.Min.X; x < bounds.Max.X; x++ {
			px := e.img.At(x, y)
			iy := y - bounds.Min.Y
			ix := x - bounds.Min.X

			if e.numComps == 1 {
				gray, _, _, _ := color.GrayModel.Convert(px).(color.Gray).RGBA()
				comps[0][iy][ix] = int32(gray >> 8)
			} else {
				r, g, b, _ := px.RGBA()
				comps[0][iy][ix] = int32(r >> 8)
				comps[1][iy][ix] = int32(g >> 8)
				comps[2][iy][ix] = int32(b >> 8)
			}
		}
	}

	// Apply DC level shift: subtract 2^(bitDepth-1)
	offset := int32(1 << (e.bitDepth - 1))
	for c := 0; c < e.numComps; c++ {
		for y := 0; y < e.height; y++ {
			for x := 0; x < e.width; x++ {
				comps[c][y][x] -= offset
			}
		}
	}

	return comps
}

func (e *encoder) applyForwardColorTransform(comps [][][]int32) [][][]int32 {
	if e.opts.Lossless {
		// RCT (reversible)
		y, cb, cr := forwardRCT(comps[0], comps[1], comps[2])
		return [][][]int32{y, cb, cr}
	}
	// ICT (irreversible) - convert int32 to float64, transform, convert back
	height := len(comps[0])
	width := len(comps[0][0])

	rf := make([][]float64, height)
	gf := make([][]float64, height)
	bf := make([][]float64, height)
	for i := range height {
		rf[i] = make([]float64, width)
		gf[i] = make([]float64, width)
		bf[i] = make([]float64, width)
		for j := range width {
			rf[i][j] = float64(comps[0][i][j])
			gf[i][j] = float64(comps[1][i][j])
			bf[i][j] = float64(comps[2][i][j])
		}
	}

	yf, cbf, crf := forwardICT(rf, gf, bf)

	// Convert back to int32 via quantization (done later), but for now store as int32
	result := make([][][]int32, 3)
	for c := range 3 {
		result[c] = make([][]int32, height)
		var src [][]float64
		switch c {
		case 0:
			src = yf
		case 1:
			src = cbf
		case 2:
			src = crf
		}
		for i := range height {
			result[c][i] = make([]int32, width)
			for j := range width {
				result[c][i][j] = int32(math.Round(src[i][j]))
			}
		}
	}
	return result
}

func (e *encoder) encodeTile(components [][][]int32, tileX, tileY, numLevels int) []byte {
	header := e.header

	// Compute tile bounds
	tx0 := tileX * header.TileWidth
	ty0 := tileY * header.TileHeight
	tx1 := tx0 + header.TileWidth
	ty1 := ty0 + header.TileHeight
	if tx1 > e.width {
		tx1 = e.width
	}
	if ty1 > e.height {
		ty1 = e.height
	}
	tw := tx1 - tx0
	th := ty1 - ty0

	// Extract tile region from each component
	tileComps := make([][][]int32, e.numComps)
	for c := 0; c < e.numComps; c++ {
		tileComps[c] = make([][]int32, th)
		for y := range th {
			tileComps[c][y] = make([]int32, tw)
			copy(tileComps[c][y], components[c][ty0+y][tx0:tx1])
		}
	}

	// Forward DWT
	if e.opts.Lossless {
		for c := 0; c < e.numComps; c++ {
			Analyze2D_53(tileComps[c], tw, th, numLevels)
		}
	} else {
		// For lossy, work in float64 for DWT then quantize
		for c := 0; c < e.numComps; c++ {
			fcoeffs := make([][]float64, th)
			for y := range th {
				fcoeffs[y] = make([]float64, tw)
				for x := range tw {
					fcoeffs[y][x] = float64(tileComps[c][y][x])
				}
			}
			Analyze2D_97(fcoeffs, tw, th, numLevels)

			// Quantize subbands
			// For simplicity, apply a single step size per subband
			stepSizes := defaultStepSizes(e.opts.Quality, numLevels, e.bitDepth)
			tileComps[c] = e.quantizeSubbands(fcoeffs, tw, th, numLevels, stepSizes)
		}
	}

	// EBCOT encode all code blocks, organized by component/resolution/subband
	cbw := header.CodeBlockWidth
	cbh := header.CodeBlockHeight
	numResolutions := numLevels + 1

	// Build resolution structures per component:
	// resolutions[component][resolution] -> EncoderResolution
	resolutions := make([][]*EncoderResolution, e.numComps)
	var allBlocks []*EncodedBlock

	for c := 0; c < e.numComps; c++ {
		coeffs := tileComps[c]
		resolutions[c] = make([]*EncoderResolution, numResolutions)

		for r := range numResolutions {
			res := &EncoderResolution{Level: r}

			// Determine which subbands belong to this resolution.
			// Resolution 0 = LL subband (index 0).
			// Resolution r (r>0) = 3 detail subbands from decomposition level (numLevels - r + 1).
			var sbIndices []int
			if r == 0 {
				sbIndices = []int{0} // LL
			} else {
				// Detail subbands for decomposition level corresponding to resolution r.
				// Per ITU-T T.800, resolution r (1-indexed) corresponds to decomposition
				// level (numLevels - r + 1), but in the subband index ordering used by
				// subbandBounds, the coarsest detail (level numLevels) is at indices 1..3,
				// and each finer level adds 3 more indices. Resolution 1 maps to the
				// coarsest detail, resolution numLevels maps to the finest.
				// base = 1 + 3*(r-1)
				// subbandBounds maps: base+0=LH, base+1=HL, base+2=HH
				// Decoder expects: [HL, LH, HH] order per ITU-T T.800
				base := 1 + 3*(r-1)
				sbIndices = []int{base + 1, base, base + 2} // HL, LH, HH
			}

			for _, sbIdx := range sbIndices {
				if sbIdx >= 3*numLevels+1 {
					continue
				}
				sbType, sbX0, sbY0, sbW, sbH := subbandBounds(sbIdx, numLevels, tw, th)
				if sbW <= 0 || sbH <= 0 {
					continue
				}

				codeBlocksX := (sbW + cbw - 1) / cbw
				codeBlocksY := (sbH + cbh - 1) / cbh
				encSb := NewEncoderSubband(sbType, sbW, sbH, codeBlocksX, codeBlocksY)

				// Guard bits + exponent for magnitude bit count
				mb := header.GuardBits + header.Exponents[sbIdx] - 1

				// Encode each code block
				for cby := range codeBlocksY {
					for cbx := range codeBlocksX {
						bx0 := cbx * cbw
						by0 := cby * cbh
						bw := min(cbw, sbW-bx0)
						bh := min(cbh, sbH-by0)

						cbCoeffs := make([][]int32, bh)
						for y := range bh {
							cbCoeffs[y] = make([]int32, bw)
							for x := range bw {
								cbCoeffs[y][x] = coeffs[sbY0+by0+y][sbX0+bx0+x]
							}
						}

						enc := newEBCOTEncoder(bw, bh)
						block := enc.EncodeCodeBlock(cbCoeffs, sbType, mb)
						allBlocks = append(allBlocks, block)

						ecb := NewEncoderCodeBlock(block, cbx, cby)
						encSb.CodeBlocks[cby][cbx] = ecb
					}
				}

				res.Subbands = append(res.Subbands, encSb)
			}

			resolutions[c][r] = res
		}
	}

	// Rate control
	rc := NewRateController(allBlocks)
	var passesPerBlock []int

	if e.opts.TargetSize > 0 {
		passesPerBlock = rc.OptimizeSingleLayer(e.opts.TargetSize)
	} else if !e.opts.Lossless {
		totalPixels := tw * th * e.numComps
		bpp := 0.5 + 7.5*e.opts.Quality
		targetBytes := int(float64(totalPixels) * bpp / 8.0)
		passesPerBlock = rc.OptimizeSingleLayer(targetBytes)
	} else {
		// Lossless: include all passes
		passesPerBlock = make([]int, len(allBlocks))
		for i, b := range allBlocks {
			passesPerBlock[i] = b.NumPasses
		}
	}

	// Distribute passes to layer 0 (single-layer encoding).
	// Also set FirstLayer on each code block and initialize tag trees.
	blockIdx := 0
	for c := 0; c < e.numComps; c++ {
		for r := range numResolutions {
			res := resolutions[c][r]
			if res == nil {
				continue
			}
			for _, sb := range res.Subbands {
				for cby := 0; cby < sb.CodeBlocksY; cby++ {
					for cbx := 0; cbx < sb.CodeBlocksX; cbx++ {
						ecb := sb.CodeBlocks[cby][cbx]
						if ecb != nil && blockIdx < len(passesPerBlock) && passesPerBlock[blockIdx] > 0 {
							ecb.FirstLayer = 0
						}
						blockIdx++
					}
				}
				InitSubbandTagTrees(sb)
			}
		}
	}

	// Build layerPasses structure for EncodePacketsLRCP:
	// layerPasses[layer][component][resolution] -> map[blockIdx]numNewPasses
	numLayers := max(header.NumLayers, 1)
	layerPasses := make([][][]map[int]int, numLayers)
	for l := range layerPasses {
		layerPasses[l] = make([][]map[int]int, e.numComps)
		for c := range layerPasses[l] {
			layerPasses[l][c] = make([]map[int]int, numResolutions)
			for r := range layerPasses[l][c] {
				layerPasses[l][c][r] = make(map[int]int)
			}
		}
	}

	// All passes go into layer 0 for single-layer.
	// The block index in layerPasses must be globally unique within a resolution
	// to avoid collisions between subbands. We use a subband offset so that
	// code blocks at the same local position in different subbands get distinct keys.
	// This offset must match the one used by EncodePacket/encodeSubbandPacketHeader.
	blockIdx = 0
	for c := 0; c < e.numComps; c++ {
		for r := range numResolutions {
			res := resolutions[c][r]
			if res == nil {
				continue
			}
			subbandOffset := 0
			for _, sb := range res.Subbands {
				for cby := 0; cby < sb.CodeBlocksY; cby++ {
					for cbx := 0; cbx < sb.CodeBlocksX; cbx++ {
						globalBlockIdx := subbandOffset + cby*sb.CodeBlocksX + cbx
						if blockIdx < len(passesPerBlock) && passesPerBlock[blockIdx] > 0 {
							layerPasses[0][c][r][globalBlockIdx] = passesPerBlock[blockIdx]
						}
						blockIdx++
					}
				}
				subbandOffset += sb.CodeBlocksX * sb.CodeBlocksY
			}
		}
	}

	// Use packet encoder to assemble proper JPEG2000 packets
	pe := NewPacketEncoder(header)
	return pe.EncodePacketsLRCP(resolutions, layerPasses)
}

// quantizeSubbands applies dead-zone quantization to each subband of a DWT-transformed tile.
func (e *encoder) quantizeSubbands(coeffs [][]float64, width, height, numLevels int, stepSizes []float64) [][]int32 {
	result := make([][]int32, height)
	for y := range height {
		result[y] = make([]int32, width)
	}

	for sbIdx := 0; sbIdx < 3*numLevels+1; sbIdx++ {
		_, sbX0, sbY0, sbW, sbH := subbandBounds(sbIdx, numLevels, width, height)
		if sbW <= 0 || sbH <= 0 {
			continue
		}

		step := stepSizes[sbIdx]
		if step <= 0 {
			step = 1.0
		}

		for y := range sbH {
			for x := range sbW {
				val := coeffs[sbY0+y][sbX0+x]
				if val >= 0 {
					result[sbY0+y][sbX0+x] = int32(math.Floor(val / step))
				} else {
					result[sbY0+y][sbX0+x] = -int32(math.Floor(-val / step))
				}
			}
		}
	}

	return result
}

// subbandBounds returns the type and pixel bounds (x0, y0, width, height) for a subband index.
// Subband order: [0]=LL_N, [1]=LH_N, [2]=HL_N, [3]=HH_N, [4]=LH_{N-1}, ...
func subbandBounds(sbIdx, numLevels, tileW, tileH int) (SubbandType, int, int, int, int) {
	if sbIdx == 0 {
		// LL at coarsest level
		llW := tileW
		llH := tileH
		for range numLevels {
			llW = (llW + 1) / 2
			llH = (llH + 1) / 2
		}
		return SubbandLL, 0, 0, llW, llH
	}

	// Detail subbands: index 1,2,3 = level N (coarsest), 4,5,6 = level N-1, ...
	detailIdx := sbIdx - 1
	levelFromCoarsest := detailIdx / 3
	orient := detailIdx % 3 // 0=LH, 1=HL, 2=HH

	level := numLevels - levelFromCoarsest // 1-indexed from finest

	// Compute LL dimensions at this level
	llW := tileW
	llH := tileH
	for range level {
		llW = (llW + 1) / 2
		llH = (llH + 1) / 2
	}

	// Parent LL dimensions (one level coarser)
	parentW := tileW
	parentH := tileH
	for i := 0; i < level-1; i++ {
		parentW = (parentW + 1) / 2
		parentH = (parentH + 1) / 2
	}

	hlW := parentW - llW
	lhH := parentH - llH

	switch orient {
	case 0: // LH: bottom-left of parent
		return SubbandLH, 0, llH, llW, lhH
	case 1: // HL: top-right of parent
		return SubbandHL, llW, 0, hlW, llH
	case 2: // HH: bottom-right of parent
		return SubbandHH, llW, llH, hlW, lhH
	}

	return SubbandLL, 0, 0, 0, 0
}

// subbandGain53 returns the subband gain for the 5/3 reversible wavelet.
// Per ITU-T T.800 Table E.1:
//
//	LL: 0, LH: 1, HL: 1, HH: 2
func subbandGain53(sbIdx, numLevels int) int {
	if sbIdx == 0 {
		return 0 // LL
	}
	orient := (sbIdx - 1) % 3
	switch orient {
	case 0, 1: // LH or HL
		return 1
	case 2: // HH
		return 2
	}
	return 0
}

func (e *encoder) writeJ2K(w io.Writer, header *MainHeader, allTileData [][]byte) error {
	cw := NewCodestreamWriter(w, header)

	if err := cw.WriteMainHeader(); err != nil {
		return err
	}

	for i, td := range allTileData {
		if err := cw.WriteTilePart(i, td); err != nil {
			return err
		}
	}

	return cw.WriteEOC()
}

func (e *encoder) writeJP2(w io.Writer, header *MainHeader, allTileData [][]byte) error {
	// First build the complete codestream
	var csBuf bytes.Buffer
	if err := e.writeJ2K(&csBuf, header, allTileData); err != nil {
		return err
	}

	return WriteJP2File(w, header, csBuf.Bytes())
}
