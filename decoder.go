package jpeg2000

import (
	"fmt"
	"image"
	"image/color"
	"io"
	"math"
)

// DecodeOptions controls progressive decoding behavior.
// This allows partial decoding for faster preview or reduced memory usage.
type DecodeOptions struct {
	// MaxLayers limits the number of quality layers to decode.
	// 0 means decode all layers (full quality).
	// Values 1+ decode only that many layers (lower = faster, lower quality).
	MaxLayers int

	// Reduce specifies the resolution reduction factor.
	// 0 means full resolution.
	// 1 means half resolution (skip finest decomposition level).
	// 2 means quarter resolution (skip two finest levels), etc.
	// The output image dimensions are divided by 2^Reduce.
	Reduce int
}

// Decoder holds state for decoding a JPEG2000 image
type Decoder struct {
	header  *MainHeader
	tiles   []*Tile
	jp2Meta *JP2Metadata // JP2 container metadata (nil for raw codestream)
	opts    DecodeOptions

	// Decoded tile components
	components [][][]int32 // [component][y][x]
}

// DecodeConfig returns the image configuration without full decoding
func DecodeConfig(r io.Reader) (image.Config, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return image.Config{}, err
	}

	// Check for JP2 container or raw codestream
	codestream, _ := extractCodestream(data)
	if codestream == nil {
		return image.Config{}, ErrInvalidHeader
	}

	header, _, err := parseCodestream(codestream)
	if err != nil {
		return image.Config{}, err
	}

	// Return component 0's dimensions (accounts for subsampling)
	return image.Config{
		Width:      header.ComponentWidth(0),
		Height:     header.ComponentHeight(0),
		ColorModel: colorModelFromHeader(header),
	}, nil
}

// DecodeWithUpsampling decodes a JPEG2000 image with component upsampling.
// Components with XRsiz/YRsiz > 1 are upsampled to full image dimensions.
func DecodeWithUpsampling(r io.Reader) (image.Image, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}

	codestream, jp2Meta := extractCodestream(data)
	if codestream == nil {
		return nil, ErrInvalidHeader
	}

	header, tiles, err := parseCodestream(codestream)
	if err != nil {
		return nil, fmt.Errorf("parse codestream: %w", err)
	}

	dec := &Decoder{
		header:  header,
		tiles:   tiles,
		jp2Meta: jp2Meta,
	}

	if err := dec.decodeTiles(); err != nil {
		return nil, fmt.Errorf("decode tiles: %w", err)
	}

	// Apply upsampling for subsampled components
	dec.upsampleComponents()

	return dec.toImageUpsampled(), nil
}

// Decode decodes a JPEG2000 image
func Decode(r io.Reader) (image.Image, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}

	// Extract codestream from JP2 or use directly
	codestream, jp2Meta := extractCodestream(data)
	if codestream == nil {
		return nil, ErrInvalidHeader
	}

	// Parse codestream
	header, tiles, err := parseCodestream(codestream)
	if err != nil {
		return nil, fmt.Errorf("parse codestream: %w", err)
	}

	// Distribute PPM headers to tiles (if present)
	// PPM stores packet headers for all tiles in main header; we pre-distribute
	// them to tiles as PPTHeaders so the tile decoder can use existing PPT logic
	if len(header.PPMHeaders) > 0 {
		distributePPMHeaders(header, tiles)
	}

	// Create decoder
	dec := &Decoder{
		header:  header,
		tiles:   tiles,
		jp2Meta: jp2Meta,
	}

	// Decode all tiles
	if err := dec.decodeTiles(); err != nil {
		return nil, fmt.Errorf("decode tiles: %w", err)
	}

	// Apply inverse DWT to reconstruct image
	if err := dec.reconstructImage(); err != nil {
		return nil, fmt.Errorf("reconstruct: %w", err)
	}

	// Apply JP2-level transforms (palette lookup, channel reordering)
	dec.applyJP2Transforms()

	// Convert to image.RGBA
	return dec.toImage(), nil
}

// DecodeWithOptions decodes a JPEG2000 image with progressive decoding options.
// This allows partial decoding for faster preview or reduced memory usage.
//
// Example usage:
//
//	// Decode only first 2 quality layers at half resolution
//	img, err := DecodeWithOptions(r, DecodeOptions{MaxLayers: 2, Reduce: 1})
func DecodeWithOptions(r io.Reader, opts DecodeOptions) (image.Image, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}

	// Extract codestream from JP2 or use directly
	codestream, jp2Meta := extractCodestream(data)
	if codestream == nil {
		return nil, ErrInvalidHeader
	}

	// Parse codestream
	header, tiles, err := parseCodestream(codestream)
	if err != nil {
		return nil, fmt.Errorf("parse codestream: %w", err)
	}

	// Distribute PPM headers to tiles (if present)
	if len(header.PPMHeaders) > 0 {
		distributePPMHeaders(header, tiles)
	}

	// Validate options
	if opts.Reduce < 0 {
		opts.Reduce = 0
	}
	if opts.Reduce > header.NumDecompLevels {
		opts.Reduce = header.NumDecompLevels
	}
	if opts.MaxLayers < 0 {
		opts.MaxLayers = 0
	}
	if opts.MaxLayers > header.NumLayers || opts.MaxLayers == 0 {
		opts.MaxLayers = header.NumLayers
	}

	// Create decoder with options
	dec := &Decoder{
		header:  header,
		tiles:   tiles,
		jp2Meta: jp2Meta,
		opts:    opts,
	}

	// Decode tiles with progressive options
	if err := dec.decodeTiles(); err != nil {
		return nil, fmt.Errorf("decode tiles: %w", err)
	}

	// Apply inverse DWT to reconstruct image
	if err := dec.reconstructImage(); err != nil {
		return nil, fmt.Errorf("reconstruct: %w", err)
	}

	// Apply JP2-level transforms (palette lookup, channel reordering)
	dec.applyJP2Transforms()

	// Convert to image.RGBA
	return dec.toImage(), nil
}

// extractCodestream finds the codestream in JP2 container or returns raw data.
// Also returns JP2 metadata if the input is a JP2 container.
func extractCodestream(data []byte) ([]byte, *JP2Metadata) {
	// Check for raw codestream (starts with SOC marker 0xFF4F)
	if len(data) >= 2 && data[0] == 0xFF && data[1] == 0x4F {
		return data, nil
	}

	// Check for JP2 signature box
	if len(data) < 12 {
		return nil, nil
	}

	// Parse JP2 container to get metadata and codestream
	meta, codestream, err := parseJP2Container(data)
	if err != nil {
		return nil, nil
	}

	return codestream, meta
}

// decodeTiles decodes all tiles using EBCOT
func (d *Decoder) decodeTiles() error {
	// Calculate reduced dimensions if Reduce is specified
	reduceScale := 1 << d.opts.Reduce

	// Initialize component storage at component dimensions (accounting for subsampling and reduction)
	d.components = make([][][]int32, d.header.NumComps)
	for c := 0; c < d.header.NumComps; c++ {
		compWidth := d.header.ComponentWidth(c) / reduceScale
		compHeight := d.header.ComponentHeight(c) / reduceScale
		if compWidth < 1 {
			compWidth = 1
		}
		if compHeight < 1 {
			compHeight = 1
		}
		d.components[c] = make([][]int32, compHeight)
		for y := 0; y < compHeight; y++ {
			d.components[c][y] = make([]int32, compWidth)
		}
	}

	// Decode each tile
	for _, tile := range d.tiles {
		if err := d.decodeTile(tile); err != nil {
			return fmt.Errorf("tile %d: %w", tile.Index, err)
		}
	}

	return nil
}

// decodeTile decodes a single tile using packet parsing and EBCOT
func (d *Decoder) decodeTile(tile *Tile) error {
	// Create tile decoder with progressive options
	td := newTileDecoderWithOptions(d.header, tile, d.opts.MaxLayers, d.opts.Reduce)

	if err := td.parsePackets(); err != nil {
		return fmt.Errorf("parse packets: %w", err)
	}

	// Decode code blocks to get wavelet coefficients.
	// floatCoeffs is non-nil for 9/7 wavelet and contains full-precision
	// dequantized values (not rounded to int32) for DWT synthesis.
	coeffs, floatCoeffs, err := td.decode()
	if err != nil {
		return fmt.Errorf("decode: %w", err)
	}

	// Note: ROI de-shifting is applied in decodeSubband() on raw quantization
	// indices BEFORE dequantization, per ITU-T T.800 Annex H.

	// Apply inverse DWT to each component independently
	// Each component has its own dimensions due to potential subsampling
	for c := 0; c < d.header.NumComps && c < len(coeffs); c++ {
		// Calculate tile dimensions in component coordinates
		// For subsampled components, tile bounds are scaled by XRsiz/YRsiz
		compWidth := d.header.ComponentWidth(c)
		compHeight := d.header.ComponentHeight(c)

		// Get subsampling factors for this component
		xrsiz := 1
		yrsiz := 1
		if c < len(d.header.XRsiz) && d.header.XRsiz[c] > 0 {
			xrsiz = d.header.XRsiz[c]
		}
		if c < len(d.header.YRsiz) && d.header.YRsiz[c] > 0 {
			yrsiz = d.header.YRsiz[c]
		}

		// Calculate tile-component bounds
		// Per OpenJPEG tcd.c: ceil for all tile-component bounds
		// This handles both single-tile (including non-zero XOsiz/YOsiz) and multi-tile
		var tcWidth, tcHeight int
		var tcX0, tcY0 int
		tcX0 = (tile.X0 + xrsiz - 1) / xrsiz  // ceil
		tcY0 = (tile.Y0 + yrsiz - 1) / yrsiz  // ceil
		tcX1 := (tile.X1 + xrsiz - 1) / xrsiz // ceil
		tcY1 := (tile.Y1 + yrsiz - 1) / yrsiz // ceil
		tcWidth = tcX1 - tcX0
		tcHeight = tcY1 - tcY0

		if tcWidth <= 0 || tcHeight <= 0 {
			continue
		}

		// Get actual array dimensions for composition later
		actualHeight := len(coeffs[c])
		actualWidth := 0
		if actualHeight > 0 {
			actualWidth = len(coeffs[c][0])
		}

		// Build resolution bounds array for DWT synthesis
		// Must use actual image-space resolution dimensions and origin for cas calculation
		waveletType := td.getWaveletFilter()
		compRes := td.compResolutions[c]
		resDims := make([]ResBounds, len(compRes))
		for r := range compRes {
			resDims[r] = ResBounds{
				Width:  compRes[r].Width,
				Height: compRes[r].Height,
				X0:     compRes[r].X0,
				Y0:     compRes[r].Y0,
			}
		}

		if waveletType == Wavelet53 {
			Synthesize2D_53_WithDims(coeffs[c], resDims)
		} else {
			// 9/7 wavelet: use full-precision float64 coefficients from dequantization.
			// This avoids premature rounding to int32 before DWT synthesis, which would
			// lose fractional precision and cause errors of 2-7 levels.
			var floatComp [][]float64
			if floatCoeffs != nil {
				floatComp = floatCoeffs[c]
			} else {
				// Fallback: convert int32 to float64
				floatComp = make([][]float64, len(coeffs[c]))
				for y := range coeffs[c] {
					floatComp[y] = make([]float64, len(coeffs[c][y]))
					for x := range coeffs[c][y] {
						floatComp[y][x] = float64(coeffs[c][y][x])
					}
				}
			}
			Synthesize2D_97_WithDims(floatComp, resDims)
			// Convert back with banker's rounding (round to even) to match OpenJPEG's lrintf()
			for y := range floatComp {
				for x := range floatComp[y] {
					coeffs[c][y][x] = int32(math.RoundToEven(floatComp[y][x]))
				}
			}
		}

		// Copy pixel-domain values to main decoder storage with correct tile position.
		// tcX0/tcY0 are absolute coordinates in component space (floor(tileX0/xrsiz)).
		// The output array d.components[c] is sized to ComponentWidth/ComponentHeight,
		// which are relative coordinates starting from the image origin.
		// Per ITU-T T.800 B-2: imgOriginX = ceil(XOsiz/XRsiz), imgOriginY = ceil(YOsiz/YRsiz).
		// We subtract the image origin to convert absolute to relative coordinates.
		imgOriginX := (d.header.XOsiz + xrsiz - 1) / xrsiz // ceil(XOsiz / XRsiz)
		imgOriginY := (d.header.YOsiz + yrsiz - 1) / yrsiz // ceil(YOsiz / YRsiz)
		for y := range actualHeight {
			imgY := tcY0 + y - imgOriginY
			if imgY < 0 || imgY >= compHeight || imgY >= len(d.components[c]) {
				continue
			}
			for x := 0; x < actualWidth; x++ {
				imgX := tcX0 + x - imgOriginX
				if imgX < 0 || imgX >= compWidth || imgX >= len(d.components[c][imgY]) {
					continue
				}
				d.components[c][imgY][imgX] = coeffs[c][y][x]
			}
		}
	}

	return nil
}

// reconstructImage is a no-op for standard decoding.
// Component upsampling (for XRsiz/YRsiz > 1) is NOT applied by default
// to match OpenJPEG behavior which outputs at component dimensions.
// Use DecodeWithUpsampling for full-resolution output.
func (d *Decoder) reconstructImage() error {
	return nil
}

// upsampleComponents applies nearest-neighbor upsampling to components
// with XRsiz > 1 or YRsiz > 1. This produces full-resolution output
// matching the SIZ marker dimensions.
func (d *Decoder) upsampleComponents() {
	fullWidth := d.header.Width
	fullHeight := d.header.Height

	for c := 0; c < d.header.NumComps; c++ {
		xrsiz := 1
		yrsiz := 1
		if c < len(d.header.XRsiz) {
			xrsiz = d.header.XRsiz[c]
		}
		if c < len(d.header.YRsiz) {
			yrsiz = d.header.YRsiz[c]
		}

		// Skip if no upsampling needed
		if xrsiz == 1 && yrsiz == 1 {
			continue
		}

		// Upsample this component using nearest-neighbor interpolation
		srcWidth := d.header.ComponentWidth(c)
		srcHeight := d.header.ComponentHeight(c)

		// Create upsampled component at full resolution
		upsampled := make([][]int32, fullHeight)
		for y := range fullHeight {
			upsampled[y] = make([]int32, fullWidth)
			// Map to source coordinate
			srcY := y / yrsiz
			if srcY >= srcHeight {
				srcY = srcHeight - 1
			}
			for x := range fullWidth {
				srcX := x / xrsiz
				if srcX >= srcWidth {
					srcX = srcWidth - 1
				}
				if srcY < len(d.components[c]) && srcX < len(d.components[c][srcY]) {
					upsampled[y][x] = d.components[c][srcY][srcX]
				}
			}
		}

		// Replace component with upsampled version
		d.components[c] = upsampled
	}
}

// needsColorTransform determines if YCbCr to RGB transform should be applied.
// Returns true if:
// 1. MCT flag is set in codestream, OR
// 2. JP2 container specifies YCbCr color space (even if MCT=0)
// applyJP2Transforms applies JP2 container-level transforms after codestream
// decoding: palette lookup (pclr+cmap) and channel reordering (cdef).
func (d *Decoder) applyJP2Transforms() {
	if d.jp2Meta == nil {
		return
	}

	// Apply palette lookup if pclr+cmap are present
	if d.jp2Meta.Palette != nil && len(d.jp2Meta.ComponentMap) > 0 {
		d.applyPaletteLookup()
	}

	// Apply channel reordering if cdef specifies non-identity mapping
	if d.jp2Meta.NeedsChannelReorder() {
		d.applyChannelReorder()
	}
}

// applyPaletteLookup expands single-component indexed data through the palette
// to produce multi-channel output per ITU-T T.800 I.5.3.4-5.
func (d *Decoder) applyPaletteLookup() {
	pal := d.jp2Meta.Palette
	cmap := d.jp2Meta.ComponentMap

	// Count output channels from cmap
	numOutput := len(cmap)
	if numOutput == 0 {
		return
	}

	// Build output components
	height := len(d.components[0])
	width := 0
	if height > 0 {
		width = len(d.components[0][0])
	}

	newComps := make([][][]int32, numOutput)
	for ch := range numOutput {
		newComps[ch] = make([][]int32, height)
		for y := range height {
			newComps[ch][y] = make([]int32, width)
		}
	}

	for ch, cm := range cmap {
		if cm.Component >= len(d.components) {
			continue
		}
		srcComp := d.components[cm.Component]

		if cm.MappingType == 1 && pal != nil {
			// Palette mapping: use source value as index into palette.
			// Source values are DWT-reconstructed with DC level removed.
			// For unsigned data, add back the DC offset to get the palette index.
			pcol := cm.PaletteCol
			if pcol >= pal.NumColumns {
				continue
			}
			// DC offset for the source component
			var dcOffset int32
			if cm.Component < len(d.header.BitDepth) && cm.Component < len(d.header.Signed) {
				if !d.header.Signed[cm.Component] {
					dcOffset = 1 << (d.header.BitDepth[cm.Component] - 1)
				}
			}
			// DC offset for the palette output (to pre-subtract so convertToRGBA's
			// addition of the offset results in the correct value)
			var palDCOffset int32
			if !pal.Signed[pcol] {
				palDCOffset = 1 << (pal.BitDepths[pcol] - 1)
			}
			for y := range height {
				for x := range width {
					idx := max(int(srcComp[y][x]+dcOffset), 0)
					if idx >= pal.NumEntries {
						idx = pal.NumEntries - 1
					}
					// Store palette value minus DC offset so that toImage's
					// re-addition of the offset gives the correct result
					newComps[ch][y][x] = int32(pal.Entries[idx][pcol]) - palDCOffset
				}
			}
		} else {
			// Direct mapping: copy component directly
			for y := range height {
				copy(newComps[ch][y], srcComp[y])
			}
		}
	}

	// Replace components with expanded palette output
	d.components = newComps

	// Update header to reflect new component count and bit depths
	d.header.NumComps = numOutput
	if pal != nil && len(pal.BitDepths) > 0 {
		newBitDepths := make([]int, numOutput)
		newSigned := make([]bool, numOutput)
		for ch, cm := range cmap {
			if cm.MappingType == 1 && cm.PaletteCol < pal.NumColumns {
				newBitDepths[ch] = pal.BitDepths[cm.PaletteCol]
				newSigned[ch] = pal.Signed[cm.PaletteCol]
			} else if cm.Component < len(d.header.BitDepth) {
				newBitDepths[ch] = d.header.BitDepth[cm.Component]
				newSigned[ch] = d.header.Signed[cm.Component]
			}
		}
		d.header.BitDepth = newBitDepths
		d.header.Signed = newSigned
	}
}

// applyChannelReorder reorders decoded components according to cdef associations.
// For example, if channels are stored as BGR, reorders them to RGB.
func (d *Decoder) applyChannelReorder() {
	order := d.jp2Meta.GetChannelOrder()
	if order == nil {
		return
	}

	numOut := min(len(order), len(d.components))

	newComps := make([][][]int32, numOut)
	for i := range numOut {
		srcIdx := order[i]
		if srcIdx >= 0 && srcIdx < len(d.components) {
			newComps[i] = d.components[srcIdx]
		}
	}
	d.components = newComps
}

func (d *Decoder) needsColorTransform() bool {
	// If codestream says MCT was applied, always transform
	if d.header.MCT {
		return true
	}

	// Check JP2 metadata for YCbCr color space.
	// When MCT=0 but colorspace is sYCC, the data is in YCbCr and needs
	// conversion to RGB. This uses ICT (not RCT) since the data wasn't
	// MCT-encoded but is in standard YCbCr.
	if d.jp2Meta != nil && d.jp2Meta.IsYCbCr() && d.header.NumComps >= 3 {
		return true
	}

	return false
}

// needsICTForYCbCr returns true when ICT should be used instead of RCT.
// This happens when MCT=0 but colorspace is sYCC — the data is in standard
// YCbCr (not MCT-encoded), so ICT (standard YCbCr→RGB) is correct.
func (d *Decoder) needsICTForYCbCr() bool {
	return !d.header.MCT && d.jp2Meta != nil && d.jp2Meta.IsYCbCr()
}

// toImageUpsampled converts upsampled components to image.RGBA
// Uses actual component dimensions (which are full image dimensions after upsampling)
func (d *Decoder) toImageUpsampled() *image.RGBA {
	reversible := d.header.WaveletFilter == Wavelet53

	// Use actual component dimensions after upsampling
	width := len(d.components[0][0])
	height := len(d.components[0])

	needsTransform := d.needsColorTransform() && d.header.NumComps >= 3

	// Determine which color transform to use:
	// - MCT=1 + 9/7 wavelet → ICT
	// - MCT=1 + 5/3 wavelet → RCT
	// - MCT=0 + sYCC colorspace → ICT (standard YCbCr→RGB)
	useICT := needsTransform && (!reversible || d.needsICTForYCbCr())

	if useICT {
		// Apply ICT using float components
		floatComps := make([][][]float64, d.header.NumComps)
		for c := 0; c < d.header.NumComps; c++ {
			floatComps[c] = make([][]float64, len(d.components[c]))
			for y := range d.components[c] {
				floatComps[c][y] = make([]float64, len(d.components[c][y]))
				for x := range d.components[c][y] {
					floatComps[c][y][x] = float64(d.components[c][y][x])
				}
			}
		}
		return convertToRGBAFloat(floatComps, width, height,
			d.header.BitDepth, d.header.Signed)
	}

	return convertToRGBA(d.components, width, height,
		d.header.BitDepth, d.header.Signed, reversible && needsTransform)
}

// toImage converts decoded components to image.RGBA
func (d *Decoder) toImage() *image.RGBA {
	reversible := d.header.WaveletFilter == Wavelet53

	// Use actual component dimensions from decoded data
	// This accounts for Reduce option (resolution reduction)
	// For grayscale, this is the output size
	// For RGB with color transform, all components should have same dimensions
	height := len(d.components[0])
	width := 0
	if height > 0 {
		width = len(d.components[0][0])
	}

	needsTransform := d.needsColorTransform() && d.header.NumComps >= 3

	// Check if components have different dimensions (subsampling)
	hasSubsampling := false
	if d.header.NumComps >= 3 {
		comp0Width := d.header.ComponentWidth(0)
		comp0Height := d.header.ComponentHeight(0)
		for c := 1; c < d.header.NumComps && c < 3; c++ {
			if d.header.ComponentWidth(c) != comp0Width ||
				d.header.ComponentHeight(c) != comp0Height {
				hasSubsampling = true
				break
			}
		}
	}

	// For subsampled images with color transform, we need to upsample first
	// because RCT/ICT requires all components to have same dimensions
	if hasSubsampling && needsTransform {
		d.upsampleComponents()
		// After upsampling, update dimensions from component 0
		height = len(d.components[0])
		if height > 0 {
			width = len(d.components[0][0])
		}
	}

	// Determine which color transform to use:
	// - MCT=1 + 9/7 wavelet → ICT (irreversible color transform)
	// - MCT=1 + 5/3 wavelet → RCT (reversible color transform)
	// - MCT=0 + sYCC colorspace → ICT (standard YCbCr→RGB, not MCT-encoded)
	useICT := needsTransform && (!reversible || d.needsICTForYCbCr())

	if useICT {
		// Apply ICT using float components
		floatComps := make([][][]float64, d.header.NumComps)
		for c := 0; c < d.header.NumComps; c++ {
			floatComps[c] = make([][]float64, len(d.components[c]))
			for y := range d.components[c] {
				floatComps[c][y] = make([]float64, len(d.components[c][y]))
				for x := range d.components[c][y] {
					floatComps[c][y][x] = float64(d.components[c][y][x])
				}
			}
		}
		return convertToRGBAFloat(floatComps, width, height,
			d.header.BitDepth, d.header.Signed)
	}

	return convertToRGBA(d.components, width, height,
		d.header.BitDepth, d.header.Signed, reversible && needsTransform)
}

// colorModelFromHeader returns the color model for the image
func colorModelFromHeader(h *MainHeader) color.Model {
	if h.NumComps == 1 {
		return color.GrayModel
	}
	return color.RGBAModel
}

// Register format with image package
func init() {
	// JP2 file format (starts with JP2 signature box)
	image.RegisterFormat("jp2",
		"\x00\x00\x00\x0cjP  \x0d\x0a\x87\x0a",
		Decode, DecodeConfig)

	// Raw codestream (starts with SOC marker)
	image.RegisterFormat("j2c", "\xff\x4f\xff\x51",
		Decode, DecodeConfig)
}
