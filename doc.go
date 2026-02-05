// Package jpeg2000 implements a pure Go JPEG2000 decoder.
//
// This package decodes JPEG2000 codestreams (.j2c) and JP2 files (.jp2)
// to Go image.Image values. It supports both lossless (5/3 wavelet) and
// lossy (9/7 wavelet) compression modes.
//
// Basic usage:
//
//	img, err := jpeg2000.Decode(reader)
//	if err != nil {
//	    log.Fatal(err)
//	}
//
// The package registers itself with the image package for automatic
// format detection:
//
//	import _ "github.com/ajroetker/go-jpeg2000"
//	img, _, err := image.Decode(reader)
//
// Limitations:
//   - Single-tile images optimized (multi-tile supported but less tested)
//   - No progressive decoding
//   - JP2 container parsing is minimal
package jpeg2000
