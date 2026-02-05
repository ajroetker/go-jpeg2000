// Package jpeg2000 implements a pure Go JPEG2000 codec.
//
// This package encodes and decodes JPEG2000 codestreams (.j2k/.j2c) and
// JP2 files (.jp2). It supports both lossless (5/3 reversible wavelet) and
// lossy (9/7 irreversible wavelet) compression modes.
//
// Decoding:
//
//	img, err := jpeg2000.Decode(reader)
//	if err != nil {
//	    log.Fatal(err)
//	}
//
// Encoding:
//
//	err := jpeg2000.Encode(writer, img, &jpeg2000.EncodeOptions{Lossless: true})
//	if err != nil {
//	    log.Fatal(err)
//	}
//
// The package registers itself with the image package for automatic
// format detection:
//
//	import _ "github.com/ajroetker/go-jpeg2000"
//	img, _, err := image.Decode(reader)
package jpeg2000
