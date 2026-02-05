package jpeg2000

import "errors"

var (
	ErrInvalidMarker      = errors.New("jpeg2000: invalid marker")
	ErrUnsupportedFormat  = errors.New("jpeg2000: unsupported format")
	ErrInvalidHeader      = errors.New("jpeg2000: invalid header")
	ErrTruncatedData      = errors.New("jpeg2000: truncated data")
	ErrInvalidTile        = errors.New("jpeg2000: invalid tile")
	ErrTooManyTiles       = errors.New("jpeg2000: too many tiles")
	ErrImageTooLarge      = errors.New("jpeg2000: image dimensions exceed limit")
	ErrUnsupportedWavelet = errors.New("jpeg2000: unsupported wavelet filter")
	ErrUnsupportedQuant   = errors.New("jpeg2000: unsupported quantization")
	ErrDecodeFailed       = errors.New("jpeg2000: decode failed")
)
