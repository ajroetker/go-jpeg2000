// Copyright 2025 go-jpeg2000 Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package jpeg2000

import (
	"sync"

	"github.com/ajroetker/go-highway/hwy"
	"github.com/ajroetker/go-highway/hwy/contrib/image"
)

// slicesToImage converts a 2D slice to a SIMD-aligned Image.
// The returned image shares no data with the input; it's a copy.
func slicesToImage[T hwy.Lanes](data [][]T) *image.Image[T] {
	if len(data) == 0 || len(data[0]) == 0 {
		return nil
	}
	height := len(data)
	width := len(data[0])

	img := image.NewImage[T](width, height)
	for y := range height {
		row := img.Row(y)
		copy(row[:width], data[y])
	}
	return img
}

// imageToSlices converts a SIMD-aligned Image back to a 2D slice.
// The returned slices share no data with the image; they're copies.
func imageToSlices[T hwy.Lanes](img *image.Image[T]) [][]T {
	if img == nil || img.Width() == 0 || img.Height() == 0 {
		return nil
	}
	width := img.Width()
	height := img.Height()

	data := make([][]T, height)
	for y := range height {
		data[y] = make([]T, width)
		row := img.Row(y)
		copy(data[y], row[:width])
	}
	return data
}

// slicesToImageInPlace converts a 2D slice to a pre-allocated Image.
// This avoids allocation when the image is reused across calls.
func slicesToImageInPlace[T hwy.Lanes](data [][]T, img *image.Image[T]) {
	if len(data) == 0 || img == nil {
		return
	}
	height := len(data)
	width := len(data[0])

	for y := range height {
		row := img.Row(y)
		copy(row[:width], data[y])
	}
}

// imageToSlicesInPlace converts a SIMD-aligned Image back to a pre-allocated 2D slice.
// This avoids allocation when the slices are reused across calls.
func imageToSlicesInPlace[T hwy.Lanes](img *image.Image[T], data [][]T) {
	if img == nil || len(data) == 0 {
		return
	}
	width := img.Width()
	height := img.Height()

	for y := range height {
		row := img.Row(y)
		copy(data[y][:width], row[:width])
	}
}

// imageBufInt32 holds 6 pooled SIMD-aligned images for int32 color transforms
// (3 input + 3 output).
type imageBufInt32 struct {
	imgs [6]*image.Image[int32]
	w, h int
}

// imageBufFloat64 holds 6 pooled SIMD-aligned images for float64 color transforms.
type imageBufFloat64 struct {
	imgs [6]*image.Image[float64]
	w, h int
}

var int32ImagePool = sync.Pool{New: func() any { return new(imageBufInt32) }}
var float64ImagePool = sync.Pool{New: func() any { return new(imageBufFloat64) }}

func getInt32Buf(w, h int) *imageBufInt32 {
	buf := int32ImagePool.Get().(*imageBufInt32)
	if buf.w != w || buf.h != h {
		for i := range buf.imgs {
			buf.imgs[i] = image.NewImage[int32](w, h)
		}
		buf.w = w
		buf.h = h
	}
	return buf
}

func putInt32Buf(buf *imageBufInt32) {
	int32ImagePool.Put(buf)
}

func getFloat64Buf(w, h int) *imageBufFloat64 {
	buf := float64ImagePool.Get().(*imageBufFloat64)
	if buf.w != w || buf.h != h {
		for i := range buf.imgs {
			buf.imgs[i] = image.NewImage[float64](w, h)
		}
		buf.w = w
		buf.h = h
	}
	return buf
}

func putFloat64Buf(buf *imageBufFloat64) {
	float64ImagePool.Put(buf)
}
