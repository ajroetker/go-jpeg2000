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
	"testing"

	hwyimage "github.com/ajroetker/go-highway/hwy/contrib/image"
)

// Scalar implementations (original code before SIMD)

func applyRCTScalar(y, cb, cr [][]int32) (r, g, b [][]int32) {
	height := len(y)
	if height == 0 {
		return nil, nil, nil
	}
	width := len(y[0])

	r = make([][]int32, height)
	g = make([][]int32, height)
	b = make([][]int32, height)

	for i := range height {
		r[i] = make([]int32, width)
		g[i] = make([]int32, width)
		b[i] = make([]int32, width)

		for j := range width {
			g[i][j] = y[i][j] - ((cb[i][j] + cr[i][j]) >> 2)
			r[i][j] = cr[i][j] + g[i][j]
			b[i][j] = cb[i][j] + g[i][j]
		}
	}

	return r, g, b
}

func forwardRCTScalar(r, g, b [][]int32) (y, cb, cr [][]int32) {
	height := len(r)
	if height == 0 {
		return nil, nil, nil
	}
	width := len(r[0])

	y = make([][]int32, height)
	cb = make([][]int32, height)
	cr = make([][]int32, height)

	for i := range height {
		y[i] = make([]int32, width)
		cb[i] = make([]int32, width)
		cr[i] = make([]int32, width)

		for j := range width {
			y[i][j] = (r[i][j] + 2*g[i][j] + b[i][j]) >> 2
			cb[i][j] = b[i][j] - g[i][j]
			cr[i][j] = r[i][j] - g[i][j]
		}
	}

	return y, cb, cr
}

func applyICTScalar(y, cb, cr [][]float64) (r, g, b [][]float64) {
	height := len(y)
	if height == 0 {
		return nil, nil, nil
	}
	width := len(y[0])

	r = make([][]float64, height)
	g = make([][]float64, height)
	b = make([][]float64, height)

	const (
		crToR = 1.402
		cbToG = 0.344136
		crToG = 0.714136
		cbToB = 1.772
	)

	for i := range height {
		r[i] = make([]float64, width)
		g[i] = make([]float64, width)
		b[i] = make([]float64, width)

		for j := range width {
			r[i][j] = y[i][j] + crToR*cr[i][j]
			g[i][j] = y[i][j] - cbToG*cb[i][j] - crToG*cr[i][j]
			b[i][j] = y[i][j] + cbToB*cb[i][j]
		}
	}

	return r, g, b
}

func forwardICTScalar(r, g, b [][]float64) (y, cb, cr [][]float64) {
	height := len(r)
	if height == 0 {
		return nil, nil, nil
	}
	width := len(r[0])

	y = make([][]float64, height)
	cb = make([][]float64, height)
	cr = make([][]float64, height)

	const (
		rToY  = 0.299
		gToY  = 0.587
		bToY  = 0.114
		rToCb = -0.16875
		gToCb = -0.33126
		bToCb = 0.5
		rToCr = 0.5
		gToCr = -0.41869
		bToCr = -0.08131
	)

	for i := range height {
		y[i] = make([]float64, width)
		cb[i] = make([]float64, width)
		cr[i] = make([]float64, width)

		for j := range width {
			rv := r[i][j]
			gv := g[i][j]
			bv := b[i][j]

			y[i][j] = rToY*rv + gToY*gv + bToY*bv
			cb[i][j] = rToCb*rv + gToCb*gv + bToCb*bv
			cr[i][j] = rToCr*rv + gToCr*gv + bToCr*bv
		}
	}

	return y, cb, cr
}

// Benchmark sizes
var benchSizes = []struct {
	name   string
	width  int
	height int
}{
	{"64x64", 64, 64},
	{"256x256", 256, 256},
	{"1080p", 1920, 1080},
	{"4K", 3840, 2160},
}

// Helper to create test data
func makeInt32Slices(width, height int) (r, g, b [][]int32) {
	r = make([][]int32, height)
	g = make([][]int32, height)
	b = make([][]int32, height)
	for y := range height {
		r[y] = make([]int32, width)
		g[y] = make([]int32, width)
		b[y] = make([]int32, width)
		for x := range width {
			r[y][x] = int32((x + y) % 256)
			g[y][x] = int32((x + y + 85) % 256)
			b[y][x] = int32((x + y + 170) % 256)
		}
	}
	return
}

func makeFloat64Slices(width, height int) (r, g, b [][]float64) {
	r = make([][]float64, height)
	g = make([][]float64, height)
	b = make([][]float64, height)
	for y := range height {
		r[y] = make([]float64, width)
		g[y] = make([]float64, width)
		b[y] = make([]float64, width)
		for x := range width {
			r[y][x] = float64((x+y)%256) / 255.0
			g[y][x] = float64((x+y+85)%256) / 255.0
			b[y][x] = float64((x+y+170)%256) / 255.0
		}
	}
	return
}

// RCT Benchmarks

func BenchmarkForwardRCT_Scalar(b *testing.B) {
	for _, size := range benchSizes {
		b.Run(size.name, func(b *testing.B) {
			r, g, bl := makeInt32Slices(size.width, size.height)
			b.ResetTimer()
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				forwardRCTScalar(r, g, bl)
			}
			b.SetBytes(int64(size.width * size.height * 4 * 3)) // 3 components, 4 bytes each
		})
	}
}

func BenchmarkForwardRCT_SIMD(b *testing.B) {
	for _, size := range benchSizes {
		b.Run(size.name, func(b *testing.B) {
			r, g, bl := makeInt32Slices(size.width, size.height)
			b.ResetTimer()
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				forwardRCT(r, g, bl)
			}
			b.SetBytes(int64(size.width * size.height * 4 * 3))
		})
	}
}

func BenchmarkInverseRCT_Scalar(b *testing.B) {
	for _, size := range benchSizes {
		b.Run(size.name, func(b *testing.B) {
			r, g, bl := makeInt32Slices(size.width, size.height)
			y, cb, cr := forwardRCTScalar(r, g, bl)
			b.ResetTimer()
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				applyRCTScalar(y, cb, cr)
			}
			b.SetBytes(int64(size.width * size.height * 4 * 3))
		})
	}
}

func BenchmarkInverseRCT_SIMD(b *testing.B) {
	for _, size := range benchSizes {
		b.Run(size.name, func(b *testing.B) {
			r, g, bl := makeInt32Slices(size.width, size.height)
			y, cb, cr := forwardRCT(r, g, bl)
			b.ResetTimer()
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				applyRCT(y, cb, cr)
			}
			b.SetBytes(int64(size.width * size.height * 4 * 3))
		})
	}
}

// ICT Benchmarks

func BenchmarkForwardICT_Scalar(b *testing.B) {
	for _, size := range benchSizes {
		b.Run(size.name, func(b *testing.B) {
			r, g, bl := makeFloat64Slices(size.width, size.height)
			b.ResetTimer()
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				forwardICTScalar(r, g, bl)
			}
			b.SetBytes(int64(size.width * size.height * 8 * 3)) // 3 components, 8 bytes each
		})
	}
}

func BenchmarkForwardICT_SIMD(b *testing.B) {
	for _, size := range benchSizes {
		b.Run(size.name, func(b *testing.B) {
			r, g, bl := makeFloat64Slices(size.width, size.height)
			b.ResetTimer()
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				forwardICT(r, g, bl)
			}
			b.SetBytes(int64(size.width * size.height * 8 * 3))
		})
	}
}

func BenchmarkInverseICT_Scalar(b *testing.B) {
	for _, size := range benchSizes {
		b.Run(size.name, func(b *testing.B) {
			r, g, bl := makeFloat64Slices(size.width, size.height)
			y, cb, cr := forwardICTScalar(r, g, bl)
			b.ResetTimer()
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				applyICTScalar(y, cb, cr)
			}
			b.SetBytes(int64(size.width * size.height * 8 * 3))
		})
	}
}

func BenchmarkInverseICT_SIMD(b *testing.B) {
	for _, size := range benchSizes {
		b.Run(size.name, func(b *testing.B) {
			r, g, bl := makeFloat64Slices(size.width, size.height)
			y, cb, cr := forwardICT(r, g, bl)
			b.ResetTimer()
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				applyICT(y, cb, cr)
			}
			b.SetBytes(int64(size.width * size.height * 8 * 3))
		})
	}
}

// Direct SIMD benchmarks (without slice conversion overhead)

func BenchmarkForwardRCT_DirectSIMD(b *testing.B) {
	for _, size := range benchSizes {
		b.Run(size.name, func(b *testing.B) {
			r := hwyimage.NewImage[int32](size.width, size.height)
			g := hwyimage.NewImage[int32](size.width, size.height)
			bl := hwyimage.NewImage[int32](size.width, size.height)
			y := hwyimage.NewImage[int32](size.width, size.height)
			cb := hwyimage.NewImage[int32](size.width, size.height)
			cr := hwyimage.NewImage[int32](size.width, size.height)

			// Fill with test data
			for row := 0; row < size.height; row++ {
				rRow := r.Row(row)
				gRow := g.Row(row)
				bRow := bl.Row(row)
				for x := 0; x < size.width; x++ {
					rRow[x] = int32((x + row) % 256)
					gRow[x] = int32((x + row + 85) % 256)
					bRow[x] = int32((x + row + 170) % 256)
				}
			}

			b.ResetTimer()
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				hwyimage.ForwardRCT(r, g, bl, y, cb, cr)
			}
			b.SetBytes(int64(size.width * size.height * 4 * 3))
		})
	}
}

func BenchmarkForwardICT_DirectSIMD(b *testing.B) {
	for _, size := range benchSizes {
		b.Run(size.name, func(b *testing.B) {
			r := hwyimage.NewImage[float64](size.width, size.height)
			g := hwyimage.NewImage[float64](size.width, size.height)
			bl := hwyimage.NewImage[float64](size.width, size.height)
			y := hwyimage.NewImage[float64](size.width, size.height)
			cb := hwyimage.NewImage[float64](size.width, size.height)
			cr := hwyimage.NewImage[float64](size.width, size.height)

			// Fill with test data
			for row := 0; row < size.height; row++ {
				rRow := r.Row(row)
				gRow := g.Row(row)
				bRow := bl.Row(row)
				for x := 0; x < size.width; x++ {
					rRow[x] = float64((x+row)%256) / 255.0
					gRow[x] = float64((x+row+85)%256) / 255.0
					bRow[x] = float64((x+row+170)%256) / 255.0
				}
			}

			b.ResetTimer()
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				hwyimage.ForwardICT(r, g, bl, y, cb, cr)
			}
			b.SetBytes(int64(size.width * size.height * 8 * 3))
		})
	}
}
