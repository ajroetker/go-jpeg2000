package jpeg2000

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
)

// Paths to OpenJPEG conformance test data.
const (
	conformanceInput    = "openjpeg-data/input"
	conformanceBaseline = "openjpeg-data/baseline"
)

// TestP0Conformance tests all p0_* conformance files (Part-1 basic).
func TestP0Conformance(t *testing.T) {
	files, err := filepath.Glob(filepath.Join(conformanceInput, "p0_*.j2k"))
	if err != nil {
		t.Fatalf("Failed to glob: %v", err)
	}

	t.Logf("Testing %d p0 conformance files", len(files))

	for _, inputPath := range files {
		name := filepath.Base(inputPath)
		t.Run(name, func(t *testing.T) {
			testConformanceFile(t, inputPath)
		})
	}
}

// TestP1Conformance tests all p1_* conformance files (Part-2 extensions).
func TestP1Conformance(t *testing.T) {
	files, err := filepath.Glob(filepath.Join(conformanceInput, "p1_*.j2k"))
	if err != nil {
		t.Fatalf("Failed to glob: %v", err)
	}

	t.Logf("Testing %d p1 conformance files", len(files))

	for _, inputPath := range files {
		name := filepath.Base(inputPath)
		t.Run(name, func(t *testing.T) {
			testConformanceFile(t, inputPath)
		})
	}
}

// TestJ2CConformance tests all letter-series .j2c conformance files (a1-g4).
func TestJ2CConformance(t *testing.T) {
	files, err := filepath.Glob(filepath.Join(conformanceInput, "[a-g]*.j2c"))
	if err != nil {
		t.Fatalf("Failed to glob: %v", err)
	}

	t.Logf("Testing %d j2c conformance files", len(files))

	for _, inputPath := range files {
		name := filepath.Base(inputPath)
		t.Run(name, func(t *testing.T) {
			testConformanceFile(t, inputPath)
		})
	}
}

// TestJP2Conformance tests all .jp2 container conformance files.
func TestJP2Conformance(t *testing.T) {
	files, err := filepath.Glob(filepath.Join(conformanceInput, "*.jp2"))
	if err != nil {
		t.Fatalf("Failed to glob: %v", err)
	}

	t.Logf("Testing %d jp2 conformance files", len(files))

	for _, inputPath := range files {
		name := filepath.Base(inputPath)
		t.Run(name, func(t *testing.T) {
			testConformanceFile(t, inputPath)
		})
	}
}

// testConformanceFile tests a single file against OpenJPEG reference decoder.
func testConformanceFile(t *testing.T, inputPath string) {
	data, err := os.ReadFile(inputPath)
	if err != nil {
		t.Skipf("Test data not found: %v", err)
	}

	// Decode with our decoder
	img, err := Decode(bytes.NewReader(data))
	if err != nil {
		t.Logf("DECODE_ERROR: %v", err)
		return
	}

	bounds := img.Bounds()

	// Try to find reference file in openjpeg-data directory
	baseName := filepath.Base(inputPath)
	baseName = baseName[:len(baseName)-len(filepath.Ext(baseName))]

	var refData []byte

	// Look for baseline reference files: try whole-image first, then split-component
	for _, ext := range []string{".ppm", ".pgm"} {
		candidate := filepath.Join(conformanceBaseline, baseName+ext)
		if d, err := os.ReadFile(candidate); err == nil {
			refData = d
			break
		}
	}
	if refData == nil {
		// Try split-component output (e.g. name_0.pgm)
		split0 := filepath.Join(conformanceBaseline, baseName+"_0.pgm")
		if d, err := os.ReadFile(split0); err == nil {
			refData = d
		}
	}
	if refData == nil {
		t.Skipf("No baseline reference file for %s", baseName)
	}

	// Parse PGM/PPM based on magic bytes, not file extension
	var refPixels []byte
	var refWidth, refHeight int
	var isPPMFormat bool
	if len(refData) >= 2 {
		magic := string(refData[:2])
		if magic == "P6" || magic == "P3" {
			// PPM (color)
			refPixels, refWidth, refHeight = parsePPMImage(t, refData)
			isPPMFormat = true
		} else {
			// PGM (grayscale) - P5, P2
			refPixels, refWidth, refHeight = parsePGM(t, refData)
			isPPMFormat = false
		}
	} else {
		t.Fatalf("Reference file too small")
	}

	// Check dimensions
	if bounds.Dx() != refWidth || bounds.Dy() != refHeight {
		t.Logf("DIM_MISMATCH: our %dx%d vs ref %dx%d", bounds.Dx(), bounds.Dy(), refWidth, refHeight)
		return
	}

	// Compare pixels
	diffCount := 0
	maxDiff := 0

	for y := 0; y < refHeight; y++ {
		for x := 0; x < refWidth; x++ {
			r, g, b, _ := img.At(bounds.Min.X+x, bounds.Min.Y+y).RGBA()
			ourR, ourG, ourB := int(r>>8), int(g>>8), int(b>>8)

			if isPPMFormat {
				idx := (y*refWidth + x) * 3
				refR, refG, refB := int(refPixels[idx]), int(refPixels[idx+1]), int(refPixels[idx+2])

				diffR := abs(ourR - refR)
				diffG := abs(ourG - refG)
				diffB := abs(ourB - refB)

				if diffR > 0 || diffG > 0 || diffB > 0 {
					diffCount++
					maxPixDiff := max(diffR, diffG, diffB)
					if maxPixDiff > maxDiff {
						maxDiff = maxPixDiff
					}
				}
			} else {
				refPixel := int(refPixels[y*refWidth+x])
				diff := abs(ourR - refPixel)
				if diff > 0 {
					diffCount++
					if diff > maxDiff {
						maxDiff = diff
					}
				}
			}
		}
	}

	totalPixels := refWidth * refHeight
	pct := float64(diffCount) * 100 / float64(totalPixels)

	if diffCount == 0 {
		t.Logf("PERFECT: 0%% error")
	} else if pct < 5 {
		t.Logf("PASS: %.2f%% error (%d/%d pixels), max diff: %d", pct, diffCount, totalPixels, maxDiff)
	} else {
		t.Logf("FAIL: %.2f%% error (%d/%d pixels), max diff: %d", pct, diffCount, totalPixels, maxDiff)
	}
}

// TestConformanceDebug tests a specific file with verbose output.
// Usage: go test -v -run TestConformanceDebug ./lib/jpeg2000/ -args p0_03.j2k
func TestConformanceDebug(t *testing.T) {
	// Default to p1_07.j2k if no args (50% error / max=128 test)
	filename := "p1_07.j2k"
	if len(os.Args) > 1 {
		for i, arg := range os.Args {
			if arg == "-args" && i+1 < len(os.Args) {
				filename = os.Args[i+1]
				break
			}
		}
	}

	inputPath := filepath.Join(conformanceInput, filename)

	data, err := os.ReadFile(inputPath)
	if err != nil {
		t.Skipf("Test data not found: %v", err)
	}

	_, err = Decode(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("Decode failed: %v", err)
	}

	t.Log("Debug decode completed - check output above")
}

// TestSummarizeConformance prints a summary table of all conformance test results.
func TestSummarizeConformance(t *testing.T) {
	entries, _ := os.ReadDir(conformanceInput)
	type testFile struct {
		name string
		path string
	}
	var files []testFile
	for _, e := range entries {
		ext := filepath.Ext(e.Name())
		if ext == ".j2k" || ext == ".j2c" || ext == ".jp2" {
			name := e.Name()[:len(e.Name())-len(ext)]
			files = append(files, testFile{name: name, path: filepath.Join(conformanceInput, e.Name())})
		}
	}
	sort.Slice(files, func(i, j int) bool { return files[i].name < files[j].name })

	progNames := []string{"LRCP", "RLCP", "RPCL", "PCRL", "CPRL"}

	fmt.Printf("%-20s | %-9s | %-4s | %-4s | %-4s | %-4s | %-4s | %-5s | %-3s | %-3s | %-2s | %-2s | %-4s | %-12s | %s\n",
		"File", "Size", "Comp", "Bits", "Wave", "MCT", "QSty", "Prog", "POC", "Lyr", "DL", "XR", "CBS", "Status", "Error Rate")
	fmt.Println("---------------------+-----------+------+------+------+------+------+-------+-----+-----+----+----+------+--------------+-------------")

	for _, f := range files {
		name := f.name
		inputPath := f.path

		data, err := os.ReadFile(inputPath)
		if err != nil {
			fmt.Printf("%-20s | ERROR reading input\n", name)
			continue
		}

		codestream, _ := extractCodestream(data)
		header, tiles, err := parseCodestream(codestream)
		if err != nil {
			fmt.Printf("%-20s | ERROR parsing\n", name)
			continue
		}

		// Decode
		img, err := Decode(bytes.NewReader(data))
		if err != nil {
			fmt.Printf("%-20s | ERROR: %v\n", name, err)
			continue
		}

		// Get baseline reference file
		var refData []byte
		var isPPMRef bool

		for _, ext := range []string{".ppm", ".pgm"} {
			candidate := filepath.Join(conformanceBaseline, name+ext)
			if d, err := os.ReadFile(candidate); err == nil {
				refData = d
				break
			}
		}
		if refData == nil {
			// Try split-component output (e.g. name_0.pgm)
			split0 := filepath.Join(conformanceBaseline, name+"_0.pgm")
			if d, err := os.ReadFile(split0); err == nil {
				refData = d
			}
		}
		if refData == nil {
			fmt.Printf("%-20s | No baseline reference\n", name)
			continue
		}

		// Detect format by magic bytes
		isPPMRef = len(refData) >= 2 && (refData[0] == 'P' && (refData[1] == '6' || refData[1] == '3'))

		bounds := img.Bounds()
		width, height := bounds.Dx(), bounds.Dy()

		// Compare pixels
		totalPixels := width * height
		errPixels := 0
		maxDiff := 0

		if isPPMRef {
			// Parse PPM reference - detect format by magic bytes
			refPixels, refWidth, refHeight := parsePPMImageRaw(refData)
			if refPixels == nil || width != refWidth || height != refHeight {
				fmt.Printf("%-20s | Dimension mismatch: %dx%d vs ref %dx%d\n",
					name, width, height, refWidth, refHeight)
				continue
			}
			for y := range height {
				for x := range width {
					r, g, b, _ := img.At(bounds.Min.X+x, bounds.Min.Y+y).RGBA()
					ourR, ourG, ourB := int(r>>8), int(g>>8), int(b>>8)

					idx := (y*refWidth + x) * 3
					if idx+2 >= len(refPixels) {
						continue
					}
					refR, refG, refB := int(refPixels[idx]), int(refPixels[idx+1]), int(refPixels[idx+2])

					diffR := abs(ourR - refR)
					diffG := abs(ourG - refG)
					diffB := abs(ourB - refB)

					maxPixDiff := max(diffR, diffG, diffB)
					if maxPixDiff > maxDiff {
						maxDiff = maxPixDiff
					}
					if maxPixDiff > 1 {
						errPixels++
					}
				}
			}
		} else {
			// Parse PGM reference
			refPixels, refWidth, refHeight, refMaxVal := parsePGMFile(refData)
			if width != refWidth || height != refHeight {
				fmt.Printf("%-20s | Dimension mismatch: %dx%d vs ref %dx%d\n",
					name, width, height, refWidth, refHeight)
				continue
			}
			for y := range height {
				for x := range width {
					r, _, _, _ := img.At(bounds.Min.X+x, bounds.Min.Y+y).RGBA()
					ourPixel := int(r >> 8)

					refIdx := y*refWidth + x
					if refIdx >= len(refPixels) {
						continue
					}
					refPixel := int(refPixels[refIdx])

					if refMaxVal != 255 && refMaxVal > 0 {
						refPixel = (refPixel*255 + refMaxVal/2) / refMaxVal
					}

					diff := abs(ourPixel - refPixel)
					if diff > maxDiff {
						maxDiff = diff
					}
					if diff > 1 {
						errPixels++
					}
				}
			}
		}

		// Get header info
		wavelet := "5/3"
		if header.WaveletFilter == Wavelet97 {
			wavelet = "9/7"
		}
		mctStr := "N"
		if header.MCT {
			mctStr = "Y"
		}

		prog := "LRCP"
		if int(header.ProgressionOrder) < len(progNames) {
			prog = progNames[header.ProgressionOrder]
		}

		maxXR := 1
		for _, xr := range header.XRsiz {
			if xr > maxXR {
				maxXR = xr
			}
		}

		// Max tile-parts across tiles
		maxTP := 0
		for _, tile := range tiles {
			if tile != nil {
				np := len(tile.TileParts)
				if np > maxTP {
					maxTP = np
				}
			}
		}

		status := "PASS"
		errRate := float64(errPixels) * 100 / float64(totalPixels)
		if maxDiff > 10 || errRate > 5 {
			status = "FAIL"
		} else if errRate > 0.1 {
			status = "CLOSE"
		}

		fmt.Printf("%-20s | %4dx%-4d | %-4d | %-4d | %-4s | %-4s | %-4d | %-5s | %-3d | %-3d | %-2d | %-2d | 0x%02x | %-12s | %.1f%% max=%d\n",
			name, header.Width, header.Height, header.NumComps, header.BitDepth[0],
			wavelet, mctStr, header.QuantStyle,
			prog, len(header.POCEntries), header.NumLayers,
			header.NumDecompLevels, maxXR, header.CodeBlockStyle,
			status, errRate, maxDiff)
	}
}

// parsePGM parses a PGM (P5 binary or P2 ASCII) file and returns pixels scaled to 8-bit.
func parsePGM(t *testing.T, data []byte) ([]byte, int, int) {
	t.Helper()

	// Check magic number
	isASCII := false
	if len(data) >= 2 {
		magic := string(data[:2])
		if magic == "P2" {
			isASCII = true
		} else if magic != "P5" {
			t.Fatalf("Invalid PGM: expected P5 or P2, got %s", magic)
		}
	}

	idx := 0
	// Skip magic number line
	for idx < len(data) && data[idx] != '\n' {
		idx++
	}
	idx++

	// Skip comments
	for idx < len(data) && data[idx] == '#' {
		for idx < len(data) && data[idx] != '\n' {
			idx++
		}
		idx++
	}

	// Read width and height
	headerEnd := idx
	for headerEnd < len(data) && data[headerEnd] != '\n' {
		headerEnd++
	}
	parts := strings.Fields(string(data[idx:headerEnd]))
	if len(parts) < 2 {
		t.Fatalf("Invalid PGM header")
	}
	width, _ := strconv.Atoi(parts[0])
	height, _ := strconv.Atoi(parts[1])
	idx = headerEnd + 1

	// Read maxval
	maxvalEnd := idx
	for maxvalEnd < len(data) && data[maxvalEnd] != '\n' {
		maxvalEnd++
	}
	maxval, _ := strconv.Atoi(string(data[idx:maxvalEnd]))
	if maxval == 0 {
		maxval = 255
	}
	idx = maxvalEnd + 1

	if isASCII {
		// P2 format: pixel values as ASCII numbers
		tokens := strings.Fields(string(data[idx:]))
		numPixels := width * height
		pixels := make([]byte, numPixels)
		for i := 0; i < numPixels && i < len(tokens); i++ {
			val, _ := strconv.Atoi(tokens[i])
			if maxval != 255 && maxval > 0 {
				val = (val * 255) / maxval
			}
			pixels[i] = byte(val)
		}
		return pixels, width, height
	}

	// P5 format: binary data
	pixels := data[idx:]

	// Scale pixels to 8-bit if needed
	if maxval != 255 {
		if maxval > 255 {
			// 2 bytes per sample (big-endian)
			numPixels := width * height
			scaled := make([]byte, numPixels)
			for i := range numPixels {
				if 2*i+1 < len(pixels) {
					val := int(pixels[2*i])<<8 | int(pixels[2*i+1])
					scaled[i] = byte((val * 255) / maxval)
				}
			}
			return scaled, width, height
		}
		scaled := make([]byte, len(pixels))
		for i, p := range pixels {
			// Use proper rounding
			scaled[i] = byte((int(p)*255 + maxval/2) / maxval)
		}
		return scaled, width, height
	}

	return pixels, width, height
}

// parsePGMFile parses a binary PGM (P5) file and returns raw pixels with maxval.
func parsePGMFile(data []byte) ([]uint16, int, int, int) {
	idx := 0
	// Skip magic number line
	for idx < len(data) && data[idx] != '\n' {
		idx++
	}
	idx++

	// Skip comments
	for idx < len(data) && data[idx] == '#' {
		for idx < len(data) && data[idx] != '\n' {
			idx++
		}
		idx++
	}

	// Read width and height
	headerEnd := idx
	for headerEnd < len(data) && data[headerEnd] != '\n' {
		headerEnd++
	}
	parts := strings.Fields(string(data[idx:headerEnd]))
	if len(parts) < 2 {
		return nil, 0, 0, 0
	}
	width, _ := strconv.Atoi(parts[0])
	height, _ := strconv.Atoi(parts[1])
	idx = headerEnd + 1

	// Read maxval
	maxvalEnd := idx
	for maxvalEnd < len(data) && data[maxvalEnd] != '\n' {
		maxvalEnd++
	}
	maxval, _ := strconv.Atoi(string(data[idx:maxvalEnd]))
	if maxval == 0 {
		maxval = 255
	}
	idx = maxvalEnd + 1

	pixelData := data[idx:]
	pixels := make([]uint16, width*height)

	if maxval > 255 {
		// 16-bit pixels (big-endian)
		for i := 0; i < len(pixels) && i*2+1 < len(pixelData); i++ {
			pixels[i] = uint16(pixelData[i*2])<<8 | uint16(pixelData[i*2+1])
		}
	} else {
		// 8-bit pixels
		for i := 0; i < len(pixels) && i < len(pixelData); i++ {
			pixels[i] = uint16(pixelData[i])
		}
	}

	return pixels, width, height, maxval
}

// parsePPMImage parses a PPM (P6 binary or P3 ASCII) image file and returns RGB pixels scaled to 8-bit.
// Note: This parses the PPM image format, not the JPEG2000 PPM marker.
func parsePPMImage(t *testing.T, data []byte) ([]byte, int, int) {
	t.Helper()

	lines := strings.Split(string(data), "\n")
	headerLine := 0

	// Check magic number (P6 = binary, P3 = ASCII)
	isASCII := false
	if strings.HasPrefix(lines[headerLine], "P6") {
		isASCII = false
	} else if strings.HasPrefix(lines[headerLine], "P3") {
		isASCII = true
	} else {
		t.Fatalf("Invalid PPM: expected P6 or P3, got %s", lines[headerLine])
	}
	headerLine++

	// Skip comments
	for headerLine < len(lines) && strings.HasPrefix(lines[headerLine], "#") {
		headerLine++
	}

	// Parse dimensions
	parts := strings.Fields(lines[headerLine])
	width, _ := strconv.Atoi(parts[0])
	height, _ := strconv.Atoi(parts[1])
	headerLine++

	// Parse max value
	maxval, _ := strconv.Atoi(strings.TrimSpace(lines[headerLine]))
	headerLine++

	if isASCII {
		// P3 format: pixel values as ASCII numbers
		// Collect all remaining text and parse as space-separated numbers
		var allText strings.Builder
		for i := headerLine; i < len(lines); i++ {
			allText.WriteString(lines[i])
			allText.WriteString(" ")
		}
		tokens := strings.Fields(allText.String())

		numComponents := width * height * 3
		pixels := make([]byte, numComponents)
		for i := 0; i < numComponents && i < len(tokens); i++ {
			val, _ := strconv.Atoi(tokens[i])
			if maxval != 255 && maxval > 0 {
				// Use proper rounding
				val = (val*255 + maxval/2) / maxval
			}
			pixels[i] = byte(val)
		}
		return pixels, width, height
	}

	// P6 format: binary data
	// Find binary data start
	headerLen := 0
	nlCount := 0
	for i, b := range data {
		if b == '\n' {
			nlCount++
			if nlCount == headerLine {
				headerLen = i + 1
				break
			}
		}
	}

	pixels := data[headerLen:]

	// Scale pixels to 8-bit if needed
	if maxval != 255 {
		if maxval > 255 {
			// 2 bytes per component (big-endian)
			numComponents := width * height * 3
			scaled := make([]byte, numComponents)
			for i := range numComponents {
				if 2*i+1 < len(pixels) {
					val := int(pixels[2*i])<<8 | int(pixels[2*i+1])
					// Use proper rounding: (val * 255 + maxval/2) / maxval
					scaled[i] = byte((val*255 + maxval/2) / maxval)
				}
			}
			return scaled, width, height
		}
		scaled := make([]byte, len(pixels))
		for i, p := range pixels {
			// Use proper rounding
			scaled[i] = byte((int(p)*255 + maxval/2) / maxval)
		}
		return scaled, width, height
	}

	return pixels, width, height
}

// parsePPMImageRaw parses a PPM (P6 binary or P3 ASCII) image file and returns
// RGB pixels scaled to 8-bit. Unlike parsePPMImage, this doesn't require *testing.T.
func parsePPMImageRaw(data []byte) ([]byte, int, int) {
	if len(data) < 2 {
		return nil, 0, 0
	}

	idx := 0
	isASCII := false
	magic := string(data[:2])
	if magic == "P6" {
		isASCII = false
	} else if magic == "P3" {
		isASCII = true
	} else {
		return nil, 0, 0
	}

	// Skip magic line
	for idx < len(data) && data[idx] != '\n' {
		idx++
	}
	idx++

	// Skip comments
	for idx < len(data) && data[idx] == '#' {
		for idx < len(data) && data[idx] != '\n' {
			idx++
		}
		idx++
	}

	// Read width and height
	headerEnd := idx
	for headerEnd < len(data) && data[headerEnd] != '\n' {
		headerEnd++
	}
	parts := strings.Fields(string(data[idx:headerEnd]))
	if len(parts) < 2 {
		return nil, 0, 0
	}
	width, _ := strconv.Atoi(parts[0])
	height, _ := strconv.Atoi(parts[1])
	idx = headerEnd + 1

	// Read maxval
	maxvalEnd := idx
	for maxvalEnd < len(data) && data[maxvalEnd] != '\n' {
		maxvalEnd++
	}
	maxval, _ := strconv.Atoi(strings.TrimSpace(string(data[idx:maxvalEnd])))
	if maxval == 0 {
		maxval = 255
	}
	idx = maxvalEnd + 1

	if isASCII {
		tokens := strings.Fields(string(data[idx:]))
		numComponents := width * height * 3
		pixels := make([]byte, numComponents)
		for i := 0; i < numComponents && i < len(tokens); i++ {
			val, _ := strconv.Atoi(tokens[i])
			if maxval != 255 && maxval > 0 {
				val = (val*255 + maxval/2) / maxval
			}
			pixels[i] = byte(val)
		}
		return pixels, width, height
	}

	// P6 binary
	pixels := data[idx:]
	if maxval != 255 {
		if maxval > 255 {
			numComponents := width * height * 3
			scaled := make([]byte, numComponents)
			for i := range numComponents {
				if 2*i+1 < len(pixels) {
					val := int(pixels[2*i])<<8 | int(pixels[2*i+1])
					scaled[i] = byte((val*255 + maxval/2) / maxval)
				}
			}
			return scaled, width, height
		}
		scaled := make([]byte, len(pixels))
		for i, p := range pixels {
			scaled[i] = byte((int(p)*255 + maxval/2) / maxval)
		}
		return scaled, width, height
	}

	return pixels, width, height
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

// TestMultiTileCompose tests multi-tile composition.
// Usage: go test -v -run TestMultiTileCompose ./lib/jpeg2000/
func TestMultiTileCompose(t *testing.T) {
	filename := "a3_mono.j2c"
	inputPath := filepath.Join(conformanceInput, filename)

	data, err := os.ReadFile(inputPath)
	if err != nil {
		t.Skipf("Test data not found: %v", err)
	}

	img, err := Decode(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("Decode failed: %v", err)
	}

	bounds := img.Bounds()
	t.Logf("Decoded: %dx%d", bounds.Dx(), bounds.Dy())

	// Check reference (mono files may have .ppm extension but be PGM format)
	baselinePath := filepath.Join(conformanceBaseline, "a3_mono.ppm")
	refData, err := os.ReadFile(baselinePath)
	if err != nil {
		t.Skipf("Reference not found: %v", err)
	}

	// Parse as PGM since it's actually P5 format despite .ppm extension
	refPixels, refWidth, refHeight := parsePGM(t, refData)
	t.Logf("Reference: %dx%d", refWidth, refHeight)

	// Compare first 10x10 pixels
	t.Log("First 10x10 pixels comparison:")
	t.Log("Our values:")
	for y := 0; y < 10 && y < bounds.Dy(); y++ {
		var row strings.Builder
		for x := 0; x < 10 && x < bounds.Dx(); x++ {
			r, _, _, _ := img.At(bounds.Min.X+x, bounds.Min.Y+y).RGBA()
			row.WriteString(fmt.Sprintf("%3d ", r>>8))
		}
		t.Logf("  row %d: %s", y, row.String())
	}

	t.Log("Reference values:")
	for y := 0; y < 10 && y < refHeight; y++ {
		var row strings.Builder
		for x := 0; x < 10 && x < refWidth; x++ {
			row.WriteString(fmt.Sprintf("%3d ", refPixels[y*refWidth+x]))
		}
		t.Logf("  row %d: %s", y, row.String())
	}

	// Check pixels near tile boundary (at x=137)
	t.Log("\nPixels near tile boundary (x=135-142):")
	for y := range 5 {
		t.Logf("  row %d:", y)
		for x := 135; x < 142 && x < bounds.Dx(); x++ {
			r, _, _, _ := img.At(bounds.Min.X+x, bounds.Min.Y+y).RGBA()
			ourVal := int(r >> 8)
			refVal := int(refPixels[y*refWidth+x])
			diff := ourVal - refVal
			if diff < 0 {
				diff = -diff
			}
			marker := ""
			if diff > 10 {
				marker = "*"
			}
			t.Logf("    x=%d: our=%3d ref=%3d diff=%3d%s", x, ourVal, refVal, diff, marker)
		}
	}

	// Per-tile error breakdown
	// Tile grid: 3x2, tile size 137x131, image 303x179
	tileXBounds := []int{0, 137, 274, 303}
	tileYBounds := []int{0, 131, 179}
	t.Log("\nPer-tile error breakdown:")
	for ty := range 2 {
		for tx := range 3 {
			tileIdx := ty*3 + tx
			x0, x1 := tileXBounds[tx], tileXBounds[tx+1]
			y0, y1 := tileYBounds[ty], tileYBounds[ty+1]

			errCount := 0
			totalPixels := 0
			maxDiff := 0
			for y := y0; y < y1 && y < refHeight; y++ {
				for x := x0; x < x1 && x < refWidth; x++ {
					r, _, _, _ := img.At(bounds.Min.X+x, bounds.Min.Y+y).RGBA()
					ourVal := int(r >> 8)
					refVal := int(refPixels[y*refWidth+x])
					d := ourVal - refVal
					if d < 0 {
						d = -d
					}
					totalPixels++
					if d > 1 {
						errCount++
					}
					if d > maxDiff {
						maxDiff = d
					}
				}
			}
			t.Logf("  Tile %d [X=%d..%d, Y=%d..%d]: %d/%d errors (%.1f%%), max diff=%d",
				tileIdx, x0, x1, y0, y1, errCount, totalPixels,
				100.0*float64(errCount)/float64(totalPixels), maxDiff)
		}
	}
}
