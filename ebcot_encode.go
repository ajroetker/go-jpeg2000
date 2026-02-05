package jpeg2000

// EBCOT (Embedded Block Coding with Optimized Truncation) Tier-1 Encoder
//
// This implements the encoding side of JPEG2000's bit-plane coding as specified
// in ITU-T T.800 Annex D. The encoder mirrors the decoder in ebcot.go, using
// the same context computation LUTs, flag constants, and stripe scanning order.
//
// Each code block is encoded using three coding passes per bit-plane:
// 1. Significance Propagation Pass: Encodes coefficients with significant neighbors
// 2. Magnitude Refinement Pass: Refines already-significant coefficients
// 3. Cleanup Pass: Encodes remaining coefficients with run-length optimization
//
// The encoder produces per-pass data segments that can be selectively truncated
// by the rate controller for optimal quality at a given bitrate.

// EncodedBlock holds the result of EBCOT encoding a single code block.
type EncodedBlock struct {
	// Per-pass encoded data
	Passes []EncodedPass

	// Number of significant (non-zero) bit planes
	NumBitPlanes int

	// Total number of coding passes
	NumPasses int

	// Zero bit planes (leading planes with no significant coefficients)
	ZeroBitPlanes int
}

// EncodedPass holds data for a single coding pass.
type EncodedPass struct {
	Data       []byte  // MQ-encoded (or raw) bytes for this pass
	Length     int     // Length in bytes
	Type       int     // Pass type: 0=SPP, 1=MRP, 2=Cleanup
	BitPlane   int     // Which bit plane this pass encodes
	Distortion float64 // Distortion reduction from this pass (for rate control)
}

// ebcotEncoder encodes EBCOT code blocks.
type ebcotEncoder struct {
	mq     *mqEncoder
	width  int
	height int

	// State arrays (width+2 x height+2 for border handling)
	state [][]uint8 // Flags for each coefficient
	data  [][]int32 // Absolute magnitude data

	// Sign array (separate from flags for clarity)
	sign [][]int32 // 1 = negative, 0 = positive

	// Code block style flags
	codeBlockStyle byte

	// Number of significant bit planes
	numBitPlanes int
}

// newEBCOTEncoder creates a new EBCOT encoder for the given code block dimensions.
func newEBCOTEncoder(width, height int) *ebcotEncoder {
	e := &ebcotEncoder{
		width:  width,
		height: height,
		mq:     newMQEncoder(),
	}

	// Allocate state arrays with borders
	e.state = make([][]uint8, height+2)
	e.data = make([][]int32, height+2)
	e.sign = make([][]int32, height+2)
	for i := range e.state {
		e.state[i] = make([]uint8, width+2)
		e.data[i] = make([]int32, width+2)
		e.sign[i] = make([]int32, width+2)
	}

	return e
}

// EncodeCodeBlock encodes a single code block and returns the encoded data.
// coeffs[y][x] contains signed wavelet coefficients (no border padding).
// subbandType is needed for context selection.
// mb is the number of magnitude bit planes (GuardBits + Exponent - 1).
func (e *ebcotEncoder) EncodeCodeBlock(coeffs [][]int32, subbandType SubbandType, mb int) *EncodedBlock {
	e.width = len(coeffs[0])
	e.height = len(coeffs)

	// Ensure arrays are large enough
	if len(e.state) < e.height+2 || len(e.state[0]) < e.width+2 {
		e.state = make([][]uint8, e.height+2)
		e.data = make([][]int32, e.height+2)
		e.sign = make([][]int32, e.height+2)
		for i := range e.state {
			e.state[i] = make([]uint8, e.width+2)
			e.data[i] = make([]int32, e.width+2)
			e.sign[i] = make([]int32, e.width+2)
		}
	}

	// Clear state arrays
	for y := range e.state {
		for x := range e.state[y] {
			e.state[y][x] = 0
			e.data[y][x] = 0
			e.sign[y][x] = 0
		}
	}

	// Copy coefficients into data array with border offset.
	// Store absolute values in data, signs in sign array.
	// The EBCOT encoder works with absolute magnitudes.
	maxMag := int32(0)
	for y := 0; y < e.height; y++ {
		for x := 0; x < e.width; x++ {
			val := coeffs[y][x]
			if val < 0 {
				e.data[y+1][x+1] = -val
				e.sign[y+1][x+1] = 1
			} else {
				e.data[y+1][x+1] = val
				e.sign[y+1][x+1] = 0
			}
			if e.data[y+1][x+1] > maxMag {
				maxMag = e.data[y+1][x+1]
			}
		}
	}

	// Determine number of bit planes from maximum magnitude
	numBitPlanes := 0
	if maxMag > 0 {
		tmp := maxMag
		for tmp > 0 {
			numBitPlanes++
			tmp >>= 1
		}
	}
	e.numBitPlanes = numBitPlanes

	// Compute zero bit planes
	zeroBitPlanes := 0
	if mb > numBitPlanes {
		zeroBitPlanes = mb - numBitPlanes
	}

	result := &EncodedBlock{
		NumBitPlanes:  numBitPlanes,
		ZeroBitPlanes: zeroBitPlanes,
	}

	if numBitPlanes == 0 {
		// All coefficients are zero - no passes needed
		return result
	}

	// In standard JPEG2000 (no ERTERM), the MQ coder runs continuously across
	// all passes within a code block. The decoder initializes one MQ stream from
	// concatenated data and decodes all passes sequentially.
	//
	// Initialize MQ once, encode all passes, flush once at end.
	e.mq.Reset()
	e.mq.ResetContexts()

	// Encode bit planes from MSB to LSB
	// First bit plane: only cleanup pass
	// Subsequent bit planes: SPP, MRP, Cleanup
	startBP := numBitPlanes - 1

	for bp := startBP; bp >= 0; bp-- {
		if bp == startBP {
			// First bit plane: cleanup pass only
			e.encodeCleanupPassCont(bp, subbandType)
			result.Passes = append(result.Passes, EncodedPass{
				Type:     passTypeCleanup,
				BitPlane: bp,
			})
		} else {
			// Subsequent bit planes: SPP, MRP, Cleanup
			e.encodeSignificancePropagationPassCont(bp, subbandType)
			result.Passes = append(result.Passes, EncodedPass{
				Type:     passTypeSPP,
				BitPlane: bp,
			})

			e.encodeMagnitudeRefinementPassCont(bp)
			result.Passes = append(result.Passes, EncodedPass{
				Type:     passTypeMRP,
				BitPlane: bp,
			})

			e.encodeCleanupPassCont(bp, subbandType)
			result.Passes = append(result.Passes, EncodedPass{
				Type:     passTypeCleanup,
				BitPlane: bp,
			})
		}

		// Clear visited flags for next bit plane
		e.clearVisited()
	}

	// Flush the MQ encoder once for the entire code block
	allData := e.mq.Flush()

	// In standard JPEG2000 (no ERTERM), the MQ coder runs continuously
	// across all passes. The entire code block produces one data blob.
	// We store the full data on the LAST pass; all preceding passes have
	// Length=0 and nil Data. When the packet encoder includes all passes
	// (single layer), it sums lengths and concatenates data correctly.
	if len(result.Passes) > 0 {
		lastIdx := len(result.Passes) - 1
		result.Passes[lastIdx].Data = allData
		result.Passes[lastIdx].Length = len(allData)
	}

	result.NumPasses = len(result.Passes)
	return result
}

// clearVisited clears visited flags for new bit plane
func (e *ebcotEncoder) clearVisited() {
	for y := 1; y <= e.height; y++ {
		for x := 1; x <= e.width; x++ {
			e.state[y][x] &^= flagVisited
		}
	}
}

// encodeSignificancePropagationPassCont encodes coefficients that have at least one
// significant neighbor but are not yet significant themselves.
// Continuous mode: does not Reset/Flush MQ — called within a continuous MQ stream.
func (e *ebcotEncoder) encodeSignificancePropagationPassCont(bp int, subbandType SubbandType) {
	bpBit := int32(1) << uint(bp)

	// Process in stripe-oriented order (4 rows at a time, column by column)
	for stripe := 0; stripe < (e.height+3)/4; stripe++ {
		y0 := stripe * 4
		y1 := min(y0+4, e.height)

		for x := 0; x < e.width; x++ {
			for y := y0; y < y1; y++ {
				yy := y + 1
				xx := x + 1

				// Skip if already significant
				if e.state[yy][xx]&flagSignificant != 0 {
					continue
				}

				// Check if has significant neighbor
				if !e.hasSignificantNeighborEnc(xx, yy) {
					continue
				}

				// Mark as visited
				e.state[yy][xx] |= flagVisited

				// Encode significance bit
				ctx := e.getSigContextEnc(xx, yy, subbandType)
				sig := int(0)
				if e.data[yy][xx]&bpBit != 0 {
					sig = 1
				}
				e.mq.Encode(ctx, sig)

				if sig != 0 {
					// Becomes significant
					e.setSignificantEnc(xx, yy)

					// Encode sign
					signCtx, xorBit := e.getSignContextEnc(xx, yy)
					signBit := e.sign[yy][xx] ^ int32(xorBit)
					e.mq.Encode(signCtx, int(signBit))
				}
			}
		}
	}
}

// encodeMagnitudeRefinementPassCont encodes refinement bits for already-significant coefficients.
// Continuous mode: does not Reset/Flush MQ.
func (e *ebcotEncoder) encodeMagnitudeRefinementPassCont(bp int) {
	bpBit := int32(1) << uint(bp)

	// Process in stripe-oriented order
	for stripe := 0; stripe < (e.height+3)/4; stripe++ {
		y0 := stripe * 4
		y1 := min(y0+4, e.height)

		for x := 0; x < e.width; x++ {
			for y := y0; y < y1; y++ {
				yy := y + 1
				xx := x + 1

				// Skip if not significant
				if e.state[yy][xx]&flagSignificant == 0 {
					continue
				}
				// Skip if visited in current pass
				if e.state[yy][xx]&flagVisited != 0 {
					continue
				}

				// Determine context
				var ctx int
				hasNeighbor := e.hasSignificantNeighborEnc(xx, yy)
				isFirstRefinement := e.state[yy][xx]&flagRefined == 0

				if !isFirstRefinement {
					ctx = 16
				} else if hasNeighbor {
					ctx = ctxMagOther // 15
				} else {
					ctx = ctxMagFirst // 14
				}

				if isFirstRefinement {
					e.state[yy][xx] |= flagRefined
				}

				// Encode the refinement bit
				bit := 0
				if e.data[yy][xx]&bpBit != 0 {
					bit = 1
				}
				e.mq.Encode(ctx, bit)
			}
		}
	}

}

// encodeCleanupPassCont encodes remaining coefficients not yet processed.
// Uses run-length mode for efficiency when 4 consecutive coefficients
// have no significant neighbors.
// Continuous mode: does not Reset/Flush MQ.
func (e *ebcotEncoder) encodeCleanupPassCont(bp int, subbandType SubbandType) {
	bpBit := int32(1) << uint(bp)

	// Process in stripe-oriented order
	for stripe := 0; stripe < (e.height+3)/4; stripe++ {
		y0 := stripe * 4

		for x := 0; x < e.width; x++ {
			xx := x + 1

			// Check if we can use run mode
			// Requires full stripe (4 rows) and all coefficients have flags==0
			runMode := e.canUseRunModeEnc(y0, xx)

			if runMode {
				// Check if any coefficient in the stripe column is significant at this bit plane
				runLen := 4
				for i := 0; i < 4 && y0+i < e.height; i++ {
					yy := y0 + i + 1
					if e.state[yy][xx]&flagVisited != 0 || e.state[yy][xx]&flagSignificant != 0 {
						continue
					}
					if e.data[yy][xx]&bpBit != 0 {
						runLen = i
						break
					}
				}

				if runLen == 4 {
					// No significant coefficient found in run
					e.mq.Encode(ctxAggZero, 0) // aggregation: all zero
					for i := 0; i < 4 && y0+i < e.height; i++ {
						e.state[y0+i+1][xx] |= flagVisited
					}
					continue
				}

				// Found a significant coefficient at position runLen
				e.mq.Encode(ctxAggZero, 1) // aggregation: has significant

				// Encode run length (2 bits, MSB first)
				e.mq.Encode(ctxUniform, (runLen>>1)&1)
				e.mq.Encode(ctxUniform, runLen&1)

				// Process coefficients in the stripe
				for i := 0; i < 4 && y0+i < e.height; i++ {
					yy := y0 + i + 1
					y := y0 + i

					if e.state[yy][xx]&flagVisited != 0 {
						continue
					}

					if i < runLen {
						e.state[yy][xx] |= flagVisited
						continue
					}

					if i == runLen {
						if e.state[yy][xx]&flagSignificant != 0 {
							e.state[yy][xx] |= flagVisited
							continue
						}
						// This one is known significant from the run length
						e.setSignificantEnc(xx, yy)
						e.state[yy][xx] |= flagVisited

						_ = y // suppress unused warning for vsc logic
						signCtx, xorBit := e.getSignContextEnc(xx, yy)
						signBit := e.sign[yy][xx] ^ int32(xorBit)
						e.mq.Encode(signCtx, int(signBit))
					} else {
						e.encodeCleanupOne(xx, yy, bp, subbandType, bpBit)
					}
				}
			} else {
				// No run mode - encode each coefficient individually
				for i := 0; i < 4 && y0+i < e.height; i++ {
					yy := y0 + i + 1

					if e.state[yy][xx]&flagVisited != 0 {
						continue
					}

					e.encodeCleanupOne(xx, yy, bp, subbandType, bpBit)
				}
			}
		}
	}

	// Write segmentation symbols if enabled
	if e.codeBlockStyle&cbstySegSymbols != 0 {
		// 4 bits: 0xA = 1010 binary
		e.mq.Encode(ctxUniform, 1)
		e.mq.Encode(ctxUniform, 0)
		e.mq.Encode(ctxUniform, 1)
		e.mq.Encode(ctxUniform, 0)
	}

}

// encodeCleanupOne encodes one coefficient in cleanup pass
func (e *ebcotEncoder) encodeCleanupOne(x, y int, bp int, subbandType SubbandType, bpBit int32) {
	e.state[y][x] |= flagVisited

	if e.state[y][x]&flagSignificant != 0 {
		return
	}

	// Encode significance
	ctx := e.getSigContextEnc(x, y, subbandType)
	sig := 0
	if e.data[y][x]&bpBit != 0 {
		sig = 1
	}
	e.mq.Encode(ctx, sig)

	if sig != 0 {
		e.setSignificantEnc(x, y)

		signCtx, xorBit := e.getSignContextEnc(x, y)
		signBit := e.sign[y][x] ^ int32(xorBit)
		e.mq.Encode(signCtx, int(signBit))
	}
}

// canUseRunModeEnc checks if 4 consecutive coefficients can use run-length mode.
// Same logic as decoder: requires full stripe and all 4 coefficients have flags==0.
func (e *ebcotEncoder) canUseRunModeEnc(y0 int, xx int) bool {
	if y0+4 > e.height {
		return false
	}
	for i := range 4 {
		if e.state[y0+i+1][xx] != 0 {
			return false
		}
	}
	return true
}

// setSignificantEnc marks coefficient as significant and updates neighbor flags.
// This mirrors setSignificant in the decoder.
func (e *ebcotEncoder) setSignificantEnc(x, y int) {
	if e.state[y][x]&flagSignificant != 0 {
		return
	}
	e.state[y][x] |= flagSignificant

	// Propagate neighbor significance (same as decoder)
	e.state[y][x-1] |= flagNeighborSig   // West
	e.state[y][x+1] |= flagNeighborSig   // East
	e.state[y+1][x] |= flagNeighborSig   // South
	e.state[y+1][x-1] |= flagNeighborSig // Southwest
	e.state[y+1][x+1] |= flagNeighborSig // Southeast
	e.state[y-1][x] |= flagNeighborSig   // North
	e.state[y-1][x-1] |= flagNeighborSig // Northwest
	e.state[y-1][x+1] |= flagNeighborSig // Northeast
}

// hasSignificantNeighborEnc checks for significant neighbors.
// Simplified version without VSC (encoder doesn't use VSC by default).
func (e *ebcotEncoder) hasSignificantNeighborEnc(x, y int) bool {
	return e.state[y][x]&flagNeighborSig != 0
}

// getSigContextEnc determines significance context (0-8).
// Same logic as decoder's getSigContext.
func (e *ebcotEncoder) getSigContextEnc(x, y int, subbandType SubbandType) int {
	h, v, d := e.countSigNeighborsEnc(x, y)

	switch subbandType {
	case SubbandHH:
		hv := h + v
		if d == 0 {
			if hv == 0 {
				return 0
			}
			if hv == 1 {
				return 1
			}
			return 2
		}
		if d == 1 {
			if hv == 0 {
				return 3
			}
			if hv == 1 {
				return 4
			}
			return 5
		}
		if d == 2 {
			if hv == 0 {
				return 6
			}
			return 7
		}
		return 8

	case SubbandHL:
		h, v = v, h
		fallthrough

	case SubbandLL, SubbandLH:
		if h == 0 {
			if v == 0 {
				if d == 0 {
					return 0
				}
				if d == 1 {
					return 1
				}
				return 2
			}
			if v == 1 {
				return 3
			}
			return 4
		}
		if h == 1 {
			if v == 0 {
				if d == 0 {
					return 5
				}
				return 6
			}
			return 7
		}
		return 8
	}

	return 0
}

// getSignContextEnc determines sign context and XOR prediction bit.
// Same logic as decoder's getSignContext (uses same LUTs).
func (e *ebcotEncoder) getSignContextEnc(x, y int) (int, int) {
	var lu int

	// West
	if e.state[y][x-1]&flagSignificant != 0 {
		lu |= lutSigW
		if e.sign[y][x-1] != 0 {
			lu |= lutSgnW
		}
	}

	// East
	if e.state[y][x+1]&flagSignificant != 0 {
		lu |= lutSigE
		if e.sign[y][x+1] != 0 {
			lu |= lutSgnE
		}
	}

	// North
	if e.state[y-1][x]&flagSignificant != 0 {
		lu |= lutSigN
		if e.sign[y-1][x] != 0 {
			lu |= lutSgnN
		}
	}

	// South
	if e.state[y+1][x]&flagSignificant != 0 {
		lu |= lutSigS
		if e.sign[y+1][x] != 0 {
			lu |= lutSgnS
		}
	}

	ctx := int(lutCtxnoSC[lu])
	xorBit := int(lutSPB[lu])

	return ctx, xorBit
}

// countSigNeighborsEnc counts significant neighbors in three categories.
// Same logic as decoder's countSigNeighbors (without VSC).
func (e *ebcotEncoder) countSigNeighborsEnc(x, y int) (h, v, d int) {
	if e.state[y][x-1]&flagSignificant != 0 {
		h++
	}
	if e.state[y][x+1]&flagSignificant != 0 {
		h++
	}
	if e.state[y-1][x]&flagSignificant != 0 {
		v++
	}
	if e.state[y+1][x]&flagSignificant != 0 {
		v++
	}
	if e.state[y-1][x-1]&flagSignificant != 0 {
		d++
	}
	if e.state[y-1][x+1]&flagSignificant != 0 {
		d++
	}
	if e.state[y+1][x-1]&flagSignificant != 0 {
		d++
	}
	if e.state[y+1][x+1]&flagSignificant != 0 {
		d++
	}
	return
}
