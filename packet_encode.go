package jpeg2000

// JPEG2000 Packet Encoder (Tier-2)
//
// This implements the encoding side of JPEG2000 packet assembly as specified
// in ITU-T T.800 Annex B.9-B.10. After EBCOT Tier-1 encoding produces per-pass
// data segments for each code block, the packet encoder assembles them into
// packets. Each packet contains data for one quality layer, one resolution level,
// one component, and one precinct.
//
// A packet consists of:
//   - Packet header (bit-stuffed): inclusion info, zero bit-planes, number of
//     passes, and data lengths for each included code block
//   - Packet body: concatenated raw encoded data from all included code blocks
//
// The encoder mirrors the decoder in packet.go and uses the same tag tree
// encoding, comma code, and Lblock mechanisms.

// PacketEncoder assembles encoded code blocks into JPEG2000 packets.
type PacketEncoder struct {
	header *MainHeader
}

// EncoderCodeBlock holds a code block for encoding with its grid position.
type EncoderCodeBlock struct {
	Block    *EncodedBlock
	SubbandX int // X position in subband's code block grid
	SubbandY int // Y position in subband's code block grid

	// Per-layer state tracking
	PassesIncluded int // Passes included in layers so far
	FirstLayer     int // First layer where included (-1 initially)

	// Lblock tracks the length indicator exponent for this code block.
	// Starts at 3 per ITU-T T.800. Incremented as needed to encode
	// pass lengths that exceed the current bit budget.
	Lblock int
}

// EncoderSubband holds code blocks for encoding within a subband.
type EncoderSubband struct {
	Type        SubbandType
	Width       int
	Height      int
	CodeBlocksX int
	CodeBlocksY int
	CodeBlocks  [][]*EncoderCodeBlock

	// Tag trees for this subband's precinct.
	// In the general case there is one pair of tag trees per precinct,
	// but for the common single-precinct case we store them directly here.
	InclusionTree *tagTree
	ZBPTree       *tagTree
}

// EncoderResolution holds subbands for encoding.
type EncoderResolution struct {
	Level    int
	Subbands []*EncoderSubband
}

// NewPacketEncoder creates a new packet encoder.
func NewPacketEncoder(header *MainHeader) *PacketEncoder {
	return &PacketEncoder{
		header: header,
	}
}

// NewEncoderCodeBlock creates an EncoderCodeBlock with properly initialized state.
func NewEncoderCodeBlock(block *EncodedBlock, subbandX, subbandY int) *EncoderCodeBlock {
	return &EncoderCodeBlock{
		Block:          block,
		SubbandX:       subbandX,
		SubbandY:       subbandY,
		PassesIncluded: 0,
		FirstLayer:     -1,
		Lblock:         3, // Initial value per ITU-T T.800
	}
}

// NewEncoderSubband creates an EncoderSubband with initialized tag trees.
// All code blocks must be added to CodeBlocks before calling EncodePacket.
func NewEncoderSubband(sbType SubbandType, width, height, codeBlocksX, codeBlocksY int) *EncoderSubband {
	sb := &EncoderSubband{
		Type:        sbType,
		Width:       width,
		Height:      height,
		CodeBlocksX: codeBlocksX,
		CodeBlocksY: codeBlocksY,
		CodeBlocks:  make([][]*EncoderCodeBlock, codeBlocksY),
	}

	for y := range codeBlocksY {
		sb.CodeBlocks[y] = make([]*EncoderCodeBlock, codeBlocksX)
	}

	// Create tag trees sized to the code block grid
	if codeBlocksX > 0 && codeBlocksY > 0 {
		sb.InclusionTree = newTagTree(codeBlocksX, codeBlocksY)
		sb.ZBPTree = newTagTree(codeBlocksX, codeBlocksY)
	}

	return sb
}

// InitSubbandTagTrees initializes the tag tree values for a subband.
// Must be called after all code blocks have been added and before the
// first call to EncodePacket. Sets inclusion values to each block's
// FirstLayer and ZBP values to each block's ZeroBitPlanes.
//
// For rate-distortion optimized encoding, the caller should set
// FirstLayer on each EncoderCodeBlock before calling this function.
// For simple single-layer encoding, set FirstLayer = 0 for all blocks
// that have data.
func InitSubbandTagTrees(sb *EncoderSubband) {
	if sb.InclusionTree == nil || sb.ZBPTree == nil {
		return
	}

	for y := 0; y < sb.CodeBlocksY; y++ {
		for x := 0; x < sb.CodeBlocksX; x++ {
			cb := sb.CodeBlocks[y][x]
			if cb == nil {
				continue
			}

			// Set inclusion value: the first layer where this block is included.
			// Per ITU-T T.800, this is what the tag tree encodes for inclusion.
			firstLayer := cb.FirstLayer
			if firstLayer < 0 {
				// Block is never included; use a large value so the tag tree
				// never signals inclusion for any layer the encoder will produce.
				firstLayer = sb.InclusionTree.levels * 32
			}
			sb.InclusionTree.setValueWithPropagation(x, y, int32(firstLayer))

			// Set zero bit-planes value
			zbp := 0
			if cb.Block != nil {
				zbp = cb.Block.ZeroBitPlanes
			}
			sb.ZBPTree.setValueWithPropagation(x, y, int32(zbp))
		}
	}
}

// EncodePacket encodes a single packet for one quality layer, one resolution,
// one component, and one precinct.
//
// passesPerBlock maps a linear block index (y*CodeBlocksX + x) to the number
// of NEW coding passes to include for that block in this layer. Blocks not
// present in the map (or with value 0) contribute no data to this packet.
//
// Returns the encoded packet bytes (header + body).
func (pe *PacketEncoder) EncodePacket(
	layer, resolution, component, precinct int,
	subbands []*EncoderSubband,
	passesPerBlock map[int]int,
) []byte {
	// Determine if the packet is empty (no code blocks contribute data).
	// Use a subband offset to look up the correct globally-unique block index,
	// matching the offset scheme used when building the passesPerBlock map.
	isEmpty := true
	{
		sbOff := 0
		for _, sb := range subbands {
			for y := 0; y < sb.CodeBlocksY; y++ {
				for x := 0; x < sb.CodeBlocksX; x++ {
					idx := sbOff + y*sb.CodeBlocksX + x
					if newPasses, ok := passesPerBlock[idx]; ok && newPasses > 0 {
						isEmpty = false
						break
					}
				}
				if !isEmpty {
					break
				}
			}
			if !isEmpty {
				break
			}
			sbOff += sb.CodeBlocksX * sb.CodeBlocksY
		}
	}

	// Create bit-stuffed writer for the packet header.
	// Per ITU-T T.800, packet headers use bit-stuffing where after a 0xFF
	// byte the MSB of the next byte must be 0.
	hdrWriter := newBitWriterWithStuffing()

	if isEmpty {
		// Empty packet: write a single 0-bit
		hdrWriter.WriteBit(0)
		hdrWriter.ByteAlign()
		return hdrWriter.Flush()
	}

	// Non-empty packet: write 1-bit
	hdrWriter.WriteBit(1)

	// Collect body data as we process code blocks.
	// Track subband offset for globally-unique block indices in passesPerBlock.
	var bodyData []byte
	subbandOffset := 0

	// Process each subband in order (LL/HL/LH/HH per resolution)
	// Per ITU-T T.800 B.9, code blocks are iterated:
	//   for each subband in resolution order:
	//     for cbY in cbY0..cbY1:
	//       for cbX in cbX0..cbX1:
	//         process code block
	for _, sb := range subbands {
		bodyData = pe.encodeSubbandPacketHeader(
			hdrWriter, sb, layer, subbandOffset, passesPerBlock, bodyData,
		)
		subbandOffset += sb.CodeBlocksX * sb.CodeBlocksY
	}

	// Byte-align the packet header
	hdrWriter.ByteAlign()

	// Assemble final packet: header + body
	headerBytes := hdrWriter.Flush()
	result := make([]byte, 0, len(headerBytes)+len(bodyData))
	result = append(result, headerBytes...)
	result = append(result, bodyData...)

	return result
}

// encodeSubbandPacketHeader encodes the packet header entries for all code
// blocks in a subband, appending their body data to bodyData.
func (pe *PacketEncoder) encodeSubbandPacketHeader(
	hdrWriter *bitWriter,
	sb *EncoderSubband,
	layer int,
	subbandOffset int,
	passesPerBlock map[int]int,
	bodyData []byte,
) []byte {
	if sb.CodeBlocksX == 0 || sb.CodeBlocksY == 0 {
		return bodyData
	}

	for y := 0; y < sb.CodeBlocksY; y++ {
		for x := 0; x < sb.CodeBlocksX; x++ {
			cb := sb.CodeBlocks[y][x]
			if cb == nil {
				continue
			}

			blockIdx := subbandOffset + y*sb.CodeBlocksX + x
			newPasses := passesPerBlock[blockIdx]

			// Encode inclusion information
			if cb.PassesIncluded == 0 {
				// Not yet included in any layer.
				// If newPasses > 0, this is the first inclusion.
				if newPasses > 0 {
					// Update FirstLayer if not already set
					if cb.FirstLayer < 0 {
						cb.FirstLayer = layer
						// Update tag tree value for this block
						if sb.InclusionTree != nil {
							sb.InclusionTree.setValueWithPropagation(x, y, int32(layer))
						}
					}
					// Encode via tag tree: will write 0-bits up to layer, then 1-bit
					if sb.InclusionTree != nil {
						sb.InclusionTree.encodeInclusion(x, y, int32(layer), hdrWriter)
					}

					// Encode zero bit-planes via tag tree (only on first inclusion)
					if sb.ZBPTree != nil {
						sb.ZBPTree.encodeValue(x, y, hdrWriter)
					}
				} else {
					// Not included in this layer - encode via tag tree
					// (will advance state but not signal inclusion)
					if sb.InclusionTree != nil {
						sb.InclusionTree.encodeInclusion(x, y, int32(layer), hdrWriter)
					}
					continue
				}
			} else {
				// Previously included - write 1-bit inclusion flag
				if newPasses > 0 {
					hdrWriter.WriteBit(1)
				} else {
					hdrWriter.WriteBit(0)
					continue
				}
			}

			// At this point, the block is included with newPasses > 0

			// Encode number of new coding passes (comma code)
			encodeNumPasses(hdrWriter, newPasses)

			// Encode pass data lengths with Lblock adaptation.
			// Collect the data bytes for the body.
			bodyData = pe.encodePassLengths(hdrWriter, cb, newPasses, bodyData)
		}
	}

	return bodyData
}

// encodeNumPasses encodes the number of new coding passes using the comma code
// defined in ITU-T T.800 B.10.5 / OpenJPEG.
//
// Encoding:
//
//	1 pass:    0
//	2 passes:  10
//	3 passes:  1100  (11 + 2-bit value 00)
//	4 passes:  1101  (11 + 2-bit value 01)
//	5 passes:  1110  (11 + 2-bit value 10)
//	6-37:      1111 + 5-bit value (n-6)
//	38+:       1111 11111 + 7-bit value (n-37)
func encodeNumPasses(bw *bitWriter, n int) {
	if n <= 0 {
		return
	}

	switch {
	case n == 1:
		bw.WriteBit(0)

	case n == 2:
		bw.WriteBit(1)
		bw.WriteBit(0)

	case n >= 3 && n <= 5:
		// 11 + 2-bit value
		bw.WriteBit(1)
		bw.WriteBit(1)
		bw.WriteBits(uint32(n-3), 2)

	case n >= 6 && n <= 36:
		// 1111 + 5-bit value
		bw.WriteBit(1)
		bw.WriteBit(1)
		bw.WriteBits(3, 2) // 11 to signal continuation
		bw.WriteBits(uint32(n-6), 5)

	default: // n >= 37
		// 1111 11111 + 7-bit value
		bw.WriteBit(1)
		bw.WriteBit(1)
		bw.WriteBits(3, 2)  // 11 to signal continuation
		bw.WriteBits(31, 5) // 11111 to signal final tier
		bw.WriteBits(uint32(n-37), 7)
	}
}

// encodePassLengths encodes data lengths for the included passes and appends
// the raw pass data to bodyData. Returns the updated bodyData.
//
// Per ITU-T T.800 B.10.6, the length encoding uses the Lblock mechanism:
//   - Each code block maintains an Lblock value (initially 3)
//   - The total data length is encoded using Lblock + floor(log2(numPasses)) bits
//   - If the length doesn't fit, Lblock is incremented (signalled by writing
//     1-bits before the length). A 0-bit terminates the increment sequence.
//
// The Lblock increment is encoded as a comma code: write consecutive 1-bits
// for each Lblock increment, then a 0-bit to signal the end.
func (pe *PacketEncoder) encodePassLengths(
	hdrWriter *bitWriter,
	cb *EncoderCodeBlock,
	newPasses int,
	bodyData []byte,
) []byte {
	if cb.Block == nil || newPasses <= 0 {
		return bodyData
	}

	// Calculate the total data length for the new passes
	startPass := cb.PassesIncluded
	endPass := startPass + newPasses
	if endPass > len(cb.Block.Passes) {
		endPass = len(cb.Block.Passes)
	}

	totalLength := 0
	for i := startPass; i < endPass; i++ {
		totalLength += cb.Block.Passes[i].Length
	}

	// Determine how many bits we need: Lblock + floor(log2(numPasses))
	bitsForLength := cb.Lblock
	if newPasses > 1 {
		bitsForLength += ilog2(newPasses)
	}

	// Determine if we need to increment Lblock.
	// The length must fit in bitsForLength bits: totalLength < (1 << bitsForLength)
	increment := 0
	for bitsForLength+increment < 32 && totalLength >= (1<<uint(bitsForLength+increment)) {
		increment++
	}

	// Write the Lblock increment as a comma code:
	// 'increment' consecutive 1-bits followed by a 0-bit
	for i := 0; i < increment; i++ {
		hdrWriter.WriteBit(1)
	}
	hdrWriter.WriteBit(0)

	// Update Lblock
	cb.Lblock += increment

	// Recalculate actual bits for the length value
	actualBits := cb.Lblock
	if newPasses > 1 {
		actualBits += ilog2(newPasses)
	}

	// Write the length value
	hdrWriter.WriteBits(uint32(totalLength), actualBits)

	// Append pass data to body
	for i := startPass; i < endPass; i++ {
		bodyData = append(bodyData, cb.Block.Passes[i].Data...)
	}

	// Update passes included
	cb.PassesIncluded = endPass

	return bodyData
}

// EncodeEmptyPacket encodes an empty packet (no code block data).
// Per ITU-T T.800, this is a single 0-bit followed by byte alignment.
func (pe *PacketEncoder) EncodeEmptyPacket() []byte {
	bw := newBitWriterWithStuffing()
	bw.WriteBit(0)
	bw.ByteAlign()
	return bw.Flush()
}

// EncodePacketsLRCP encodes all packets for a tile in LRCP progression order.
// This is the most common progression order: Layer, Resolution, Component, Position.
//
// resolutions[component][resolution] provides the encoder subbands.
// layerPasses[layer][component][resolution][blockIdx] provides the number of new
// passes for each code block in each layer.
//
// Returns all encoded packets concatenated.
func (pe *PacketEncoder) EncodePacketsLRCP(
	resolutions [][]*EncoderResolution,
	layerPasses [][][]map[int]int,
) []byte {
	var result []byte

	numLayers := pe.header.NumLayers
	numComps := pe.header.NumComps

	// Determine the maximum number of resolutions across all components
	maxRes := 0
	for c := 0; c < numComps; c++ {
		if c < len(resolutions) && len(resolutions[c]) > maxRes {
			maxRes = len(resolutions[c])
		}
	}

	// LRCP progression: Layer, Resolution, Component, Position
	for l := 0; l < numLayers; l++ {
		for r := 0; r < maxRes; r++ {
			for c := 0; c < numComps; c++ {
				if c >= len(resolutions) || r >= len(resolutions[c]) {
					continue
				}
				res := resolutions[c][r]
				if res == nil {
					continue
				}

				// Get passes for this layer/component/resolution
				var passes map[int]int
				if l < len(layerPasses) &&
					c < len(layerPasses[l]) &&
					r < len(layerPasses[l][c]) {
					passes = layerPasses[l][c][r]
				}

				if passes == nil {
					passes = make(map[int]int)
				}

				pkt := pe.EncodePacket(l, r, c, 0, res.Subbands, passes)
				result = append(result, pkt...)
			}
		}
	}

	return result
}

// EncodePacketWithSOP encodes a packet preceded by a SOP (Start of Packet)
// marker. Per ITU-T T.800, the SOP marker has the structure:
//
//	0xFF91 (2 bytes) + Lsop=0x0004 (2 bytes) + Nsop (2 bytes)
//
// where Nsop is the packet sequence number (mod 65536).
func (pe *PacketEncoder) EncodePacketWithSOP(
	layer, resolution, component, precinct int,
	subbands []*EncoderSubband,
	passesPerBlock map[int]int,
	packetSeqNum int,
) []byte {
	// SOP marker: FF 91 00 04 Nsop(2 bytes)
	sop := []byte{
		0xFF, 0x91, // SOP marker
		0x00, 0x04, // Lsop = 4
		byte((packetSeqNum >> 8) & 0xFF), // Nsop high byte
		byte(packetSeqNum & 0xFF),        // Nsop low byte
	}

	pkt := pe.EncodePacket(layer, resolution, component, precinct, subbands, passesPerBlock)

	result := make([]byte, 0, len(sop)+len(pkt))
	result = append(result, sop...)
	result = append(result, pkt...)

	return result
}

// EncodePacketWithEPH encodes a packet with an EPH (End of Packet Header)
// marker inserted between the packet header and body.
// Per ITU-T T.800, the EPH marker is: 0xFF92 (2 bytes).
func (pe *PacketEncoder) EncodePacketWithEPH(
	layer, resolution, component, precinct int,
	subbands []*EncoderSubband,
	passesPerBlock map[int]int,
) []byte {
	// Determine if the packet is empty
	isEmpty := true
	{
		sbOff := 0
		for _, sb := range subbands {
			for y := 0; y < sb.CodeBlocksY; y++ {
				for x := 0; x < sb.CodeBlocksX; x++ {
					idx := sbOff + y*sb.CodeBlocksX + x
					if newPasses, ok := passesPerBlock[idx]; ok && newPasses > 0 {
						isEmpty = false
						break
					}
				}
				if !isEmpty {
					break
				}
			}
			if !isEmpty {
				break
			}
			sbOff += sb.CodeBlocksX * sb.CodeBlocksY
		}
	}

	hdrWriter := newBitWriterWithStuffing()

	if isEmpty {
		hdrWriter.WriteBit(0)
		hdrWriter.ByteAlign()
		headerBytes := hdrWriter.Flush()

		// EPH marker after header
		eph := []byte{0xFF, 0x92}

		result := make([]byte, 0, len(headerBytes)+len(eph))
		result = append(result, headerBytes...)
		result = append(result, eph...)
		return result
	}

	// Non-empty packet
	hdrWriter.WriteBit(1)

	var bodyData []byte
	subbandOffset := 0
	for _, sb := range subbands {
		bodyData = pe.encodeSubbandPacketHeader(
			hdrWriter, sb, layer, subbandOffset, passesPerBlock, bodyData,
		)
		subbandOffset += sb.CodeBlocksX * sb.CodeBlocksY
	}

	hdrWriter.ByteAlign()
	headerBytes := hdrWriter.Flush()

	// EPH marker between header and body
	eph := []byte{0xFF, 0x92}

	result := make([]byte, 0, len(headerBytes)+len(eph)+len(bodyData))
	result = append(result, headerBytes...)
	result = append(result, eph...)
	result = append(result, bodyData...)

	return result
}

// ResetSubbandTagTreeStates resets the tag tree encoding states for all
// provided subbands. This should be called when re-encoding packets
// (e.g., for rate-distortion optimization) to ensure consistent state.
// Node values are preserved; only the incremental encoding states are cleared.
func ResetSubbandTagTreeStates(subbands []*EncoderSubband) {
	for _, sb := range subbands {
		if sb.InclusionTree != nil {
			sb.InclusionTree.resetStates()
		}
		if sb.ZBPTree != nil {
			sb.ZBPTree.resetStates()
		}
	}
}

// ResetEncoderCodeBlocks resets the per-layer state of all code blocks
// in the provided subbands. This is useful when re-encoding with different
// layer assignments.
func ResetEncoderCodeBlocks(subbands []*EncoderSubband) {
	for _, sb := range subbands {
		for y := 0; y < sb.CodeBlocksY; y++ {
			for x := 0; x < sb.CodeBlocksX; x++ {
				cb := sb.CodeBlocks[y][x]
				if cb == nil {
					continue
				}
				cb.PassesIncluded = 0
				cb.FirstLayer = -1
				cb.Lblock = 3
			}
		}
	}
}

// ComputeLayerPasses distributes coding passes across quality layers using
// a simple equal-pass distribution strategy. For more sophisticated
// rate-distortion optimization, the caller should compute passes per layer
// using the distortion values from EncodedPass.
//
// This function assigns all passes of a code block to a single layer
// (layer 0). For multi-layer encoding, use rate-distortion optimization
// to determine optimal truncation points.
func ComputeLayerPasses(
	subbands []*EncoderSubband,
	numLayers int,
) []map[int]int {
	layers := make([]map[int]int, numLayers)
	for l := range layers {
		layers[l] = make(map[int]int)
	}

	if numLayers == 0 {
		return layers
	}

	// Simple strategy: put all passes in layer 0.
	// Use subband offset to avoid collisions between subbands.
	sbOff := 0
	for _, sb := range subbands {
		for y := 0; y < sb.CodeBlocksY; y++ {
			for x := 0; x < sb.CodeBlocksX; x++ {
				cb := sb.CodeBlocks[y][x]
				if cb == nil || cb.Block == nil || cb.Block.NumPasses == 0 {
					continue
				}
				idx := sbOff + y*sb.CodeBlocksX + x
				layers[0][idx] = cb.Block.NumPasses
				cb.FirstLayer = 0
			}
		}
		sbOff += sb.CodeBlocksX * sb.CodeBlocksY
	}

	return layers
}

// ComputeLayerPassesRD distributes coding passes across quality layers using
// rate-distortion optimization. Passes are assigned to layers based on their
// distortion reduction per byte (slope), with higher-slope passes going to
// earlier layers.
//
// targetRates[layer] specifies the target bytes for each layer (cumulative).
// A targetRate of 0 means include all remaining data.
//
// Returns passesPerBlock[layer][blockIdx] = number of new passes for that layer.
func ComputeLayerPassesRD(
	subbands []*EncoderSubband,
	numLayers int,
	targetRates []int,
) []map[int]int {
	layers := make([]map[int]int, numLayers)
	for l := range layers {
		layers[l] = make(map[int]int)
	}

	if numLayers == 0 {
		return layers
	}

	// For single layer or no rate targets, put everything in layer 0
	if numLayers == 1 || len(targetRates) == 0 {
		return ComputeLayerPasses(subbands, numLayers)
	}

	// Collect all passes with their slopes (distortion/length)
	type passInfo struct {
		sb       *EncoderSubband
		cb       *EncoderCodeBlock
		blockIdx int
		passIdx  int
		slope    float64
		length   int
	}

	var allPasses []passInfo
	sbOff := 0
	for _, sb := range subbands {
		for y := 0; y < sb.CodeBlocksY; y++ {
			for x := 0; x < sb.CodeBlocksX; x++ {
				cb := sb.CodeBlocks[y][x]
				if cb == nil || cb.Block == nil {
					continue
				}
				idx := sbOff + y*sb.CodeBlocksX + x
				for p, pass := range cb.Block.Passes {
					slope := 0.0
					if pass.Length > 0 {
						slope = pass.Distortion / float64(pass.Length)
					} else if pass.Distortion > 0 {
						slope = pass.Distortion * 1e6 // Zero-length passes have infinite slope
					}
					allPasses = append(allPasses, passInfo{
						sb:       sb,
						cb:       cb,
						blockIdx: idx,
						passIdx:  p,
						slope:    slope,
						length:   pass.Length,
					})
				}
			}
		}
		sbOff += sb.CodeBlocksX * sb.CodeBlocksY
	}

	// Sort by slope descending (highest quality improvement per byte first)
	// Use insertion sort for stability (passes within a block stay ordered)
	for i := 1; i < len(allPasses); i++ {
		for j := i; j > 0 && allPasses[j].slope > allPasses[j-1].slope; j-- {
			allPasses[j], allPasses[j-1] = allPasses[j-1], allPasses[j]
		}
	}

	// Assign passes to layers based on cumulative rate targets
	// Track cumulative bytes per block to determine which layer each pass goes to
	blockPassCount := make(map[int]int) // blockIdx -> passes assigned so far

	cumulativeBytes := 0
	currentLayer := 0

	for _, pi := range allPasses {
		// Move to next layer if we've exceeded the target rate
		for currentLayer < numLayers-1 &&
			currentLayer < len(targetRates) &&
			targetRates[currentLayer] > 0 &&
			cumulativeBytes >= targetRates[currentLayer] {
			currentLayer++
		}

		// Verify pass ordering: this pass must follow all previously assigned passes
		expectedPass := blockPassCount[pi.blockIdx]
		if pi.passIdx != expectedPass {
			continue // Skip out-of-order passes
		}

		// Assign this pass to the current layer
		layers[currentLayer][pi.blockIdx]++
		blockPassCount[pi.blockIdx]++

		// Set FirstLayer if this is the first pass for this block
		if pi.cb.FirstLayer < 0 {
			pi.cb.FirstLayer = currentLayer
		}

		cumulativeBytes += pi.length
	}

	return layers
}
