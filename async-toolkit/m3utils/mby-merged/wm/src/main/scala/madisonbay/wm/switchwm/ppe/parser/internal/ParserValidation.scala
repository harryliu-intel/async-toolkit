package madisonbay.wm.switchwm.ppe.parser.internal

import madisonbay.wm.switchwm.epl.{IPv4Utils, Packet, PacketHeader}
import madisonbay.wm.switchwm.csr.Csr.CsrParser
import madisonbay.wm.switchwm.epl.PacketHeader.IPv4CorrectCheckSum
import madisonbay.wm.switchwm.ppe.parser.output.{CheckSums, ProtocolsOffsets}
import madisonbay.wm.switchwm.ppe.parser.output.ProtocolsOffsets.Pointer.PointerNumber
import madisonbay.wm.switchwm.ppe.ppe.Port
import madisonbay.wm.utils.extensions.UIntegers._
import madisonbay.wm.utils.extensions.ExtOption.ifThenOpt


//scalastyle:off magic.number
object ParserValidation {

  def validatePayload(csrParser: CsrParser, port: Port, packet: Packet, header: PacketHeader,
                      protocolsOffsets: ProtocolsOffsets): CheckSums = {

    val csumCfg = csrParser.ppeParserMap.PARSER_CSUM_CFG(port.index)

    val cSumOpt = ifThenOpt(csumCfg.VALIDATE_IPV4_HDR_CSUM() == 0) {
        val rxLength = packet.length
        IPv4CorrectCheckSum(
          outerHeaderBit0 = checkSum(2, protocolsOffsets, rxLength, header),
          innerHeaderBit1 = checkSum(6, protocolsOffsets, rxLength, header)
        )
      }

    CheckSums(
      cSumOpt,
      false
    )
  }

  private def checkSum(pointer: PointerNumber, protosOffsets: ProtocolsOffsets, rxLength: Int, header: PacketHeader): Boolean = {
    if (protosOffsets.validPointer(pointer) && protosOffsets.validPointer(pointer + 1)) {
      val offset0 = getLower8(protosOffsets(pointer).offset)
      val offset1 = getLower8(protosOffsets(pointer + 1).offset - 1)
      val shiftedHeader = header.bytes.drop(offset0)
      (offset1 - offset0 <= 64) && (offset1 + 1 <= rxLength) && getLower16(IPv4Utils.checksum(shiftedHeader)) > 0
    } else {
      false
    }
  }

}
