package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.internal

import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.{IPv4Utils, Packet, PacketHeader}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrParser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.PacketHeader.IPv4CorrectCheckSum
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ProtocolsOffsets
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ProtocolsOffsets.Pointer.PointerNumber
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.Port
import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.UIntegers._

case class ParserValidation(checkSumOk: Option[IPv4CorrectCheckSum], drop: Boolean)

//scalastyle:off magic.number
object ParserValidation {

  def validatePayload(csrParser: CsrParser, port: Port, packet: Packet, header: PacketHeader,
                      protocolsOffsets: ProtocolsOffsets): ParserValidation = {

    val csumCfg = csrParser.ppeParserMap.PARSER_CSUM_CFG(port.index)

    val cSumOpt = if (csumCfg.VALIDATE_IPV4_HDR_CSUM() == 0) {
        None
      } else {
        val rxLength = packet.length
        Some(IPv4CorrectCheckSum(
          outerHeaderBit0 = checkSum(2, protocolsOffsets, rxLength, header),
          innerHeaderBit1 = checkSum(6, protocolsOffsets, rxLength, header)
          ))
      }

    ParserValidation(
      cSumOpt,
      false
    )
  }

  private def checkSum(pointer: PointerNumber, protoOffsets: ProtocolsOffsets, rxLength: Int, header: PacketHeader): Boolean = {
    if (protoOffsets.validPointer(pointer) && protoOffsets.validPointer(pointer + 1)) {
      val offset0 = getLower8(protoOffsets(pointer).offset)
      val offset1 = getLower8(protoOffsets(pointer + 1).offset - 1)
      val shiftedHeader = header.bytes.drop(offset0)
      (offset1 - offset0 <= 64) && (offset1 + 1 <= rxLength) && getLower16(IPv4Utils.checksum(shiftedHeader)) > 0
    } else {
      false
    }
  }

}
