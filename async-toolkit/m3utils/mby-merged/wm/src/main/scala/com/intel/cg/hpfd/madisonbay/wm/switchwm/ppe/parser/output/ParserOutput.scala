package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrParser
import ParserExceptions.ParserException
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.Port
import com.intel.cg.hpfd.madisonbay.wm.utils.BitFlags

case class ParserOutput(updatedParserCsr:       CsrParser,
                        rxPort:                 Port,
                        adjustedSegmentLength:  Int,
                        parserKeys:             PacketFields,
                        parserFlags:            BitFlags,
                        parserPointers:         ProtocolsOffsets,
                        parserException:        Option[ParserException],
                        packetType:             Int,
                        checkSums:              CheckSums
                      ) {
  def simplifiedString: String =
    s"""ParserOutput(updatedParserCsr=..., $rxPort, adjustedSegmentLength=$adjustedSegmentLength,
       | parserKeys=$parserKeys,
       | parserFlags=${parserFlags.get}, parserPointers=$parserPointers
       | parserException=$parserException, packetType=$packetType, $checkSums)
     """.stripMargin
}
