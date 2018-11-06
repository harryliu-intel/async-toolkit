package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrParser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ProtoOffsets
import ParserExceptions.ParserException
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.{EplRxFlags, Port}
import com.intel.cg.hpfd.madisonbay.wm.utils.BitFlags

case class ParserOutput(updatedParserCsr: CsrParser,
                        rxPort: Port,
                        pktMeta: Int,
                        rxFlags: EplRxFlags,
                        segMetaErr: Boolean,
                        paAdjSegLegLen: Int,
                        parserKeys: PacketFields,
                        parserKeysValid: Boolean,
                        parserFlags: BitFlags,
                        parserPointers: ProtoOffsets,
                        parserPointersValid: Boolean,
                        parserCsumOk: Boolean,
                        parserException: Option[ParserException],
                        drop: Boolean,
                        packetType: Int
                      ) {
  def simplifiedString: String =
    s"""ParserOutput(updatedParserCsr=..., rxPort=$rxPort, pktMeta=$pktMeta, rxFlags=$rxFlags, segMetaErr=$segMetaErr, paAdjSegLegLen=$paAdjSegLegLen,
       | parserKeys=$parserKeys,
       | parserKeysValid=$parserKeysValid, parserFlags=${parserFlags.get}, parserPointers=$parserPointers, parserPointersValid=$parserPointersValid,
       | parserCsumOk=$parserCsumOk, parserException=$parserException, drop=$drop, paPacketType=$packetType)
     """.stripMargin
}


