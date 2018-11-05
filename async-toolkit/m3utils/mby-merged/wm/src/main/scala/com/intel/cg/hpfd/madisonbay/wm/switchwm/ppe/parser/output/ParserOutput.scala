package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrParser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.PacketFields
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ProtoOffsets
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.ParserExceptions.ParserException
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.{EplRxFlags, Port}
import com.intel.cg.hpfd.madisonbay.wm.utils.BitFlags

case class ParserOutput(updatedParserCsr: CsrParser,
                        rxPort: Port,
                        pktMeta: Int,
                        rxFlags: EplRxFlags,
                        segMetaErr: Boolean,
                        paAdjSegLegLen: Int,
                        paKeys: PacketFields, // should be 'option' values?
                        paKeysValid: Boolean,
                        paFlags: BitFlags,
                        paPointers: ProtoOffsets,
                        paPointersValid: Boolean,
                        paCsumOk: Boolean,
                        paParseException: Option[ParserException],
                        paDrop: Boolean,
                        paPacketType: Int
                      ) {
  def simplifiedString: String =
    s"""ParserOutput(updatedParserCsr=..., rxPort=$rxPort, pktMeta=$pktMeta, rxFlags=$rxFlags, segMetaErr=$segMetaErr, paAdjSegLegLen=$paAdjSegLegLen,
       | paKeys=$paKeys,
       | paKeysValid=$paKeysValid, paFlags=${paFlags.get}, paPointers=$paPointers, paPointersValid=$paPointersValid, paCsumOk=$paCsumOk,
       | paParseException=$paParseException, paDrop=$paDrop, paPacketType=$paPacketType)
     """.stripMargin
}


