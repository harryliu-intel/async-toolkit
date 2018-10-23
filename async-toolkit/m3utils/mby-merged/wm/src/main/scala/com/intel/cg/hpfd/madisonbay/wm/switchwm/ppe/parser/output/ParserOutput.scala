package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output

import com.intel.cg.hpfd.csr.generated.mby_ppe_parser_map
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.PacketFields
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ProtoOffsets
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.ParserExceptions.ParserException
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.{EplRxFlags, PortIndex}

case class ParserOutput(updatedCsr: mby_ppe_parser_map.mby_ppe_parser_map,
                        rxPort: PortIndex,
                        pktMeta: Int,
                        rxFlags: EplRxFlags,
                        segMetaErr: Boolean,
                        paAdjSegLegLen: Int,
                        paKeys: PacketFields, // should be 'option' values?
                        paKeysValid: Boolean,
                        paFlags: PacketFlags,
                        paPointers: ProtoOffsets,
                        paPointersValid: Boolean,
                        paCsumOk: Boolean,
                        paParseException: Option[ParserException],
                        paDrop: Boolean,
                        paPacketType: Int
                      ) {
  def simplifiedString: String =
    s"""ParserOutput(updatedCsr=..., rxPort=${rxPort.p}, pktMeta=$pktMeta, rxFlags=$rxFlags, segMetaErr=$segMetaErr, paAdjSegLegLen=$paAdjSegLegLen,
       | paKeys=${paKeys.fields},
       | paKeysValid=$paKeysValid, paFlags=${paFlags.get}, paPointers=$paPointers, paPointersValid=$paPointersValid, paCsumOk=$paCsumOk,
       | paParseException=$paParseException, paDrop=$paDrop, paPacketType=$paPacketType)
     """.stripMargin
}


