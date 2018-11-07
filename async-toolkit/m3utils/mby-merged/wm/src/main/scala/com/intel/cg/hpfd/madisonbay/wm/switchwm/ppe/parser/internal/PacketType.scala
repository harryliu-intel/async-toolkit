package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.internal

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrParser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.{ExtractionIndex, PacketType}
import com.intel.cg.hpfd.madisonbay.wm.utils.BitFlags
import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.UIntegers.getLower32

object PacketType {

  def derive(csrParser: CsrParser, packetFlags: BitFlags): (PacketType, ExtractionIndex) = {
    val interface = 0
    val tcamCsr = csrParser.ppeParserMap.PARSER_PTYPE_TCAM(interface).PARSER_PTYPE_TCAM
    val sramCsr = csrParser.ppeParserMap.PARSER_PTYPE_RAM(interface).PARSER_PTYPE_RAM
    val flag = packetFlags.toInt

    tcamCsr.zip(sramCsr).reverse.collectFirst {
      case (x,y) if ParserTcam.matchPtype(x.KEY().toInt, x.KEY_INVERT().toInt, flag) =>
        (getLower32(y.PTYPE()).toInt, getLower32(y.EXTRACT_IDX()).toInt)
    }.getOrElse((0,0))
  }

}
