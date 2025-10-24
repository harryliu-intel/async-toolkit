// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.wm.switchwm.ppe.parser.internal

import madisonbay.wm.switchwm.csr.Csr.CsrParser
import madisonbay.wm.switchwm.ppe.parser.internal.KeysExtractor.ExtractionIndex
import madisonbay.wm.utils.BitFlags
import madisonbay.wm.utils.extensions.UIntegers.getLower32

object PacketType {

  type PacketType = Int

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
