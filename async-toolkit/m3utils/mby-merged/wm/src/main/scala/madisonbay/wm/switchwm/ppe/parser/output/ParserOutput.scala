// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.wm.switchwm.ppe.parser.output

import madisonbay.wm.switchwm.csr.Csr.CsrParser
import ParserExceptions.ParserException
import madisonbay.wm.switchwm.ppe.ppe.Port
import madisonbay.wm.utils.BitFlags

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
