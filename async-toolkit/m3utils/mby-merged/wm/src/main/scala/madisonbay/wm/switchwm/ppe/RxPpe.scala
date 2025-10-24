// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.wm.switchwm.ppe

import madisonbay.wm.switchwm.csr.Csr
import madisonbay.wm.switchwm.epl.Epl
import madisonbay.wm.switchwm.ppe.parser.Parser
import madisonbay.wm.switchwm.ppe.ppe.Port


object RxPpe {

  def process(csr: Csr, port: Port, idMgp: Int, packetBytes: Array[Byte]): Array[Byte] = {
    val packet = Epl.process(packetBytes)
    val parserOutput = Parser.parse(csr.getParser(0, idMgp), packet, port)
    val csrAfterParse = csr.updated(parserOutput.updatedParserCsr)
    csrAfterParse.topMap
    packetBytes
  }

}
