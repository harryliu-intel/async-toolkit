package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.Epl
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.Port


object RxPpe {

  def process(csr: Csr, port: Port, idMgp: Int, packetBytes: Array[Byte]): Array[Byte] = {
    val packet = Epl.process(packetBytes)
    val parserOutput = Parser.parse(csr.getParser(idMgp), packet, port)
    val csrAfterParse = csr.updated(parserOutput.updatedParserCsr)
    csrAfterParse.topMap
    packetBytes
  }

}
