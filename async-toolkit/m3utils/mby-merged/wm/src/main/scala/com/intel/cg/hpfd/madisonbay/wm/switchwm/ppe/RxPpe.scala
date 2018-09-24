package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe

import com.intel.cg.hpfd.csr.generated.mby_ppe_rx_top_map
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.Epl
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.{HeaderExtraction, Parser, ParserOutput}


// TODO: split pipeline from different abstract layers

class RxPpe(csr: mby_ppe_rx_top_map) extends PipelineStage[Array[Byte], ParserOutput] {

  val headerExtractor = new HeaderExtraction

  val parser = new Parser(csr.parser)
  // val mapper = new KeyMapper(csr.mapper)

  // how do we provide the 'port' here?
  val x: Array[Byte] => ParserOutput = Epl.x andThen parser.x

}
