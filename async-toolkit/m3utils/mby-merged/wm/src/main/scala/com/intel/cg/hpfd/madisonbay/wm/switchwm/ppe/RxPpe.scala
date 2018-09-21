package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe

import com.intel.cg.hpfd.csr.generated.mby_ppe_rx_top_map
import com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline.{Epl, HeaderExtraction, PipelineStage}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.ParserOutput


class RxPpe(csr: mby_ppe_rx_top_map) extends PipelineStage[Array[Byte], ParserOutput] {

  val epl = new Epl

  val headerExtractor = new HeaderExtraction

  val parser = new Parser(csr.parser)
  // val mapper = new KeyMapper(csr.mapper)

  // how do we provide the 'port' here?
  val x: Array[Byte] => ParserOutput = epl.x andThen parser.x

}
