package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.PipelineStage
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.{Packet, PacketHeader}

// from parser to multiple stages; parser output

object HeaderExtraction {

  val PacketHeaderOffset = 80

}

class HeaderExtraction extends PipelineStage[Packet, PacketHeader] {

  val x: Packet => PacketHeader = p =>  new PacketHeader(p.bytes.slice(0, HeaderExtraction.PacketHeaderOffset))

}
