package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import com.intel.cg.hpfd.madisonbay.wm.switchwm.PipelineStage
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.{Packet, PacketHeader}

// from parser to multiple stages; parser output

object HeaderExtraction {

  val PacketHeaderOffset = 80

}

class HeaderExtraction extends PipelineStage[Packet, PacketHeader] {

  def process: Packet => PacketHeader = packet =>  new PacketHeader(packet.bytes.slice(0, HeaderExtraction.PacketHeaderOffset))

}
