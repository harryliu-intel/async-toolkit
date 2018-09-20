package com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline


object HeaderExtraction {

  val PacketHeaderOffset = 80

}

class HeaderExtraction extends PipelineStage[Packet, PacketHeader] {

  val x: Packet => PacketHeader = p =>  new PacketHeader(p.bytes.slice(0, HeaderExtraction.PacketHeaderOffset))

}
