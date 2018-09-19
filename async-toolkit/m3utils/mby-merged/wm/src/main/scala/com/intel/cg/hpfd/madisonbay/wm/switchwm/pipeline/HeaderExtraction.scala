package com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline

import com.intel.cg.hpfd.madisonbay.wm.util.Packet


class HeaderExtraction extends PipelineStage[Packet, PacketHeader] {
  val x: Packet => PacketHeader = p =>  new PacketHeader(p.bytes.slice(0,80))
}
