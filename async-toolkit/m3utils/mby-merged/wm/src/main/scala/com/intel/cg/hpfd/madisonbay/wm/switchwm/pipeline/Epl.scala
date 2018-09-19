package com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline

import com.intel.cg.hpfd.madisonbay.wm.util.Packet


class Epl extends PipelineStage[Array[Byte], Packet] {
  val x: Array[Byte] => Packet = bits => new Packet(bits)
}
