package com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline


class Epl extends PipelineStage[Array[Byte], Packet] {

  val x: Array[Byte] => Packet = bits => new Packet(bits)

}
