package com.intel.cg.hpfd.madisonbay.wm.switchwm.epl

import com.intel.cg.hpfd.madisonbay.wm.switchwm.PipelineStage

/**
  * Ethernet Port Logic
  */
object Epl extends PipelineStage[Array[Byte], Packet]{

  def process: Array[Byte] => Packet = bits => Packet(bits)

}
