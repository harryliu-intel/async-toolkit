package com.intel.cg.hpfd.madisonbay.wm.switchwm.epl

import com.intel.cg.hpfd.madisonbay.wm.switchwm.PipelineStage
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.Packet


object Epl extends PipelineStage[Array[Byte], Packet]{

  def process: Array[Byte] => Packet = bits => new Packet(bits)

}
