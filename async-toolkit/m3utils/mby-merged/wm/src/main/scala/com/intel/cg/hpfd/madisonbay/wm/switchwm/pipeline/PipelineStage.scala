package com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline


abstract class PipelineStage[I,O] {
  val x: I => O
}
