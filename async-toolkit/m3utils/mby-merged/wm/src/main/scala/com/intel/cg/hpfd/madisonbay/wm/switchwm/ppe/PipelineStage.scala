package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe

abstract class PipelineStage[I,O] {
  val x: I => O
}
