package com.intel.cg.hpfd.madisonbay.wm.switchwm

trait PipelineStage[I,O] {

  def process: I => O

}
