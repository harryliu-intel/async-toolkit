package madisonbay.wm.switchwm

trait PipelineStage[I,O] {

  def process: I => O

}
