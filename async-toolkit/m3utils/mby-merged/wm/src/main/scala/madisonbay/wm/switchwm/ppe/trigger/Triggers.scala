package madisonbay.wm.switchwm.ppe.trigger

import madisonbay.csr.all._

class Triggers(val ppe_cfg: mby_ppe_rx_top_map) extends IndexedSeq[Trigger] {

  val tcfg = new TriggerCfg(ppe_cfg.trig_apply, ppe_cfg.trig_apply_misc, ppe_cfg.trig_usage)

  def apply(i: Int): Trigger = {
    new Trigger(tcfg, i)
  }

  def length: Int = tcfg.TRIGGER_ACTION_CFG_1.length // no way to get this more safely (assumes all of the trigger cfg registers are the same length

  def firingTriggers(fs: FrameState): Iterable[Trigger] = {
    val result = List.newBuilder[Trigger]
    var firedThisPrecedenceStage  = false
    (0 until length).map { i =>
      val trig = apply(i)
      if (tcfg.TRIGGER_CONDITION_CFG(i).MATCH_BY_PRECEDENCE() == 0) firedThisPrecedenceStage = false
      if (!firedThisPrecedenceStage && trig.c(fs)) result += trig
    }
    result.result()
  }

}
