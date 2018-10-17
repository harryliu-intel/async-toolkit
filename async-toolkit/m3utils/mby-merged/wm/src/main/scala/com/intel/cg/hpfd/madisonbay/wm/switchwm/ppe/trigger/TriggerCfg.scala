package com.intel.cgr.hpfd.madisonbay.wm.switchwm.ppe.trigger

import com.intel.cg.hpfd.csr.generated.{mby_ppe_trig_apply_map, mby_ppe_trig_apply_misc_map, mby_ppe_trig_usage_map}

/**
  * Abstract trigger CSRs into a single logic operation, so as to avoid requirement
  * the author know precisely where a given CSR occurs
  *
  * @param apply_map
  * @param apply_misc_map
  * @param usage_map
  */
class TriggerCfg(val apply_map: mby_ppe_trig_apply_map.mby_ppe_trig_apply_map,
                 val apply_misc_map: mby_ppe_trig_apply_misc_map.mby_ppe_trig_apply_misc_map,
                 val usage_map: mby_ppe_trig_usage_map.mby_ppe_trig_usage_map) {
  // how can I express an apply method such that
  // tcfg(1) returns a type that can be implicitly converted into _all_ of the
  // individual registers at the given index within the several maps?
}

object TriggerCfg {
  implicit def tcfgToApplyMap(x: TriggerCfg): mby_ppe_trig_apply_map.mby_ppe_trig_apply_map = x.apply_map
  implicit def tcfgToApplyMiscMap(x: TriggerCfg): mby_ppe_trig_apply_misc_map.mby_ppe_trig_apply_misc_map = x.apply_misc_map
  implicit def tcfgToUsageMap(x: TriggerCfg): mby_ppe_trig_usage_map.mby_ppe_trig_usage_map = x.usage_map
}
