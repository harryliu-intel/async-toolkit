package madisonbay.wm.switchwm.ppe.mapper.internal

import madisonbay.wm.switchwm.ppe.mapper.Mapper.{MapPortDefault, MapPortDefaultEntry}
import madisonbay.wm.switchwm.ppe.mapper.output.ActionPrecVal
import madisonbay.wm.switchwm.ppe.ppe.Port
import madisonbay.wm.utils.extensions.UIntegers

object ClassifierActionsFiller {
  private val act24Lower16DefaultRange = 96  to 111
  private val act24Upper8DefaultRange  = 112 to 127

  def getAct24(defaults: MapPortDefault, rxPort: Port, index: Int): ActionPrecVal = ActionPrecVal(1,
    // TODO handle MBY_CGRP_ACTION_POLICER0 and MBY_CGRP_ACTION_POLICER1
    // TODO act24 will probably become a map...
    {
      val lowerIndex = act24Lower16DefaultRange.head + index
      val upperIndex = act24Upper8DefaultRange.head + index

      val maybeDefault = for (
        lower <- defaults.forRxPort(rxPort.index).collectFirst{ case MapPortDefaultEntry(value, `lowerIndex`) => value};
        higher <- defaults.forRxPort(rxPort.index).collectFirst{ case MapPortDefaultEntry(value, `upperIndex`) => value}
      ) yield (UIntegers.toInt(higher) << 16) | UIntegers.toInt(lower)

      maybeDefault.getOrElse(0)
    })
}
