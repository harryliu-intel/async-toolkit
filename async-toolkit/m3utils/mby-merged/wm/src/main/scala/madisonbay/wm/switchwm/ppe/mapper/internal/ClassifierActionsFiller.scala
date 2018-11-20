package madisonbay.wm.switchwm.ppe.mapper.internal

import madisonbay.wm.switchwm.ppe.mapper.Mapper.{MapPortDefault, MapPortDefaultEntry}
import madisonbay.wm.switchwm.ppe.mapper.output.ActionPrecVal
import madisonbay.wm.switchwm.ppe.ppe.Port

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
      ) yield (higher << 16) | lower

      maybeDefault.getOrElse(0)
    })

  private val act4FourNibblesDefaultRange  = 128 to 159
  private val act4TwoNibblesDefaultRange   = 160 to 191
  private val act4SingleNibbleDefaultRange = 192 to 223

  def getAct4(defaults: MapPortDefault, rxPort: Port, index: Int): ActionPrecVal = ActionPrecVal(1, {
    // TODO do CGRP_ACTION_DSCP AND _VPRI
    def process(theirRange: Range, noOfNibblesSet: Int) = {
      // For act4, each MAP_PORT_DEFAULT entry may set four, two or one act4 (see below).
      // A four-nibble entry with target X (exactly X + 128 in case of four nibbles) will fill nibbles X, X+1, X+2, X+3,
      // so we make a range containing all targets that affect our nibble.
      val theseNibbleIndices = theirRange.head + index - noOfNibblesSet + 1 to theirRange.head + index
      val theseNibbleValue = defaults.forRxPort(rxPort.index).collect{case MapPortDefaultEntry(value, target)
        if theseNibbleIndices.contains(target) =>
        val nibbleIndex = index - target + theirRange.head
        val nibbleValue = (value >> nibbleIndex*4) & 0xF
        nibbleValue
      }
      theseNibbleValue
    }

    // We can set 4, 2 or 1 nibbles at once, and we have ranges of TARGET values for these,
    // so we get a lsit of whole 'set' instructions from the whole space for this particulat act4.
    val nibbleMultiplities = List(
      (act4FourNibblesDefaultRange,  4),
      (act4TwoNibblesDefaultRange,   2),
      (act4SingleNibbleDefaultRange, 1)).flatMap{case (range, multiplicity) => process(range, multiplicity)}

    // If there was no config we do not do anything, if there was one we use it,
    // and if there were multiple -  which one should we take? I don't know, so we fail
    nibbleMultiplities.size match {
      case 0 => 0
      case 1 => nibbleMultiplities.head
      case _ => throw new RuntimeException("Overconfigured - act4 set by more than one config")
    }
  })
}
