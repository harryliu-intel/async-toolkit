package madisonbay.wm.switchwm.ppe.mapper.internal

import madisonbay.csr.all._

object Registers {
  object MacToMap extends Enumeration {
    val OuterDestinationMac = Value(1)
    val OuterSourceMac      = Value(2)
    val InnerDestinationMac = Value(4)
    val InnerSourceMac      = Value(8)
  }

  case class MapMacEntry(isMacRoutable: Boolean, mappedMac: Byte, valid: MacToMap.Value, ignoreLength: Byte, mac: Long)
  class MappingRegisters(csrs: mby_ppe_mapper_map) {
  def getMacMappings(macType: MacToMap.Value): Seq[MapMacEntry] = {
    csrs.MAP_MAC.filter(_.VALID().toInt == macType.id).map(regEntry =>
      MapMacEntry(regEntry.MAC_ROUTABLE() == 1, regEntry.MAP_MAC().toByte, MacToMap(regEntry.VALID().toInt),
        regEntry.IGNORE_LENGTH().toByte, regEntry.MAC()))
  }
}
}
