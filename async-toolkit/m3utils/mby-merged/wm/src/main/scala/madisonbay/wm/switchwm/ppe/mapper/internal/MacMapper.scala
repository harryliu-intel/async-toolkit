package madisonbay.wm.switchwm.ppe.mapper.internal

import madisonbay.wm.switchwm.epl.MACAddress
import madisonbay.wm.switchwm.ppe.parser.output.PacketFields
import madisonbay.csr.all._

object MacMapper {

  //scalastyle:off magic.number
  implicit class MacMapperImposed(pckFields: PacketFields) {

    def innerDMAC: MACAddress = MACAddress(pckFields.key16(0), pckFields.key16(1), pckFields.key16(2))
    def innerSMAC: MACAddress = MACAddress(pckFields.key16(3), pckFields.key16(4), pckFields.key16(5))
    def outerDMAC: MACAddress = MACAddress(pckFields.key16(6), pckFields.key16(7), pckFields.key16(8))
    def outerSMAC: MACAddress = MACAddress(pckFields.key16(9), pckFields.key16(10), pckFields.key16(11))

  }
  //scalastyle:on

  case class MacLookupResult(index: Int, routeable: Boolean)
  case class MacMapperResult(outerDMAC: MacLookupResult,
                             outerSMAC: MacLookupResult,
                             innerDMAC: MacLookupResult,
                             innerSMAC: MacLookupResult)

  def matches(mac: MACAddress, macMapElem: map_mac_r): Boolean = {
    val mask = (1L << (macMapElem.IGNORE_LENGTH() + 1L)) - 1L
    macMapElem.MAC() == (mac.addr & mask)
  }

  def highestMatching(mac: MACAddress, mapperMap: mby_ppe_mapper_map): MacLookupResult =
    mapperMap.MAP_MAC.indices.reverse.collectFirst { case i if matches(mac, mapperMap.MAP_MAC(i)) =>
      MacLookupResult(i, mapperMap.MAP_MAC(i).MAC_ROUTABLE() == 1)
    } match {
      case Some(l) => l
      case None => MacLookupResult(0, routeable = false)
    }

  /*def x(fv: MACMapperImposed): MacMapperResult =
    MacMapperResult(highestMatching(fv.OuterDMAC), highestMatching(fv.OuterSMAC), highestMatching(fv.InnerDMAC),
    highestMatching(fv.OuterSMAC))*/

}
