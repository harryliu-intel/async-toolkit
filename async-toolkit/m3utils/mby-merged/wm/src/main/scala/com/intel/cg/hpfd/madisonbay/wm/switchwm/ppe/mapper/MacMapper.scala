package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper

import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.MACAddress
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.Mapper.MacLookupResult

object MacMapper {

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
