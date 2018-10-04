package com.intel.cg.hpfd.madisonbay.wm.switchwm.csr

import com.intel.cg.hpfd.csr.generated.{mby_ppe_rx_top_map, mby_top_map}
import com.intel.cg.hpfd.madisonbay.Memory._

object Csr {

  val topMap: mby_top_map.mby_top_map = mby_top_map.mby_top_map(Address(0, 0 bits))

  val topRxPpe0: mby_ppe_rx_top_map.mby_ppe_rx_top_map = topMap.mpp.mgp(0).rx_ppe

}
