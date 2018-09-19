package com.intel.cg.hpfd.madisonbay.wm.switchwm

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.PortIndex
import com.intel.cg.hpfd.madisonbay.wm.util.Packet



package object epl {

  case class EplOutput(
                      rxData : Packet,
                      rxPort : PortIndex,
                      pktMeta : Int
                      )

}
