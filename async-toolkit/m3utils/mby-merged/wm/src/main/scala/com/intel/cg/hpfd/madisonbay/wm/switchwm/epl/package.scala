package com.intel.cg.hpfd.madisonbay.wm.switchwm

import com.intel.cg.hpfd.madisonbay.wm.switch_wm.Packet
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.PortIndex



package object epl {

  case class EplOutput(
                      rxData : Packet,
                      rxPort : PortIndex,
                      pktMeta : Int,
                      )

}
