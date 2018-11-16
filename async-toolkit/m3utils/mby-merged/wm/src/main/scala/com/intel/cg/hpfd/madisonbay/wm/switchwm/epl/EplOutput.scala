package com.intel.cg.hpfd.madisonbay.wm.switchwm.epl

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.Port

case class EplOutput(rxData: Packet,
                     rxPort: Port,
                     pktMeta: Int)
