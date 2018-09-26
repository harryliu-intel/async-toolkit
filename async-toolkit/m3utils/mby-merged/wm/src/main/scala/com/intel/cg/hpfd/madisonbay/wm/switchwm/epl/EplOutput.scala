package com.intel.cg.hpfd.madisonbay.wm.switchwm.epl

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.PortIndex
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.Packet

case class EplOutput(rxData: Packet,
                     rxPort: PortIndex,
                     pktMeta: Int)
