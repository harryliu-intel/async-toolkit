package com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.PortIndex

case class EplOutput(rxData: Packet,
                     rxPort: PortIndex,
                     pktMeta: Int)
