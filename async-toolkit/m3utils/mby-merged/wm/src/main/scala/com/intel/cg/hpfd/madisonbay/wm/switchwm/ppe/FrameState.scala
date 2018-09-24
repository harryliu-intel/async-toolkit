package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe._


// TODO: obsolete - to be split into specified outputs
case class FrameState (egressVidCfg: VID,
                      cgrp: CGRP,
                      markRouted: Boolean,
                      fclass: PacketClass.Value,
                      tc: TrafficClass,
                      rxPort: PortIndex,
                      destPorts: Set[PortIndex],
                      dglort: GLORT,
                      l3EgressDomain: L3Domain,
                      l2EgressDomain: L2Domain,
                      actionMask: Set[ActionFlag.Value]
                      )
