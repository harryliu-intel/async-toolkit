package madisonbay.wm.switchwm.ppe.trigger

import madisonbay.wm.switchwm.ppe.ppe._

// TODO: obsolete - to be split into specified outputs
case class FrameState(egressVidCfg: VID,
                      cgrp: CGRP,
                      markRouted: Boolean,
                      fclass: PacketClass.Value,
                      tc: TrafficClass,
                      rxPort: Port,
                      destPorts: Set[Port],
                      dglort: GLORT,
                      l3EgressDomain: L3Domain,
                      l2EgressDomain: L2Domain,
                      actionMask: Set[ActionFlag.Value]
                      )
