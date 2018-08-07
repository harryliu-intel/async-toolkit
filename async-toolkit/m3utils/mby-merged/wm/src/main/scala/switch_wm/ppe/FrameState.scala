package switch_wm.ppe




class FrameState (val egressVidCfg : VID,
                  val cgrp : CGRP,
                   val markRouted : Boolean,
                   val fclass : PacketClass.Value,
                  val rxPort : PortIndex,
                  val destPorts : Set[PortIndex],
                  val dglort : GLORT,
                 val l3EgressDomain : L3Domain,
                  val l2EgressDomain : L2Domain,
                  val actionMask : Set[ActionFlag.Value]

                 )
{


}
