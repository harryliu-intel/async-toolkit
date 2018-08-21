package switch_wm

import switch_wm.ppe.PortIndex

package object epl {

  case class EplOutput(
                      rxData : Packet,
                      rxPort : PortIndex,
                      pktMeta : Int,
                      )

}
