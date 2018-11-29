package madisonbay.wm.switchwm.epl

import madisonbay.wm.switchwm.ppe.ppe.Port

case class EplOutput(rxData: Packet,
                     rxPort: Port,
                     pktMeta: Int)
