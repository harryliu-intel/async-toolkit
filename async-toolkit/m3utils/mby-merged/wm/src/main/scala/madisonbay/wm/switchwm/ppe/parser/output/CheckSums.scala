package madisonbay.wm.switchwm.ppe.parser.output

import madisonbay.wm.switchwm.epl.PacketHeader.IPv4CorrectCheckSum

case class CheckSums(checkSumOk: Option[IPv4CorrectCheckSum], drop: Boolean)
