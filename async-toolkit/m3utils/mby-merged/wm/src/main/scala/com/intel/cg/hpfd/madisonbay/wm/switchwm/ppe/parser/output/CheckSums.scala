package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output

import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.PacketHeader.IPv4CorrectCheckSum

case class CheckSums(checkSumOk: Option[IPv4CorrectCheckSum], drop: Boolean)
