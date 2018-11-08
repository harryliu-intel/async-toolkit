package com.intel.cg.hpfd.madisonbay.wm.switchwm.epl

object IPVersion  {

  val IPv4Int = 0x4
  val IPv6Int = 0x6

  sealed trait IPVersion
  case object IPv4    extends IPVersion
  case object IPv6    extends IPVersion

}
