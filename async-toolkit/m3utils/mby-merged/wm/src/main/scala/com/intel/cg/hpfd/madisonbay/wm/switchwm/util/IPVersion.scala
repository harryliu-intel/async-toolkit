package com.intel.cg.hpfd.madisonbay.wm.switchwm.util

object IPVersion extends Enumeration  {

  val IpV4Int = 0x4
  val IpV6Int = 0x6

  val IPV4: Value = Value(IpV4Int, "IPv4")
  val IPV6: Value = Value(IpV6Int, "IPv4")

}
