package com.intel.cg.hpfd.madisonbay.wm.switchwm.epl

import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.Packet


class Epl {

  val x: Array[Byte] => Packet = bits => new Packet(bits)

}
