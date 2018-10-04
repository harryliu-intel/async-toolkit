package com.intel.cg.hpfd.madisonbay.wm.utils

import org.scalatest._

class BinarySpec extends FlatSpec with Matchers {

  "Binary " should " convert binary number" in {
    Binary("1110") shouldEqual 14
  }

}
