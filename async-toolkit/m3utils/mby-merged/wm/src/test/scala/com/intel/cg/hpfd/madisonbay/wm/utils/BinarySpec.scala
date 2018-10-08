package com.intel.cg.hpfd.madisonbay.wm.utils

import org.scalatest._
import Binary.BinaryInterpolator

class BinarySpec extends FlatSpec with Matchers {

  "Binary " should " convert binary number" in {
    b"1110" shouldEqual 14
  }

}
