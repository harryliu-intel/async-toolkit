package com.intel.cg.hpfd.madisonbay.wm.utils.extensions

import org.scalatest.{FlatSpec, Matchers}

//scalastyle:off magic.number
class ExtOptionSpec extends FlatSpec with Matchers {

  "Ext Option" should "apply ifThenOpt" in {
    ExtOption.ifThenOpt(true) { 5 } shouldEqual Some(5)
    ExtOption.ifThenOpt(false) { 5 } shouldEqual None
    ExtOption.ifThenOpt(false) { 5 } shouldEqual Option.empty[Int]
  }

}
