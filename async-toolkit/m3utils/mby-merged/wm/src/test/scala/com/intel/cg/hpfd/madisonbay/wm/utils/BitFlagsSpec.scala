package com.intel.cg.hpfd.madisonbay.wm.utils

import org.scalatest.{FlatSpec, Matchers}
import com.intel.cg.hpfd.madisonbay.wm.utils.Binary.BinaryInterpolator

import scala.collection.BitSet

//scalastyle:off
class BitFlagsSpec extends FlatSpec with Matchers {

  "Bit Flags" should "Provide 0L on lack of flags" in {
    BitFlags().toLong shouldEqual 0L
  }

  it should "fit proper flags" in {
    BitFlags().set(1).set(3).set(7).toLong.toInt shouldEqual b"10001010"
  }

  it should "fit proper flags with BitSet constructor" in {
    BitFlags(BitSet(4, 5, 8)).toInt shouldEqual b"100110000"
  }

  it should "clear flags" in {
    BitFlags(BitSet(4, 5, 8)).clear(8).toInt shouldEqual b"00110000"
  }

  it should "assign flags" in {
    BitFlags(BitSet(4, 5, 8)).assign(8, false).assign(1, true).toInt shouldEqual b"00110010"
  }

}