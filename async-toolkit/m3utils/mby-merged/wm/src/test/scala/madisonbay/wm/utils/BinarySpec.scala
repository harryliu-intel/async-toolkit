package madisonbay.wm.utils

import org.scalatest._
import Binary.BinaryInterpolator

class BinarySpec extends FlatSpec with Matchers {

  "Binary" should "convert binary number" in {
    b"1110" shouldEqual 14
  }

  it should "convert binary number from value" in {
    val sn = "1110"
    b"$sn" shouldEqual 14
  }

  it should "convert binary number from expression" in {
    val sn = "111"
    b"${sn + "0"}" shouldEqual 14
  }

}
