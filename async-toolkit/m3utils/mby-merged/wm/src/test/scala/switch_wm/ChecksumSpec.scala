package switch_wm

import org.scalatest._
import switch_wm._

/**
  * Validate 1's complement based checksum computation over arrays of bytes
  */
class ChecksumSpec extends FlatSpec with Matchers {
  // http://www.netfor2.com/checksum.html
  val csumExp1 = Seq(0x01,0x00,0xF2,0x03, 0xF4, 0xF5, 0xF6, 0xF7 ,0x00,0x00).map(_.toByte)
  val csumResult1 = 0x210e
  s"The checksum of ${csumExp1.map(_.hex)}" should s"be 0x${csumResult1.toHexString}" in {
    checksum(csumExp1) shouldEqual csumResult1
  }

  // https://en.wikipedia.org/wiki/IPv4_header_checksum
  val csumExp2 = Seq(0x45,0x00,0x00,0x73,0x00,0x00,0x40,0x00,0x40,0x11, 0xb8,0x61, 0xc0, 0xa8,0x00, 0x01, 0xc0, 0xa8,0x00,0xc7).map(_.toByte)
  s"The checksum of ${csumExp2.map(_.hex)}" should s"validate to 0" in {
    checksum(csumExp2) shouldEqual 0
  }
}
