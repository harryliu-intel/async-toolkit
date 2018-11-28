package madisonbay.wm.switchwm.epl

import madisonbay.wm.switchwm.ppe.parser.defs.PacketFlags._
import madisonbay.wm.utils.BitFlags
import org.scalatest._

import scala.collection.BitSet

//scalastyle:off
class PacketFlagsSpec extends FlatSpec with Matchers with Inspectors {
  val pckFlags = BitFlags(BitSet(4,5,8,31)).get
  val pckFlagObjs = List(Flag_otr_l4_udp_v, Flag_otr_l4_tcp_v, Flag_GeneralFlags, Flag_inr_l4_v)

  "Packet Flags" should "properly translate flags IDs" in {
    forEvery (pckFlags zip pckFlagObjs) { case (flagId, flagObj) =>
      getFlag(flagId) should contain (flagObj)
    }
  }

  it should "properly find flags by IDs" in {
    val notFound = (pckFlags zip pckFlagObjs) filter { case (flagId, flagObj) =>
      !getConstantIndex(flagObj).contains(flagId)
    }
    notFound should contain only ((8, Flag_GeneralFlags))
  }

  it should "get flag IDs from Generic Flags" in {
    Flag_otr_l4_udp_v.index should contain (4)
  }

}
