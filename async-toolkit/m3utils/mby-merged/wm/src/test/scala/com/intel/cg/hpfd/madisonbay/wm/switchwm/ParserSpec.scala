//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.switchwm

import java.io.File

import com.intel.cg.hpfd.csr.generated.mby_ppe_parser_map
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.Parser
import com.intel.cg.hpfd.madisonbay.wm.util.Packet

import org.scalatest.{FlatSpec, Matchers}

/**
  * Demonstrate the operation of the parser against the architecturally intended use cases
  *
  * The approach this specification should take is to use a 'canonical' image, as determined
  * by DV or the SDK team. Then apply packets generated in 'pcap' format with some known characteristics.
  * Confirm those packets are properly handled
  *
  * Also, provide at least one case of a packet which the parser is incapable of handling (for example, too long a header)
  */
class ParserSpec extends FlatSpec with Matchers {

  val cfg = mby_ppe_parser_map()
  cfg.foreachResetableField(f => f.reset())
  val parser = new Parser(cfg)

  // load a 'canonical' image (tbd, how we're going to get this)

  // for each of some packets, confirm that they are aligned with the expected parse output
  // https://en.wikipedia.org/wiki/IPv4_header_checksum
  val pcapFile : File = new File("src/test/data/scapy.pcap")
  s"${pcapFile}" should s" have an UDP header" in {
    val pkt = Packet.loadPcap(pcapFile)(0)
    // val pout = parser.x(pkt) // (crashes without correct image)
    // can't actually do this assertion yet (no parser image)
    // pout.paFlags.flags should contain (TypicalPacketFlags.otr_l4_udp_v.id)
  }
}
