//scalastyle:off
package madisonbay.wm.switchwm.parser

import java.io.File

import madisonbay.wm.switchwm.csr.Csr
// import madisonbay.wm.switchwm.ppe.parser.Parser
// import madisonbay.wm.switchwm.ppe.parser.output.PacketFlags.TypicalPacketFlags
// import madisonbay.wm.switchwm.epl.Packet
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

  val csr = Csr().getRxPpe(0).ppeRxMap.parser

  // load a 'canonical' image (tbd, how we're going to get this)

  // for each of some packets, confirm that they are aligned with the expected parse output
  // https://en.wikipedia.org/wiki/IPv4_header_checksum
  val pcapFile: File = new File("src/test/resources/scapy.pcap")
  s"$pcapFile" should s" have an UDP header" in {
    /*val pkt = Packet.loadPcap(pcapFile)(0)
    val pout = Parser.parse(csr, pkt) // (crashes without correct image)
    // can't actually do this assertion yet (no parser image)
    pout.paFlags.get should contain (TypicalPacketFlags.otr_l4_udp_v.id)*/
  }
}
