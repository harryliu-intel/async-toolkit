package switch_wm

import java.io.File

import com.intel.cg.hpfd.csr.generated.mby_ppe_parser_map
import org.scalatest._
import switch_wm.ppe._

class ParserSpec extends FlatSpec with Matchers {

  val cfg = mby_ppe_parser_map()



  val parser = new Parser(cfg)

  // load a 'canonical' image (tbd, how we're going to get this)
  // for each of some packets, confirm that they are aligned with the expected parse output
  // https://en.wikipedia.org/wiki/IPv4_header_checksum
  val pcapFile : File = new File("src/test/data/scapy.pcap")
  s"${pcapFile}" should s" have an UDP header" in {
    val pkt = loadPcap(pcapFile)(0)
    val ph = new PacketHeader(pkt.bytes.slice(0, 79))
    val pout = parser.x(ph)
    // can't actually do this assertion yet (no parser image)
    // pout.paFlags.flags should contain (TypicalPacketFlags.otr_l4_udp_v.id)
  }
}
