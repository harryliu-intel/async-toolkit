//scalastyle:off
package madisonbay.wm.utils

import java.io.File

import madisonbay.wm.switchwm.util.Pcap
import org.scalatest.{FlatSpec, Matchers}

/**
  * Test the 'pcap' parsing capability.
  *
  * @see https://wiki.wireshark.org/Development/LibpcapFileFormat
  * @todo Dynamically generate pcap files via a tool like 'scapy'
  */
class PcapSpec extends FlatSpec with Matchers {

  val pcapFile: File = new File("src/test/resources/pcap/scapy.pcap")
  s"The file $pcapFile" should s"have one packet in it" in {
    println(System.getenv("PWD"))
    Pcap.loadPcap(pcapFile).length shouldEqual 1
  }

}
