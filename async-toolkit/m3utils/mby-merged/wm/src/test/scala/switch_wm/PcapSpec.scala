package switch_wm

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

/**
  * Validate 1's complement based checksum computation over arrays of bytes
  */
class PcapSpec extends FlatSpec with Matchers {
  // https://en.wikipedia.org/wiki/IPv4_header_checksum
  val pcapFile : File = new File("src/test/data/scapy.pcap")
  s"The file ${pcapFile}" should s"have one packet in it" in {
    println(System.getenv("PWD"))
    loadPcap(pcapFile).length shouldEqual 1
  }
}