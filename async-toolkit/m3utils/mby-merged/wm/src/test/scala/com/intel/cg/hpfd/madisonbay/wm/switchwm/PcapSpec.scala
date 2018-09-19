package com.intel.cg.hpfd.madisonbay.wm.switchwm

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

import com.intel.cg.hpfd.madisonbay.wm.util.Packet

/**
  * Test the 'pcap' parsing capability.
  *
  * @see https://wiki.wireshark.org/Development/LibpcapFileFormat
  *
  * @todo Dynamically generate pcap files via a tool like 'scapy'
  */
class PcapSpec extends FlatSpec with Matchers {
  val pcapFile : File = new File("src/test/data/scapy.pcap")
  s"The file ${pcapFile}" should s"have one packet in it" in {
    println(System.getenv("PWD"))
    Packet.loadPcap(pcapFile).length shouldEqual 1
  }
}
