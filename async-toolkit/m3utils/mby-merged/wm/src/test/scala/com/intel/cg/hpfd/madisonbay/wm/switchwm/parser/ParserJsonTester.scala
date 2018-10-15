package com.intel.cg.hpfd.madisonbay.wm.switchwm.parser

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.Packet
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags
import com.intel.cg.hpfd.madisonbay.wm.utils.{Json, Loader}
import org.scalatest.{FlatSpec, Matchers}
import com.intel.cg.hpfd.madisonbay.wm.utils.progparser.ParserProgrammer

//scalastyle:off
class ParserJsonTester extends FlatSpec with Matchers {
  val packets: Map[String, Any] = Loader.loadJson("src/test/resources/json/parser_packets.json").get
  val programmer: Map[String, Any] = Loader.loadJson("src/test/resources/json/parser_input.json").get
  val csr = Csr()
  val parserMap = ParserProgrammer(programmer, csr)

  val parserStr = "Parser"

  Json.getListOpt(packets, "packets").get.foreach { test =>
    val testCase = test.asInstanceOf[Map[String, Any]]
    val name = testCase("name").asInstanceOf[String]

    val packet = Packet.strHexToPacket(testCase("data").asInstanceOf[String])

    val parseResult = Parser.parse(parserMap, packet)
    val parserFlags = parseResult.paFlags.get

    val expectedFlags = Json.getOpt(testCase, "out.flags").get.asInstanceOf[List[String]]

    parserStr should "correctly identify TCP in " + name in {
      parserFlags.contains(PacketFlags.TypicalPacketFlags.otr_l4_tcp_v.id) shouldEqual expectedFlags.contains("tcp")
    }

    parserStr should "correctly identify UDP in " + name in {
      parserFlags.contains(PacketFlags.TypicalPacketFlags.otr_l4_udp_v.id) shouldEqual expectedFlags.contains("udp")
    }
  }

}
