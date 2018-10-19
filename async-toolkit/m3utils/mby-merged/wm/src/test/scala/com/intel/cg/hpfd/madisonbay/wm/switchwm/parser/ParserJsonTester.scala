package com.intel.cg.hpfd.madisonbay.wm.switchwm.parser

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.Packet
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ParserOutput
import com.intel.cg.hpfd.madisonbay.wm.utils.Loader
import com.intel.cg.hpfd.madisonbay.wm.utils.Json.JsonMap
import org.scalatest.{FlatSpec, Matchers}
import com.intel.cg.hpfd.madisonbay.wm.utils.progparser.ParserProgrammer

//scalastyle:off
class ParserJsonTester extends FlatSpec with Matchers {
  val parserStr = "Parser"
  val jsonPath = "src/test/resources/json/parser"
  val csr = Csr()

  val testsFromScapy: Map[String, Any] = Loader.loadJson(s"$jsonPath/scapy_packets.json").get

  val progFromC: Map[String, Any] = Loader.loadJson(s"$jsonPath/program_from_c.json").get
  val testsFromC: Map[String, Any] = Loader.loadJson(s"$jsonPath/tests_from_c.json").get

  testsFromC.getList[Map[String,Any]]("tests").foreach { testCase =>
    val name = testCase.getString("dscr")
    val packet = Packet.strHexToPacket(testCase.getString("in.data"))
    val parseResult = parseFromCConfig(packet)
    println(s"$name\n${parseResult.simplifiedString}")
  }

  testsFromScapy.getList[Map[String,Any]]("packets").foreach { testCase =>
    val name = testCase.getString("name")
    //val packet = Packet.strHexToPacket(testCase.getString("data"))
    //val expectedFlags = testCase.getList[String]("out.flags")

    name match {

      case "simple_tcp" =>
        /*parserStr should "correctly identify TCP in " + name in {
          parserFlags.contains(PacketFlags.TypicalPacketFlags.otr_l4_tcp_v.id) shouldEqual expectedFlags.contains("tcp")
        }*/

      case "simple_udp" =>
        /*parserStr should "correctly identify UDP in " + name in {
          parserFlags.contains(PacketFlags.TypicalPacketFlags.otr_l4_udp_v.id) shouldEqual expectedFlags.contains("udp")
        }*/

      case _ =>
    }

  }

  def parseFromCConfig(packet: Packet): ParserOutput = {
    val parserMap = ParserProgrammer(progFromC, csr)
    Parser.parse(parserMap, packet, 0)
  }

}
