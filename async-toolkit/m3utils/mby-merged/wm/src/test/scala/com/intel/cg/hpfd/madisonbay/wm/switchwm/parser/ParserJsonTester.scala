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
  val prog2FromC: Map[String, Any] = Loader.loadJson(s"$jsonPath/init_parser_from_c.json").get
  val testsFromC: Map[String, Any] = Loader.loadJson(s"$jsonPath/tests_from_c.json").get

  val payload = "123456789abcdef"
  testsFromC.getList[Map[String,Any]]("tests").take(1).foreach { testCase =>
    val name = testCase.getString("dscr")
    println(s"taking test: $name")
    val packet = Packet.strHexToPacket(testCase.getString("in.data") + payload)
    val port = testCase.getIntOpt("in.port").getOrElse(0)
    val parseResult = parseFromCConfig2(packet, port)
    println(parseResult.simplifiedString)
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

  def parseFromCConfig(packet: Packet, port: Int): ParserOutput = {
    val parserMap = ParserProgrammer.readVer1(progFromC, csr)
    Parser.parse(parserMap, packet, port)
  }

  def parseFromCConfig2(packet: Packet, port: Int): ParserOutput = {
    val parserMap = ParserProgrammer.readVer2(prog2FromC, csr)
    Parser.parse(parserMap, packet, port)
  }

}
