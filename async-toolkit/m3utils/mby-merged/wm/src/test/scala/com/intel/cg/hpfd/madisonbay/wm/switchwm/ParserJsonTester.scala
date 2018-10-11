package com.intel.cg.hpfd.madisonbay.wm.switchwm

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.Packet
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags
import com.intel.cg.hpfd.madisonbay.wm.utils.Loader
import org.scalatest.{FlatSpec, Matchers}

//scalastyle:off
class ParserJsonTester extends FlatSpec with Matchers {
  val json = Loader.loadJson("src/test/resources/json/parser_packets.json").get
  val parserStr = "Parser"

  for ((name, test) <- json) {
    val mapOfTestCase = test.asInstanceOf[Map[String, Any]]
    val dataString = mapOfTestCase("data").asInstanceOf[String]

    val packet = Packet(dataString.grouped(2).map(Integer.parseUnsignedInt(_, 16).toByte).toArray)

    val csr = Csr().getRxPpe(0).csrRxPpe.parser
    val parseResult = Parser.parse(csr, packet)

    mapOfTestCase.get("tcp").foreach(shouldBeTcp => {
      val shouldBeTcpBool = shouldBeTcp.asInstanceOf[Boolean]
      parserStr should "correctly identify TCP in " + name in {
        // TODO here parse actual flags
        parseResult.paFlags.get.contains(PacketFlags.TypicalPacketFlags.otr_l4_tcp_v.id) shouldEqual shouldBeTcpBool
      }
    })

    // TODO same for udp...
  }
}
