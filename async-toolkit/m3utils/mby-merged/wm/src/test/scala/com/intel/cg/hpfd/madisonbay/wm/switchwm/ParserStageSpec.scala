package com.intel.cg.hpfd.madisonbay.wm.switchwm

import com.intel.cg.hpfd.csr.generated.mby_ppe_parser_map
import com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline.PacketHeader
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.Parser.{ParserException, ProtoOffsets}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe._
import org.scalatest._

/**
  * Demonstrate some trivial examples how a single parser stage should behave.
  */
class ParserStageSpec extends FlatSpec with Matchers {


  "Flag 1 and 4" should "unconditionally be set when rule 0 configured as unconditional match" in {
    val csr = mby_ppe_parser_map()
    val myIndex = 0
    val pf = PacketFlags()
    val protoOffset = Parser.EmptyProtoOffsets
    val noException = Option.empty[ParserException]
    val ps  = ParserState(List(0,0,0), new AluOperation(0,0), 0, 0)
    val ph = PacketHeader(Array.ofDim[Byte](79))

    csr.foreachResetableField(f => f.reset())
    csr.PARSER_KEY_S(myIndex)(0).STATE_MASK() = 0
    csr.PARSER_KEY_S(myIndex)(0).STATE_VALUE() = 0
    csr.PARSER_KEY_W(myIndex)(0).W0_MASK() = 0
    csr.PARSER_KEY_W(myIndex)(0).W0_VALUE() = 0
    csr.PARSER_KEY_W(myIndex)(0).W1_MASK() = 0
    csr.PARSER_KEY_W(myIndex)(0).W1_VALUE() = 0

    csr.PARSER_EXT(myIndex)(0).FLAG_NUM() = 1
    csr.PARSER_EXT(myIndex)(0).FLAG_VALUE() = 1
    csr.PARSER_EXT(myIndex)(16).FLAG_NUM() = 4
    csr.PARSER_EXT(myIndex)(16).FLAG_VALUE() = 1

    val pStage = new ParserStage(csr, myIndex)

    val result: (ParserState, PacketFlags, ProtoOffsets, Option[ParserException]) = pStage.apply(ph, ps, pf, protoOffset, exception = noException)
    result._2.flags contains 1 shouldEqual true
    result._2.flags contains 2 shouldEqual false
    result._2.flags contains 3 shouldEqual false
    result._2.flags contains 4 shouldEqual true
  }

}
