//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.switchwm

import com.intel.cg.hpfd.csr.generated.{mby_ppe_parser_map, parser_ext_rf, parser_key_s_rf, parser_key_w_rf}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.{ParserState, ProtoOffsets}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.PacketHeader
import com.intel.cg.hpfd.madisonbay.Memory._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.ParserExceptions.ParserException
import org.scalatest._
import monocle.state.all._
import monocle.function.Index._

/**
  * Demonstrate some trivial examples how a single parser stage should behave.
  */
class ParserStageSpec extends FlatSpec with Matchers {

  "Flag 1 and 4" should "unconditionally be set when rule 0 configured as unconditional match" in {
    val csr = mby_ppe_parser_map.mby_ppe_parser_map(Address(0,0 bits))
    val idx = 0
    val pf = PacketFlags()
    val protoOffset = Parser.EmptyProtoOffsets
    val noException = Option.empty[ParserException]
    val ps  = ParserState(List(0,0,0), new AluOperation(0,0), 0, 0)
    val ph = PacketHeader(Array.ofDim[Byte](79))

    // TODO: fix that
    //csr.foreachResetableField(f => f.reset())

    val parserKeySL = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_KEY_S composeOptional
      index(idx) composeLens
      parser_key_s_rf.parser_key_s_rf._PARSER_KEY_S composeOptional
      index(0)

    val parserKeyWL = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_KEY_W composeOptional
      index(idx) composeLens
      parser_key_w_rf.parser_key_w_rf._PARSER_KEY_W composeOptional
      index(0)

    def parserExtL(parserExtRIdx: Int) = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_EXT composeOptional
      index(idx) composeLens
      parser_ext_rf.parser_ext_rf._PARSER_EXT composeOptional
      index(parserExtRIdx)

    val stateTransition = for {
      _ <- parserKeySL.mod_(_.STATE_MASK.set(0))
      _ <- parserKeySL.mod_(_.STATE_VALUE.set(0))
      _ <- parserKeyWL.mod_(_.W0_MASK.set(0))
      _ <- parserKeyWL.mod_(_.W0_VALUE.set(0))
      _ <- parserKeyWL.mod_(_.W1_MASK.set(0))
      _ <- parserKeyWL.mod_(_.W1_VALUE.set(0))
      _ <- parserExtL(0).mod_(_.FLAG_NUM.set(1))
      _ <- parserExtL(0).mod_(_.FLAG_VALUE.set(1))
      _ <- parserExtL(16).mod_(_.FLAG_NUM.set(4))
      _ <- parserExtL(16).mod_(_.FLAG_VALUE.set(1))
    } yield ()

    val updatedCsr = stateTransition.exec(csr)

    val result: (PacketFlags, ProtoOffsets, Option[ParserException]) = Parser.applyStage(updatedCsr, idx, ph, ps, pf, protoOffset, exceptionOpt = noException)
    result._1.get contains 1 shouldEqual true
    result._1.get contains 2 shouldEqual false
    result._1.get contains 3 shouldEqual false
    result._1.get contains 4 shouldEqual true
  }

}
