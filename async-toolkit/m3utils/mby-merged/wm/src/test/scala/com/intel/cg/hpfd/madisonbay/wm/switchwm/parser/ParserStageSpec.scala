//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.switchwm.parser

import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.{Csr, CsrLenses, ParserLenses}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.PacketHeader
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.{ParserState, ProtoOffsets}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.ParserExceptions.ParserException
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.PortIndex
import com.intel.cg.hpfd.madisonbay.wm.utils.Binary.BinaryInterpolator
import monocle.state.all._
import org.scalatest._

/**
  * Demonstrate some trivial examples how a single parser stage should behave.
  */
class ParserStageSpec extends FlatSpec with Matchers {

  "Flag 1 and 4" should "unconditionally be set when rule 0 configured as unconditional match" in {
    val csrParser = Csr().getParser(0).csrParser
    val idx = 0
    val pf = PacketFlags()
    val protoOffset = Parser.EmptyProtoOffsets
    val noException = Option.empty[ParserException]
    val ps = ParserState(Array(0,0,0), new AluOperation(0,0), 0, 0)
    val ph = PacketHeader(Array.ofDim[Byte](79))
    val ps2 = Parser.initialState(csrParser, ph, new PortIndex(0))

    // TODO: fix that
    //csr.foreachResetableField(f => f.reset())

    val pl = ParserLenses(idx)

    val parserKeySLmask = pl.keyS(0) composeLens parser_key_s_r._STATE_MASK composeLens parser_key_s_r.STATE_MASK._value

    val parserKeySLvalue = pl.keyS(0) composeLens parser_key_s_r._STATE_VALUE composeLens parser_key_s_r.STATE_VALUE._value

    val parserKeyWLw0mask = pl.keyW(0) composeLens parser_key_w_r._W0_MASK composeLens parser_key_w_r.W0_MASK._value

    val parserKeyWLw0value = pl.keyW(0) composeLens parser_key_w_r._W0_VALUE composeLens parser_key_w_r.W0_VALUE._value

    val parserKeyWLw1mask = pl.keyW(0) composeLens parser_key_w_r._W1_MASK composeLens parser_key_w_r.W1_MASK._value

    val parserKeyWLw1value = pl.keyW(0) composeLens parser_key_w_r._W1_VALUE composeLens parser_key_w_r.W1_VALUE._value

    def parserExtLnum(parserExtRIdx: Int) = pl.actExt(parserExtRIdx) composeLens
      parser_ext_r._FLAG_NUM composeLens parser_ext_r.FLAG_NUM._value

    def parserExtLvalue(parserExtRIdx: Int) = pl.actExt(parserExtRIdx) composeLens
      parser_ext_r._FLAG_VALUE composeLens parser_ext_r.FLAG_VALUE._value

    val updatedCsr = CsrLenses.execute(csrParser, for {
      _ <- parserKeySLmask.assign_(0)
      _ <- parserKeySLvalue.assign_(0)
      _ <- parserKeyWLw0mask.assign_(0)
      _ <- parserKeyWLw0value.assign_(0)
      _ <- parserKeyWLw1mask.assign_(0)
      _ <- parserKeyWLw1value.assign_(0)
      _ <- parserExtLnum(0).assign_(1)
      _ <- parserExtLvalue(0).assign_(1)
      _ <- parserExtLnum(16).assign_(4)
      _ <- parserExtLvalue(16).assign_(1)
    } yield ())

    val result: (PacketFlags, ProtoOffsets, Option[ParserException]) = Parser.applyStage(updatedCsr, ph)(idx, ps, pf, protoOffset, exceptionOpt = noException)
    result._1.get contains 1 shouldEqual true
    result._1.get contains 2 shouldEqual false
    result._1.get contains 3 shouldEqual false
    result._1.get contains 4 shouldEqual true

    val result2: (PacketFlags, ProtoOffsets, Option[ParserException]) = Parser.applyStage(updatedCsr, ph)(idx, ps2, pf, protoOffset, exceptionOpt = noException)
    result2._1.toInt shouldEqual b"10010"
  }

}
