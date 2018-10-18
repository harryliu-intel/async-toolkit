//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.switchwm.parser

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
    val ps = ParserState(List(0,0,0), new AluOperation(0,0), 0, 0)
    val ph = PacketHeader(Array.ofDim[Byte](79))
    val ps2 = Parser.initialState(csrParser, ph, new PortIndex(0))

    // TODO: fix that
    //csr.foreachResetableField(f => f.reset())

    val pl = ParserLenses(idx)
    val updatedCsr = CsrLenses.execute(csrParser, for {
      _ <- pl.keyS(0).mod_(_.STATE_MASK.set(0))
      _ <- pl.keyS(0).mod_(_.STATE_VALUE.set(0))
      _ <- pl.keyW(0).mod_(_.W0_MASK.set(0))
      _ <- pl.keyW(0).mod_(_.W0_VALUE.set(0))
      _ <- pl.keyW(0).mod_(_.W1_MASK.set(0))
      _ <- pl.keyW(0).mod_(_.W1_VALUE.set(0))
      _ <- pl.actExt(0).mod_(_.FLAG_NUM.set(1))
      _ <- pl.actExt(0).mod_(_.FLAG_VALUE.set(1))
      _ <- pl.actExt(16).mod_(_.FLAG_NUM.set(4))
      _ <- pl.actExt(16).mod_(_.FLAG_VALUE.set(1))
    } yield ())

    val result: (PacketFlags, ProtoOffsets, Option[ParserException]) = Parser.applyStage(updatedCsr, idx, ph, ps, pf, protoOffset, exceptionOpt = noException)
    result._1.get contains 1 shouldEqual true
    result._1.get contains 2 shouldEqual false
    result._1.get contains 3 shouldEqual false
    result._1.get contains 4 shouldEqual true

    val result2: (PacketFlags, ProtoOffsets, Option[ParserException]) = Parser.applyStage(updatedCsr, idx, ph, ps2, pf, protoOffset, exceptionOpt = noException)
    result2._1.toInt shouldEqual b"10010"
  }

}
