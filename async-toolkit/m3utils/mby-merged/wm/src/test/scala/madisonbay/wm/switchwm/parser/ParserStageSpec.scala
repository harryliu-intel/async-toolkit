//scalastyle:off
package madisonbay.wm.switchwm.parser

import madisonbay.csr.all._
import madisonbay.wm.switchwm.csr.{Csr, CsrLenses, ParserStageLenses}
import madisonbay.wm.switchwm.epl.{Packet, PacketHeader}
import madisonbay.wm.switchwm.ppe.parser.output.ParserExceptions.ParserException
import madisonbay.wm.switchwm.ppe.parser._
import madisonbay.wm.switchwm.ppe.parser.output.ProtocolsOffsets
import madisonbay.wm.switchwm.ppe.ppe.Port
import madisonbay.wm.utils.Binary.BinaryInterpolator
import madisonbay.wm.utils.BitFlags
import monocle.state.all._
import org.scalatest._

/**
  * Demonstrate some trivial examples how a single parser stage should behave.
  */
class ParserStageSpec extends FlatSpec with Matchers {

  "Flag 1 and 4" should "unconditionally be set when rule 0 configured as unconditional match" in {
    val csrParser = Csr().getParser(0)
    val idx = 0
    val port = Port(0)
    val pck = Packet(Array.ofDim[Byte](79))
    val ph = PacketHeader(pck)
    val pl = ParserStageLenses(idx)

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

    val updatedParserMap = CsrLenses.execute(csrParser.ppeParserMap, for {
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
    val updatedCsrParser = csrParser.copy(ppeParserMap = updatedParserMap)

    val (bitFlags, _, _): (BitFlags, ProtocolsOffsets, Option[ParserException]) = Parser.applyActions(updatedCsrParser, ph, port)

    bitFlags.get contains 1 shouldEqual true
    bitFlags.get contains 2 shouldEqual false
    bitFlags.get contains 3 shouldEqual false
    bitFlags.get contains 4 shouldEqual true

    val (bitFlags2, _, _): (BitFlags, ProtocolsOffsets, Option[ParserException]) = Parser.applyActions(updatedCsrParser, ph, port)
    bitFlags2.toInt shouldEqual b"10010"
  }

}
