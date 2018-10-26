package com.intel.cg.hpfd.madisonbay.wm.csr

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrParser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.{Csr, CsrLenses, ParserLenses}
import org.scalatest._
import monocle.state.all._

//scalastyle:off magic.number
class CsrSpec extends FlatSpec with Matchers {

  "csr " should " produce new csr with updated parser " in {

    val csr = Csr()
    val parser = csr.getParser(0)

    val idStage = 0
    val idRule = 0
    val pl = ParserLenses(idStage)
    val updatedParser = CsrLenses.execute(parser.csrParser, for {
      _ <- pl.keyS(idRule).mod_(_.STATE_MASK.set(5))
      _ <- pl.keyS(idRule).mod_(_.STATE_VALUE.set(6))
      _ <- pl.actExt(idRule).mod_(_.PROTOCOL_ID.set(18))
      } yield ())

    val updatedCsr = csr.updated(CsrParser(parser.idMgp, updatedParser))

    val newParser = updatedCsr.topMap.mpp.mgp(parser.idMgp).rx_ppe.parser
    val keyS = newParser.PARSER_KEY_S(idStage).PARSER_KEY_S(idRule)
    val actExt = newParser.PARSER_EXT(idStage).PARSER_EXT(idRule)

    keyS.STATE_MASK.get() shouldEqual 5
    keyS.STATE_VALUE.get() shouldEqual 6
    actExt.PROTOCOL_ID.get() shouldEqual 18
  }

}
