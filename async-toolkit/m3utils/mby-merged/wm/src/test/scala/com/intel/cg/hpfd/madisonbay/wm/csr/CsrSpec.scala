package com.intel.cg.hpfd.madisonbay.wm.csr

import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrParser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.{Csr, CsrLenses, ParserStageLenses}
import org.scalatest._
import monocle.state.all._

//scalastyle:off magic.number
class CsrSpec extends FlatSpec with Matchers {

  "csr " should " produce new csr with updated parser " in {

    val csr = Csr()
    val parser = csr.getParser(0)

    val idStage = 0
    val idRule = 0
    val pl = ParserStageLenses(idStage)

    val smL = pl.keyS(idRule) composeLens parser_key_s_r._STATE_MASK composeLens parser_key_s_r.STATE_MASK._value
    val svL = pl.keyS(idRule) composeLens parser_key_s_r._STATE_VALUE composeLens parser_key_s_r.STATE_VALUE._value
    val updatedParser = CsrLenses.execute(parser.ppeParserMap, for {
      _ <- smL.assign_(5)
      _ <- svL.assign_(6)
      } yield ())

    val updatedCsr = csr.updated(CsrParser(parser.idMgp, updatedParser))

    val keyS = updatedCsr.
      topMap.
      mpp.
      mgp(parser.idMgp).
      rx_ppe.
      parser.
      PARSER_KEY_S(idStage).
      PARSER_KEY_S(idRule)


    keyS.STATE_MASK.value shouldEqual 5
    keyS.STATE_VALUE.value shouldEqual 6
  }

}
