// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.wm.csr

import madisonbay.csr.all._
import madisonbay.wm.switchwm.csr.Csr.CsrParser
import madisonbay.wm.switchwm.csr.lens.{CsrLenses, ParserStageLenses}
import madisonbay.wm.switchwm.csr.Csr
import org.scalatest._
import monocle.state.all._


class CsrSpec extends FlatSpec with Matchers {

  "csr " should " produce new csr with updated parser " in {

    val csr = Csr()
    val parser = csr.getParser(0, 0)

    val idStage = 0
    val idRule = 0
    val pl = ParserStageLenses(idStage)

    val smL = pl.keyS(idRule) composeLens parser_key_s_r._STATE_MASK composeLens parser_key_s_r.STATE_MASK._value
    val svL = pl.keyS(idRule) composeLens parser_key_s_r._STATE_VALUE composeLens parser_key_s_r.STATE_VALUE._value
    val updatedParser = CsrLenses.execute(parser.ppeParserMap, for {
      _ <- smL.assign_(5)
      _ <- svL.assign_(6)
      } yield ())

    val updatedCsr = csr.updated(CsrParser(parser.idMpp, parser.idMgp, updatedParser))

    val keyS = updatedCsr.
      topMap.
      mpp(parser.idMpp).
      mgp(parser.idMgp).
      rx_ppe.
      parser.
      PARSER_KEY_S(idStage).
      PARSER_KEY_S(idRule)


    keyS.STATE_MASK.value shouldEqual 5
    keyS.STATE_VALUE.value shouldEqual 6
  }

}
