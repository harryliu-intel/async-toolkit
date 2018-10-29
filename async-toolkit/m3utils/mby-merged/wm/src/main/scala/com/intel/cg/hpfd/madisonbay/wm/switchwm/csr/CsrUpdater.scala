package com.intel.cg.hpfd.madisonbay.wm.switchwm.csr

import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.{CsrParser, CsrRxPpe}
import CsrLenses._

trait CsrUpdater[A] {
  def updated(csr: mby_top_map, csrNode: A): Csr
}

object CsrUpdater {

  implicit object CsrUpdaterRxPpe extends CsrUpdater[CsrRxPpe] {
    def updated(csr: mby_top_map, crsRxPpe: CsrRxPpe): Csr =
     Csr(rxPpeL(crsRxPpe.idMgp).modify(_ => crsRxPpe.csrRxPpe)(csr))
  }

  implicit object CsrUpdaterParser extends CsrUpdater[CsrParser] {
    def updated(csr: mby_top_map, crsParser: CsrParser): Csr =
      Csr(parserL(crsParser.idMgp).modify(_ => crsParser.csrParser)(csr))
  }

}
