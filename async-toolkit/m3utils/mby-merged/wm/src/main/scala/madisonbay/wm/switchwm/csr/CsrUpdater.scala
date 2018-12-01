package madisonbay.wm.switchwm.csr

import madisonbay.csr.all._
import madisonbay.wm.switchwm.csr.Csr._
import madisonbay.wm.switchwm.csr.lens.CsrLenses._

trait CsrUpdater[A] {
  def updated(csr: mby_top_map, csrNode: A): Csr
}

object CsrUpdater {

  implicit object CsrUpdaterRxPpe extends CsrUpdater[CsrRxPpe] {
    def updated(csr: mby_top_map, crsRxPpe: CsrRxPpe): Csr = Csr(rxPpeL(crsRxPpe.idMgp).modify(_ => crsRxPpe.ppeRxMap)(csr))
  }

  implicit object CsrUpdaterParser extends CsrUpdater[CsrParser] {
    def updated(csr: mby_top_map, crsParser: CsrParser): Csr = Csr(parserL(crsParser.idMgp).modify(_ => crsParser.ppeParserMap)(csr))
  }

  implicit object CsrUpdaterMapper extends CsrUpdater[CsrMapper] {
    def updated(csr: mby_top_map, csrMapper: CsrMapper): Csr = Csr(mapperL(csrMapper.idMgp).modify(_ => csrMapper.ppeMapperMap)(csr))
  }

}
