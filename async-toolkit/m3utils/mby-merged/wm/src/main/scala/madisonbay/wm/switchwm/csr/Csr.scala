package madisonbay.wm.switchwm.csr

import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.Memory._
import madisonbay.wm.switchwm.csr.Csr._

object Csr {

  final object Initial {
    val topMap: mby_top_map = mby_top_map(Address(0, 0 bits))
  }

  def apply(): Csr = new Csr(Initial.topMap)

  def apply(csr: mby_top_map): Csr = new Csr(csr)

  def apply(csr: Csr): Csr = new Csr(csr.topMap)

  case class CsrRxPpe(idMgp: Int, ppeRxMap:  mby_ppe_rx_top_map)

  case class CsrParser(idMgp: Int, ppeParserMap:  mby_ppe_parser_map)

  case class CsrMapper(idMgp: Int, ppeMapperMap:  mby_ppe_mapper_map)

}

class Csr(csr: mby_top_map) {

  def topMap: mby_top_map = csr

  def updated[A](csrNode: A)(implicit ev: CsrUpdater[A]): Csr = ev.updated(csr, csrNode)

  def getRxPpe(idMgp: Int): CsrRxPpe = CsrRxPpe(idMgp, csr.mpp.mgp(idMgp).rx_ppe)

  def getParser(idMgp: Int): CsrParser = CsrParser(idMgp, csr.mpp.mgp(idMgp).rx_ppe.parser)

  def getMapper(idMgp: Int): CsrMapper = CsrMapper(idMgp, csr.mpp.mgp(idMgp).rx_ppe.mapper)

}
