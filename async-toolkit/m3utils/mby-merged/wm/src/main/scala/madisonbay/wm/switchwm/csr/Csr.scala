package madisonbay.wm.switchwm.csr

import madisonbay.BitVector
import madisonbay.csr.all._
import madisonbay.memory._
import madisonbay.wm.switchwm.csr.Csr.Initial.topMap
import madisonbay.wm.switchwm.csr.Csr._
import monocle.Optional

object Csr {

  final object Initial {
    val topMap: mby_top_map = mby_top_map(Address(0, 0 bits))
  }

  val paths: Map[Address, Optional[mby_top_map, BitVector]] = mby_top_map.genOpticsLookup(topMap, Optional.id)

  def apply(): Csr = new Csr(topMap)

  def apply(csr: mby_top_map): Csr = new Csr(csr)

  def apply(csr: Csr): Csr = new Csr(csr.topMap)

  case class CsrRxPpe(idMpp: Int, idMgp: Int, ppeRxMap:  mby_ppe_rx_top_map)

  case class CsrParser(idMpp: Int, idMgp: Int, ppeParserMap:  mby_ppe_parser_map)

  case class CsrMapper(idMpp: Int, idMgp: Int, ppeMapperMap:  mby_ppe_mapper_map)

}

class Csr(csr: mby_top_map) {

  def topMap: mby_top_map = csr

  def getRegister(address: Address): Option[BitVector] = Csr.paths.get(address).flatMap(_.getOption(csr))

  def getRegister(address: Long): Option[BitVector] = getRegister(Address at address.bytes)

  def updated[A](csrNode: A)(implicit ev: CsrUpdater[A]): Csr = ev.updated(csr, csrNode)

  def getRxPpe(idMpp: Int, idMgp: Int): CsrRxPpe = CsrRxPpe(idMpp, idMgp, csr.mpp(idMpp).mgp(idMgp).rx_ppe)

  def getParser(idMpp: Int, idMgp: Int): CsrParser = CsrParser(idMpp, idMgp, csr.mpp(idMpp).mgp(idMgp).rx_ppe.parser)

  def getMapper(idMpp: Int, idMgp: Int): CsrMapper = CsrMapper(idMpp, idMgp, csr.mpp(idMpp).mgp(idMgp).rx_ppe.mapper)

}
