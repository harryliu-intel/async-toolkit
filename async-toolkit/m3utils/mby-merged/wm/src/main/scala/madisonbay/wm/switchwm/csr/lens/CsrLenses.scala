package madisonbay.wm.switchwm.csr.lens

import madisonbay.csr.all._
import scalaz.{Id, IndexedStateT}
import monocle.{Lens, Optional}
import monocle.function.Index.index

object CsrLenses {

  def execute[A](csr: A, stateTransition: IndexedStateT[Id.Id, A, A, Unit]): A = stateTransition.exec(csr)

  val mppL: Lens[mby_top_map, mby_mpp_map] = mby_top_map._mpp

  def mgpL(idMgp: Int): Optional[mby_top_map, mby_mgp_top_map] = mppL composeLens mby_mpp_map._mgp composeOptional index(idMgp)

  def rxPpeL(idMgp: Int): Optional[mby_top_map, mby_ppe_rx_top_map] = mgpL(idMgp) composeLens mby_mgp_top_map._rx_ppe

  def parserL(idMgp: Int): Optional[mby_top_map, mby_ppe_parser_map] = rxPpeL(idMgp) composeLens mby_ppe_rx_top_map._parser

  def mapperL(idMgp: Int): Optional[mby_top_map, mby_ppe_mapper_map] = rxPpeL(idMgp) composeLens mby_ppe_rx_top_map._mapper

}
