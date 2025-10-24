// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.wm.switchwm.csr.lens

import madisonbay.csr.all._
import scalaz.{Id, IndexedStateT}
import monocle.Optional
import monocle.function.Index.index

object CsrLenses {

  def execute[A](csr: A, stateTransition: IndexedStateT[Id.Id, A, A, Unit]): A = stateTransition.exec(csr)

  def mppL(idMpp: Int): Optional[mby_top_map, mby_mpp_map] = mby_top_map._mpp composeOptional index(idMpp)

  def mgpL(idMpp: Int, idMgp: Int): Optional[mby_top_map, mby_mgp_top_map] = mppL(idMpp) composeLens mby_mpp_map._mgp composeOptional index(idMgp)

  def rxPpeL(idMpp: Int, idMgp: Int): Optional[mby_top_map, mby_ppe_rx_top_map] = mgpL(idMpp, idMgp) composeLens mby_mgp_top_map._rx_ppe

  def parserL(idMpp: Int, idMgp: Int): Optional[mby_top_map, mby_ppe_parser_map] = rxPpeL(idMpp, idMgp) composeLens mby_ppe_rx_top_map._parser

  def mapperL(idMpp: Int, idMgp: Int): Optional[mby_top_map, mby_ppe_mapper_map] = rxPpeL(idMpp, idMgp) composeLens mby_ppe_rx_top_map._mapper

}
