package com.intel.cg.hpfd.madisonbay.wm.switchwm.csr


import scalaz.{Id, IndexedStateT}

object CsrLenses {

  def execute[A](csr: A, stateTransition: IndexedStateT[Id.Id, A, A, Unit]): A = stateTransition.exec(csr)

}
