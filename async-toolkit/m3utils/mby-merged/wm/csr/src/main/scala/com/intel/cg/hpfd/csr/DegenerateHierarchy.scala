//scalastyle:off
package com.intel.cg.hpfd.csr

trait DegenerateHierarchy[T <: IndexedSeq[_ <: RdlElement]] extends RdlHierarchy {
  protected def next : T
}
object DegenerateHierarchy {
  implicit def toNextLevel[T <: IndexedSeq[_ <: RdlElement]](x: DegenerateHierarchy[T]) : T = x.next
}
