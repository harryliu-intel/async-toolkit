package com.intel.cg.hpfd.madisonbay.wm.util

trait DegenerateIndexedSeq[+A] extends IndexedSeq[A] {

  require(length == 1, "Degenerate list only makes sense for size 1")

}

object DegenerateIndexedSeq {

  class DegenerateIndexedSeqImpl[+A](val underlying : A) extends DegenerateIndexedSeq[A] {

    def apply(i : Int) : A = underlying

    def length = 1

  }

  def apply[A](elem: A) : DegenerateIndexedSeq[A] = new DegenerateIndexedSeqImpl[A](elem)

  implicit def toScalar[A](x : DegenerateIndexedSeq[A]) : A = x(0)

}
