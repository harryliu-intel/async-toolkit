package com.intel.cg.hpfd.csr

import com.intel.cg.hpfd.csr.PrimitiveTypes.U64

import scala.collection._

/** Base class for RDL structure (i.e. address maps, regfiles, registers)
  *
  */
abstract class RdlElement {
  val changes : mutable.Set[RdlElement => Unit] = new mutable.HashSet[RdlElement => Unit]
  def addressRegisterMap(baseAddress : Int) : SortedMap[Int, RdlElement]
  def foreachResetableField(f : RdlRegister[U64]#HardwareResetable => Unit)
}

abstract class RdlHierarchy(val parent : Option[RdlHierarchy]) extends RdlElement {
  def children : List[IndexedSeq[_ <: RdlElement]]
  def foreachResetableField(f : RdlRegister[U64]#HardwareResetable => Unit) = {
    children foreach (_.foreach(_.foreachResetableField(f)))
  }

  def mySeq(s : IndexedSeq[RdlElement]) : Boolean  = {
    s.contains()
      true

  }

  /**
    * Compute the hierarchical path to an RDL element
    *
    * Computes the hierarchical path to a hierarchical point in the CSR space. Implemented with
    * Java-style runtime reflection. (Could this be better implement with Scala-reflection or
    * by doing something at compile-time).
    *
    * This is probably a relatively slow method
    *
    * @return the path
    */
  def path : String = {
    parent match {
      case Some(p) => {
        val parentClass = p.getClass
        for (f <- parentClass.getDeclaredFields) {
          // println("examining "  + f.getName + " which is" + f.getType.getName)
          val wasAccessible = f.isAccessible
          f.setAccessible(true)
          val obj = f.get(p)
          val m = obj.getClass.getMethod("indexOf",classOf[Object])
          val res = m.invoke(obj, this).asInstanceOf[Int]
          f.setAccessible(wasAccessible)
          if ( res != -1 ) {
            return(s"${p.path}.${f.getName}($res)")
          }
        }
        assert(false, "Expected to find element in parent " + p +" but did not")
        null
      }
      case None => ""
    }
  }
}
trait RdlDegenerateHierarchy extends RdlHierarchy {
  def apply : RdlElement
}


abstract class RdlAddressMap(parent : Option[RdlHierarchy]) extends RdlHierarchy(parent) {
  def addressRegisterMap(baseAddress : Int) : SortedMap[Int, RdlElement] = {
    SortedMap[Int,RdlElement]((baseAddress, this))
  }
  def updateListeners: Unit = {
    // call update on any registered listeners
    // and the on my parent, recursively
  }
}
abstract class RdlRegisterFile(parent : Option[RdlHierarchy]) extends RdlHierarchy(parent) {
  def addressRegisterMap(baseAddress : Int) : SortedMap[Int, RdlElement] = {
    SortedMap[Int,RdlElement]((baseAddress, this))
  }
  def updateListeners: Unit = {
    // call update on any registered listeners
    // and the on my parent, recursively
  }
}