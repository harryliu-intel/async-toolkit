package switch_wm


abstract class RdlElement {
  import scala.collection._
  val changes : mutable.Set[RdlElement => Unit] = new mutable.HashSet[RdlElement => Unit]
  def addressRegisterMap(baseAddress : Int) : Map[Int, RdlElement]
}

abstract class RdlHierarchy(val parent : Option[RdlHierarchy]) extends RdlElement
trait RdlDegenerateHierarchy extends RdlHierarchy {
  def apply : RdlElement
}


abstract class RdlAddressMap(parent : Option[RdlHierarchy]) extends RdlHierarchy(parent) {
  def addressRegisterMap(baseAddress : Int) : Map[Int, RdlElement] = {
    Map[Int,RdlElement]((baseAddress, this))
  }
  def updateListeners: Unit = {
    // call update on any registered listeners
    // and the on my parent, recursively
  }
}
abstract class RdlRegisterFile(parent : Option[RdlHierarchy]) extends RdlHierarchy(parent) {
  def addressRegisterMap(baseAddress : Int) : Map[Int, RdlElement] = {
    Map[Int,RdlElement]((baseAddress, this))
  }
  def updateListeners: Unit = {
    // call update on any registered listeners
    // and the on my parent, recursively
  }
}


