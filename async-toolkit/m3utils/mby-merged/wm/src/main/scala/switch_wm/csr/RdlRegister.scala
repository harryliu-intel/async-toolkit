package switch_wm.csr

import switch_wm.PrimitiveTypes.U64
import scala.collection.immutable.SortedMap


object RegisterCounter {
    var count = 0
}

abstract class RdlRegister[U <: Long](val parent : RdlHierarchy) extends RdlElement {
  RegisterCounter.count += 1

  def addressRegisterMap(baseAddress: Int): SortedMap[Int, RdlElement] = {
    SortedMap[Int, RdlElement]((baseAddress, this))
  }


  abstract class RdlField {
    protected def r: Range

    def reset(): Unit
  }

  trait HardwareReadable {
    this: RdlField =>
    def apply: Long = extract(r)
  }

  trait HardwareWritable {
    this: RdlField =>
    def assign(x: Long): Unit = replace(r, x)
  }

  trait HardwareIncrement {
    this: RdlField =>
    def ++ : Unit
  }

  trait HardwareResetable extends HardwareWritable {
    this: RdlField =>
    def reset() = assign(resetValue)

    val resetValue: Long
  }

  protected var underlyingState: Long

  def fields: List[RdlField]

  def foreachResetableField(f: RdlRegister[U64]#HardwareResetable => Unit) = {
    fields foreach {
      _ match {
        case r: RdlRegister[U64]#HardwareResetable => f(r)
      }
    }
  }

  //  abstract val resetableFields : List[HardwareResetable] = for (x <- fields) if (x.isInstanceOf[HardwareResetable]) yield x

  protected def extract(r: Range): Long = (underlyingState >> r.start) & (-1 << (r.start - r.end))

  protected def replace(r: Range, l: Long): Unit = {
    val shifted = l << r.start
    val mask = 0
    underlyingState = underlyingState & mask | shifted
  }

  //  def reset = fields.rese
  def updateListeners = {
    // call 'changes' functions, proving 'this' register
    //
  }

  def set(u: U) = {
    underlyingState = u
    updateListeners
  }

  def path: String = {
    val parentClass = parent.getClass
    for (f <- parentClass.getDeclaredFields) {
      // println("examining " + f.getName + " which is" + f.getType.getName)
      f.setAccessible(true)
      val obj = f.get(parent)
      val m = obj.getClass.getMethod("indexOf", classOf[Object])
      val res = m.invoke(obj, this).asInstanceOf[Int]
      if (res != -1) {
        return (s"${parent.path}.${f.getName}($res)")
      }
    }
    assert(false, s"Expected to find element in parent $parent but did not")
    null
  }
}

