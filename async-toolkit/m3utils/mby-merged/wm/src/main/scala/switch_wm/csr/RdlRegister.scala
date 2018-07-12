package switch_wm.csr

import switch_wm.PrimitiveTypes.U64

object RegisterCounter {
    var count = 0
}

abstract class RdlRegister[U <: Long](val parent : RdlHierarchy) extends RdlElement {
  RegisterCounter.count += 1

  def addressRegisterMap(baseAddress: Int): Map[Int, RdlElement] = {
    Map[Int, RdlElement]((baseAddress, this))
  }


  abstract class RdlField {
    protected def r: Range

    def reset(): Unit
  }

  /** Readable by hardware
    *
    * Applied to RdlFields which can be read by hardware (no known examples of cases where this trait does not apply)
    */
  trait HardwareReadable {
    this: RdlField =>
    def apply: Long = extract(r)
  }

  /** Writable by hardware
    *
    * Applied to fields which can be generically written to by the hardware, allows writes of any value.
    */
  trait HardwareWritable {
    this: RdlField =>
    def assign(x: Long): Unit = replace(r, x)
    def update(x: Long): Unit = assign(x)
  }

  /** Incrementer
    *
    * Applied to RdlFields which are typically counters. Future enhancmements, if needed, will be providing
    * from constructors fields to set as saturating, threshold, etc. This trait does not require the
    * HardwareWritable trait to be set.
    */
  trait HardwareIncrement {
    this: RdlField =>
    /**
      * Increment the field, by the amount of increment specified in the RDL
      */
    def inc() : Unit
  }

  /** Resetable
    *
    * Applied to fields which can be reset by the hardware reset mechanism. I.e. typically not
    * applied to cases where persistency is expected between resets.
    */
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

  /** Extract a fraction of the register, used by RdlField objects
    *
    * @param r the range to extract
    * @return a Long representing the field in question
    */
  protected def extract(r: Range): Long = (underlyingState >> r.start) & (-1 << (r.start - r.end))

  /** Assign to a fraction of the register, used by RdlField objects.
    *
    * It is an error to assign bits which include a '1' beyond the 'final' bit which will go into the range
    *
    * @param r the range to assign
    * @param l the bits to place in the range
    */
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
      val wasAccessible = f.isAccessible
      f.setAccessible(true)
      val obj = f.get(parent)
      val m = obj.getClass.getMethod("indexOf", classOf[Object])
      val res = m.invoke(obj, this).asInstanceOf[Int]
      f.setAccessible(wasAccessible)
      if (res != -1) {
        return (s"${parent.path}.${f.getName}($res)")
      }
    }
    assert(false, s"Expected to find element in parent $parent but did not")
    null
  }
}

