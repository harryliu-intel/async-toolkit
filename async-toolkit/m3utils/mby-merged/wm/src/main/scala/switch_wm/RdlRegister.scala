package switch_wm

object RegisterCounter {
    var count = 0
}

abstract class RdlRegister[U <: Long](val parent : RdlHierarchy) extends RdlElement {
  RegisterCounter.count += 1

  def addressRegisterMap(baseAddress : Int) : Map[Int, RdlElement] = {
    Map[Int,RdlElement]((baseAddress, this))
  }


  abstract class RdlField {
    protected def r : Range
    def reset() : Unit
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
    val resetValue : Long
  }

  protected var underlyingState : Long
  def fields : List[RdlField]
  //  abstract val resetableFields : List[HardwareResetable] = for (x <- fields) if (x.isInstanceOf[HardwareResetable]) yield x

  protected def extract(r : Range) : Long = (underlyingState >> r.start) & (-1 << (r.start - r.end))
  protected def replace(r : Range, l : Long) : Unit = {
    val shifted = l << r.start
    val mask = 0
    underlyingState = underlyingState & mask | shifted
  }

  //  def reset = fields.rese
  def updateListeners = {
    // call 'changes' functions, proving 'this' register
    //
  }
  def set(u : U) = {
    underlyingState = u
    updateListeners
  }
}

