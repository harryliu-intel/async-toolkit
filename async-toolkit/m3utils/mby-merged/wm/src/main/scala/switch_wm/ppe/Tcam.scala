package switch_wm.ppe

import switch_wm.csr.RdlRegister

object Tcam {
  type tcTriple = (RdlRegister[Long]#HardwareReadable, RdlRegister[Long]#HardwareReadable, Long)
  def tcamMatchSeq(f : ((Boolean, Boolean, Boolean)) => Boolean)(x : Seq[tcTriple]) : Boolean = {
    assert(x.forall(t => t._1.size == t._2.size), "key and mask/inverted version should be the same width")
    x.forall(t => tcamMatch1(f)(t._1.size)(t._1(), t._2(), t._3))
  }
  //def tcamMatch(f : ((Boolean, Boolean, Boolean)) => Boolean)(x : tcTriple) : Boolean = {
  //  tcamMatchSeq(f)(Seq(x))
  //}
  val tcamMatch : (((Boolean, Boolean, Boolean)) => Boolean, tcTriple)=> Boolean = {
    (f, x) => tcamMatchSeq(f)(Seq(x))
  }

  def standardTcamMatchBit(x : (Boolean, Boolean, Boolean)) : Boolean = {
    x match {
      // keyInvert : Boolean, key : Boolean,  input: Boolean
      case (false, false, _) => false // invalid
      // match 0
      case(false, true, false) => true
      case(false, true, true) => false
      // match 1
      case(true, false, false) => true
      case(true, false, true) => false
      // don't care
      case (true, true, _) => true
    }
  }
  /**
    * Parser Analyzer TCAMs have a special encoding
    */
  def parserAnalyzerTcamMatchBit(x : (Boolean, Boolean, Boolean)) : Boolean = {
    x match {
      // keyInvert : Boolean, key : Boolean,  input: Boolean
      case (false, false, _) => true // dont care
      case (false, true, _) => false // invalid
      case (true, false, false) => true
      case (true, false, true) => false
      case (true, true, false) => false
      case (true, true, true) => true
    }
  }
  def tcamMatch1(f : ((Boolean, Boolean, Boolean)) => Boolean)(bits : Int)(keyInvert : Long, key : Long, input : Long) = {
    def getBit(x : Long, pos : Int) = (0x1 & x >> pos) == 1
    (0 until bits).map(pos => (getBit(keyInvert, pos), getBit(key, pos), getBit(input,pos))).forall(f)
  }
}
