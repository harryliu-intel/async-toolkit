//scalastyle:off regex.tuples
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import com.intel.cg.hpfd.madisonbay.{HardwareReadable, RdlField}

/**
  * Ternary Content-Addressable Memory
  * TCAM table: Stores ACL, QoS, and other information generally associated with Layer 3 and up  layer processing.
  * Switches use CAM to store MAC Addrss Table.
  */
object ParserTcam {
  case class TcamQuery(keyInvert: Boolean, key: Boolean, input: Boolean)
  case class TcTriple(keyInvert: RdlField[_, Long] with HardwareReadable[Long],
                      key: RdlField[_, Long] with HardwareReadable[Long],
                      input: Long)

  type TcamMatchingBehavior = TcamQuery => Boolean

  // rp: used like tcamMatchSeq(parserAnalyzerTcamMatchBit)
  def tcamMatchSeq(behavior: TcamMatchingBehavior)(dataSequence: Seq[TcTriple]): Boolean = {
    assert(dataSequence.forall(t => t.keyInvert.range.size == t.key.range.size), "key and mask/inverted version should be the same width")
    dataSequence.forall(t => tcamExecuteMatching(behavior)(t.keyInvert.range.size)(t.keyInvert(), t.key(), t.input))
  }

  // rp: used like tcamMatch.curried(standardTcamMatchBit)
  def tcamMatch(behavior: TcamMatchingBehavior)(data: TcTriple): Boolean =
    tcamMatchSeq(behavior)(Seq(data))

  // rp: used like tcamMatch.curried(standardTcamMatchBit)
  def standardTcamMatchBit(x: TcamQuery): Boolean = {
    x match {
      // keyInvert: Boolean, key: Boolean,  input: Boolean
      case TcamQuery(false, false, _) => false // invalid
      // match 0
      case TcamQuery(false, true, false) => true
      case TcamQuery(false, true, true) => false
      // match 1
      case TcamQuery(true, false, false) => true
      case TcamQuery(true, false, true) => false
      // don't care
      case TcamQuery(true, true, _) => true
    }
  }

  /**
    * Parser Analyzer TCAMs have a special encoding
    */
  // rp: used like tcamMatch.curried(standardTcamMatchBit)
  def parserAnalyzerTcamMatchBit(x: TcamQuery): Boolean = {
    x match {
      // keyInvert: Boolean, key: Boolean,  input: Boolean
      case TcamQuery(false, false, _) => true // dont care
      case TcamQuery(false, true, _) => false // invalid
      case TcamQuery(true, false, false) => true
      case TcamQuery(true, false, true) => false
      case TcamQuery(true, true, false) => false
      case TcamQuery(true, true, true) => true
    }
  }

  // rp: private
  private def tcamExecuteMatching(behavior: TcamMatchingBehavior)(bits: Int)(keyInvert: Long, key: Long, input: Long): Boolean = {
    def getBit(x: Long, pos: Int) = (0x1 & x >> pos) == 1
    (0 until bits).map(pos => TcamQuery(getBit(keyInvert, pos), getBit(key, pos), getBit(input,pos))).forall(behavior)
  }

}
