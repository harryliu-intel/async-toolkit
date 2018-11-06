package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.internal

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ParserState
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.Tcam
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.Tcam.TcamQuery
import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.UIntegers.getLower16
import com.intel.cg.hpfd.madisonbay.{HardwareReadable, RdlField}
import madisonbay.csr.all._

/**
  * Ternary Content-Addressable Memory
  * TCAM table: Stores ACL, QoS, and other information generally associated with Layer 3 and up  layer processing.
  * Switches use CAM to store MAC Addrss Table.
  */
object ParserTcam {

  case class TcTriple(keyInvert: RdlField[_, Long] with HardwareReadable[Long],
                      key: RdlField[_, Long] with HardwareReadable[Long],
                      input: Long)

  case class ToMatch(mask: Short, value: Short, input: Short)

  val parserMatchBitFun: TcamQuery => Boolean = tcq => parserMatchBit(tcq)

  def matchRegisterSeq(behavior: TcamQuery => Boolean)(dataSequence: Seq[TcTriple]): Boolean = {
    assert(dataSequence.forall(t => t.keyInvert.range.size == t.key.range.size), "key and mask/inverted version should be the same width")
    dataSequence.forall(t => Tcam.matchLong(behavior)(t.keyInvert.range.size)(t.keyInvert(), t.key(), t.input))
  }

  def matchRegisterSeq(dataSequence: Seq[TcTriple]): Boolean = matchRegisterSeq(parserMatchBitFun)(dataSequence)

  // added temporary to keep compatibility with c
  def camMatching(keyW: parser_key_w_r, keyS: parser_key_s_r, parserState: ParserState): Boolean = {
    Seq(ToMatch(keyW.W0_MASK().toShort,  keyW.W0_VALUE().toShort,    parserState.w(0)),
      ToMatch(keyW.W1_MASK().toShort,    keyW.W1_VALUE().toShort,    parserState.w(1)),
      ToMatch(keyS.STATE_MASK().toShort, keyS.STATE_VALUE().toShort, parserState.state)).forall {
        d => getLower16(d.input & d.mask) == getLower16(d.value)
      }
  }

  def matchRegister(behavior: TcamQuery => Boolean)(data: TcTriple): Boolean = matchRegisterSeq(behavior)(Seq(data))

  /**
    * Parser Analyzer TCAMs have a special encoding
    */
  def parserMatchBit(x: TcamQuery): Boolean = {
    x match {
      // keyInvert: Boolean, key: Boolean,  input: Boolean
      case TcamQuery(false, false, _)     => true // dont care
      case TcamQuery(false, true,  _)     => false // invalid
      case TcamQuery(true,  false, false) => true
      case TcamQuery(true,  false, true)  => false
      case TcamQuery(true,  true,  false) => false
      case TcamQuery(true,  true,  true)  => true
    }
  }
}
