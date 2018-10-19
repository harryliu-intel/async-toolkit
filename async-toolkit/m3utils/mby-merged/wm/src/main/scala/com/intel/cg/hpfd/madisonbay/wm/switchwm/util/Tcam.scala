package com.intel.cg.hpfd.madisonbay.wm.switchwm.util

import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.ExtLong.Implicits

object Tcam {
  case class TcamQuery(keyInvert: Boolean, key: Boolean, input: Boolean)

  def tcamMatchBit(x: TcamQuery): Boolean = {
    x match {
      case TcamQuery(false, false,  _)    => false // invalid
      // match 0
      case TcamQuery(false, true,  false) => true
      case TcamQuery(false, true,  true)  => false
      // match 1
      case TcamQuery(true,  false, false) => true
      case TcamQuery(true,  false, true)  => false
      // don't care
      case TcamQuery(true,  true,  _)     => true
    }
  }

  def tcamMatchLong(behavior: TcamQuery => Boolean)(bits: Int)(keyInvert: Long, key: Long, input: Long): Boolean = {
    (0 until bits).map(pos => TcamQuery(keyInvert.getBit(pos), key.getBit(pos), input.getBit(pos))).forall(behavior)
  }
}
