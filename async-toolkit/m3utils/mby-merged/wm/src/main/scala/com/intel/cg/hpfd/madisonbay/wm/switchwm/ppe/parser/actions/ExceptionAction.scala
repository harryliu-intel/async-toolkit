package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.{ParseDepthExceededException, ParserDoneException, ParserException}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.PacketHeader

class ExceptionAction(val exOffset: Short, val parsingDone: Boolean) {

  def x(ph: PacketHeader, currentOffset: Int, stage: Int): Option[ParserException] = {
    if (eos(ph, currentOffset) & ph.eop ) {
      Some(ParseDepthExceededException(stage))
    } else if (eos(ph, currentOffset) & ph.eop) {
      Some(ParseDepthExceededException(stage))
    } else if (parsingDone) {
      Some(ParserDoneException(stage))
    } else {
      None
    }
  }

  // EOP = (if last byte of non-FCS payload is in current segment) ? TRUE: FALSE -- stored in packet header
  // at construction time
  // EOS = adjustedSegmentLength < (currentPointer + currentStage.exceptionOffset)
  def eos(ph: PacketHeader, currentOffset: Int): Boolean = {
    ph.adjustedSegmentLength < currentOffset + exOffset
  }

}
