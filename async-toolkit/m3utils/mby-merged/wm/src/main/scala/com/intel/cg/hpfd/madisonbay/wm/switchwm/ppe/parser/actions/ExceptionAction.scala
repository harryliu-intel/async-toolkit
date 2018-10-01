package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.ParserExceptions.{ParseDepthExceededException, ParserDoneException, ParserException}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.PacketHeader

class ExceptionAction(exOffset: Short, parsingDone: Boolean) {

  def test(packetHeader: PacketHeader, currentOffset: Int, stage: Int): Option[ParserException] = {
    if (conditionEOS(packetHeader, currentOffset) & packetHeader.conditionEOP) {
      Some(ParseDepthExceededException(stage))
    } else if (conditionEOS(packetHeader, currentOffset) & packetHeader.conditionEOP) {
      Some(ParseDepthExceededException(stage))
    } else if (parsingDone) {
      Some(ParserDoneException(stage))
    } else {
      None
    }
  }

  // EOP = (if last byte of non-FCS payload is in current segment) ? TRUE: FALSE -- stored in packet header
  // at construction time
  // End-Of-Segment Condition
  // EOS = adjustedSegmentLength < (currentPointer + currentStage.exceptionOffset)
  def conditionEOS(packetHeader: PacketHeader, currentOffset: Int): Boolean = {
    packetHeader.adjustedSegmentLength < currentOffset + exOffset
  }

}
