package madisonbay.wm.switchwm.ppe.parser.internal

import madisonbay.csr.all._
import madisonbay.wm.switchwm.epl.PacketHeader
import madisonbay.wm.switchwm.ppe.parser.output.ParserExceptions._

class ExceptionAction(registerExc: parser_exc_r) {

  val exOffset: Short       = registerExc.EX_OFFSET().toShort
  val parsingDone: Boolean  = registerExc.PARSING_DONE.apply() == 1

  def test(packetHeader: PacketHeader, currentOffset: Int, stage: Int): Option[ParserException] = {
    if (conditionEOS(packetHeader, currentOffset)) {
      if (packetHeader.conditionEOP) {
        Some(TruncatedHeaderException(stage))
      } else {
        Some(ParseDepthExceededException(stage))
      }
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
  private def conditionEOS(packetHeader: PacketHeader, currentOffset: Int): Boolean =
    packetHeader.adjustedSegmentLength < currentOffset + exOffset

}

object ExceptionAction {

  def apply(registerExc: parser_exc_r): ExceptionAction = new ExceptionAction(registerExc)

}
