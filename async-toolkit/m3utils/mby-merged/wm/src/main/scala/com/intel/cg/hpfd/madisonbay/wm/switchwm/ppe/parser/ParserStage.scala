package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import com.intel.cg.hpfd.csr.generated.{mby_ppe_parser_map, parser_ana_s_r, parser_ana_w_r}
import ParserTcam.{parserAnalyzerTcamMatchBit, tcamMatchSeq}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.ParserStage.{AnalyzerAction, ExceptionAction}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.PacketHeader


class ParserStage(val csr: mby_ppe_parser_map, val myindex: Int) {

  case class Action(aa: AnalyzerAction, extractA: List[ExtractAction], excAction: ExceptionAction) {

    def apply(ps: ParserState, pf: PacketFlags, po: Parser.ProtoOffsets)(ph: PacketHeader ):
          (ParserState, PacketFlags, Parser.ProtoOffsets, Option[ParserException]) = {
      val currentOffset = ps.ptr
      // is there an error condition requiring an abort (i.e. without processing this stage)
      // or is the exception action to do nothing (or mark 'done')
      excAction.x(ph, currentOffset, myindex) match {

        case e @ Some(_: AbortParserException) => (ps, pf, po, e)

        case e =>
          val newPs = aa(ph, ps) // setup the analyze actions for the next stage
          // do all of the extraction operations to add more to the flags and offsets
          val poPf: (ProtoOffsets, PacketFlags) = extractA.foldLeft(po, pf)({ (prev, f) => f(prev)} )
          (newPs, poPf._2, poPf._1, e)
      }
    }

  }

  /**
    * Do the TCAM lookup, translate the result into actions
    */
  def matchingAction(w0: Short, w1: Short, state: Short): Option[Action]  = {
    val wcsr = csr.PARSER_KEY_W(myindex)
    val kcsr = csr.PARSER_KEY_S(myindex)


    val analyzerActions = (csr.PARSER_ANA_W(myindex) zip csr.PARSER_ANA_S(myindex)).map(x => AnalyzerAction(x._1, x._2))
    val extractActions = (0 until 16).map(e => {
      List(ExtractAction(csr.PARSER_EXT(myindex)(e)), ExtractAction(csr.PARSER_EXT(myindex)(e + 16)))
    })
    val exceptionActions = csr.PARSER_EXC(myindex).map(x => new ExceptionAction(x.EX_OFFSET.toShort, x.PARSING_DONE.apply == 1))
    val matcher = tcamMatchSeq(parserAnalyzerTcamMatchBit) _

    (wcsr zip kcsr) zip (analyzerActions, extractActions, exceptionActions).zipped.toIterable collectFirst {
      case (x, y) if matcher(Seq(
        (x._1.W0_MASK, x._1.W0_VALUE, w0),
        (x._1.W1_MASK, x._1.W1_VALUE, w1),
        (x._2.STATE_MASK, x._2.STATE_VALUE, state)
      )) => Action(y._1, y._2, y._3)
    }
  }

  def apply(ph: PacketHeader, ps: ParserState, pf: PacketFlags, fields: ProtoOffsets, exception: Option[ParserException]):
            (ParserState, PacketFlags, ProtoOffsets, Option[ParserException]) = {
    val action = matchingAction(ps.w(0), ps.w(1), ps.state)
    (exception, action) match {
      // if an exception has already been encountered, do nothing
      case(Some(exc), _) => (ps, pf, fields, Some(exc))
      // if nothing matches, do nothing
      case (exc, None) => (ps, pf, fields, exc)
        // otherwise, apply the action
      case (_, Some(act)) => act(ps, pf, fields)(ph)
    }
  }

}

object ParserStage {

  class AnalyzerAction(w_off: List[Short], skip: Short, op: AluOperation,
                       next_state: Short, next_state_mask: Short) {

    def apply (ph: PacketHeader, input: ParserState): ParserState = {
      val baseOffset = input.ptr + op(input.w(2))
      val outputPtr = baseOffset + skip
      val w = w_off.map(off => (ph(baseOffset + off + 1) << 16 & ph(baseOffset + off)).toShort)
      val state = (input.state & ~next_state_mask) | (next_state & next_state_mask)
      ParserState(w, op, state.toShort, outputPtr.toShort)
    }

  }

  object AnalyzerAction {

    def apply(anaW: parser_ana_w_r, anaS: parser_ana_s_r): AnalyzerAction = {
      new AnalyzerAction(List(anaW.NEXT_W0_OFFSET.toShort, anaW.NEXT_W1_OFFSET.toShort, anaW.NEXT_W2_OFFSET.toShort),
        anaW.SKIP.toShort,AluOperation(anaS.NEXT_OP.toShort), anaS.NEXT_STATE.toShort, anaS.NEXT_STATE_MASK.toShort)
    }

  }

  class ExceptionAction(val exOffset: Short, val parsingDone: Boolean) {

    def x (ph: PacketHeader, currentOffset: Int, stage: Int): Option[ParserException] = {
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
    def eos  (ph: PacketHeader, currentOffset: Int): Boolean = {
      ph.adjustedSegmentLength < currentOffset + exOffset
    }

  }

}
