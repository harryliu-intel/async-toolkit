//scalastyle:off regex.tuples
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import com.intel.cg.hpfd.csr.generated.mby_ppe_parser_map
import ParserTcam.{parserAnalyzerTcamMatchBit, tcamMatchSeq}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions.{AnalyzerAction, ExceptionAction, ExtractAction}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.PacketHeader


class ParserStage(csr: mby_ppe_parser_map.mby_ppe_parser_map, val myindex: Int) {

  private case class Action(analyzerAction: AnalyzerAction, extractActions: List[ExtractAction], exceptionAction: ExceptionAction) {

    def apply(ps: ParserState, pf: PacketFlags, po: Parser.ProtoOffsets)(ph: PacketHeader):
          (ParserState, PacketFlags, Parser.ProtoOffsets, Option[ParserException]) = {
      val currentOffset = ps.ptr
      // is there an error condition requiring an abort (i.e. without processing this stage)
      // or is the exception action to do nothing (or mark 'done')
      exceptionAction.test(ph, currentOffset, myindex) match {

        case e @ Some(_: AbortParserException) => (ps, pf, po, e)

        case e =>
          val newPs = analyzerAction(ph, ps) // setup the analyze actions for the next stage
          // do all of the extraction operations to add more to the flags and offsets
          val poPf: (ProtoOffsets, PacketFlags) = extractActions.foldLeft(po, pf)({ (prev, f) => f(prev)} )
          (newPs, poPf._2, poPf._1, e)
      }
    }

  }

  /**
    * Do the TCAM lookup, translate the result into actions
    */
  private def matchingAction(w0: Short, w1: Short, state: Short): Option[Action]  = {
    val wcsr = csr.PARSER_KEY_W(myindex)
    val kcsr = csr.PARSER_KEY_S(myindex)

    val analyzerActions = (csr.PARSER_ANA_W(myindex).PARSER_ANA_W zip csr.PARSER_ANA_S(myindex).PARSER_ANA_S).map(x => AnalyzerAction(x._1, x._2))
    val extractActions = (0 until 16).map(e => {
      List(ExtractAction(csr.PARSER_EXT(myindex).PARSER_EXT(e)), ExtractAction(csr.PARSER_EXT(myindex).PARSER_EXT(e + 16)))
    })
    val exceptionActions = csr.PARSER_EXC(myindex).PARSER_EXC.map(x => new ExceptionAction(x.EX_OFFSET().toShort, x.PARSING_DONE.apply == 1))
    val matcher = tcamMatchSeq(parserAnalyzerTcamMatchBit) _

    (wcsr.PARSER_KEY_W zip kcsr.PARSER_KEY_S) zip ((analyzerActions, extractActions, exceptionActions).zipped.toIterable) collectFirst {
      case (x, y) if matcher(Seq(
        ParserTcam.TcTriple(x._1.W0_MASK, x._1.W0_VALUE, w0),
        ParserTcam.TcTriple(x._1.W1_MASK, x._1.W1_VALUE, w1),
        ParserTcam.TcTriple(x._2.STATE_MASK, x._2.STATE_VALUE, state)
      )) => Action(y._1, y._2, y._3)
    }
  }

  def apply(packetHeader: PacketHeader, parserState: ParserState, packetFlags: PacketFlags, fields: ProtoOffsets, exception: Option[ParserException]):
            (ParserState, PacketFlags, ProtoOffsets, Option[ParserException]) = {
    val action = matchingAction(parserState.w(0), parserState.w(1), parserState.state)
    (exception, action) match {
      // if an exception has already been encountered, do nothing
      case(Some(exc), _) => (parserState, packetFlags, fields, Some(exc))
      // if nothing matches, do nothing
      case (exc, None) => (parserState, packetFlags, fields, exc)
        // otherwise, apply the action
      case (_, Some(act)) => act(parserState, packetFlags, fields)(packetHeader)
    }
  }

}
