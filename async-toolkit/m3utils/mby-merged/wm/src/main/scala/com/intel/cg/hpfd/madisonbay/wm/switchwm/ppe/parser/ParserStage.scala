package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import com.intel.cg.hpfd.csr.generated.mby_ppe_parser_map
import ParserTcam.{parserAnalyzerTcamMatchBit, tcamMatchSeq}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions.{AnalyzerAction, ExceptionAction, ExtractAction}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.PacketHeader


class ParserStage(val csr: mby_ppe_parser_map, val myindex: Int) {

  private case class Action(aa: AnalyzerAction, extractA: List[ExtractAction], excAction: ExceptionAction) {

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
  private def matchingAction(w0: Short, w1: Short, state: Short): Option[Action]  = {
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
