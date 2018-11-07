package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.internal

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrParser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.PacketHeader
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ParserState
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ParserExceptions.{AbortParserException, ParserException}
import com.intel.cg.hpfd.madisonbay.wm.utils.BitFlags
import madisonbay.csr.all._

import scala.annotation.tailrec

class ParserAction(analyzerAction: AnalyzerAction, extractActions: List[ExtractAction], exceptionAction: ExceptionAction) {

  def run(idStage: Int, parserState: ParserState, parserFlags: BitFlags, protoOffsets: Parser.ProtoOffsets)(packetHeader: PacketHeader):
  (ParserState, BitFlags, Parser.ProtoOffsets, Option[ParserException]) = {
    val currentOffset = parserState.ptr
    // is there an error condition requiring an abort (i.e. without processing this stage)
    // or is the exception action to do nothing (or mark 'done')
    exceptionAction.test(packetHeader, currentOffset, idStage) match {

      case Some(ape: AbortParserException) => (parserState, parserFlags, protoOffsets, Some(ape))

      case parsExcOpt =>
        val updatedParserState = analyzerAction.analyze(packetHeader, parserState) // setup the analyze actions for the next stage
      // do all of the extraction operations to add more to the flags and offsets
      val (updatedProtoOffsets, updatedPaFlags) = ExtractAction.extractActions(extractActions, protoOffsets, parserFlags)
        (updatedParserState, updatedPaFlags, updatedProtoOffsets, parsExcOpt)
    }
  }

}

object ParserAction {

  /**
    * Do the TCAM lookup, translate the result into actions
    */
  def matchingAction(csrParser: CsrParser, idStage: Int, parserState: ParserState): Option[ParserAction] = {
    // All lists have size of 16
    @tailrec
    def findAction(keysW: List[parser_key_w_r], keysS: List[parser_key_s_r],
                   anaWs: List[parser_ana_w_r], anaSs: List[parser_ana_s_r],
                   exts:  List[parser_ext_r],   excs:  List[parser_exc_r]): Option[ParserAction] =
      (keysW, keysS, anaWs, anaSs, exts, excs) match {
        case (kW :: _, kS :: _, aW :: _, aS :: _, _ :: _, ec :: _) if ParserTcam.camMatching(kW, kS, parserState) =>
          Some(new ParserAction(
            AnalyzerAction(aW, aS),
            ExtractAction(exts),
            ExceptionAction(ec)
          ))

        case (_ :: kWs, _ :: kSs, _ :: aWs, _ :: aSs, _ :: exs, _ :: ecs) => findAction(kWs, kSs, aWs, aSs, exs, ecs)

        case (_, _, _, _, _, _) => None
      }

    val parserMap = csrParser.ppeParserMap
    // reverse because: when multiple rules hit, the highest numbered rule within the stage wins
    findAction(parserMap.PARSER_KEY_W(idStage).PARSER_KEY_W.reverse, parserMap.PARSER_KEY_S(idStage).PARSER_KEY_S.reverse,
      parserMap.PARSER_ANA_W(idStage).PARSER_ANA_W.reverse, parserMap.PARSER_ANA_S(idStage).PARSER_ANA_S.reverse,
      parserMap.PARSER_EXT(idStage).PARSER_EXT.reverse,     parserMap.PARSER_EXC(idStage).PARSER_EXC.reverse)
  }


}
