package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions

import com.intel.cg.hpfd.csr.generated.{parser_ana_s_r, parser_ana_w_r}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.PacketHeader
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.{AluOperation, Parser}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ParserState

class AnalyzerAction(anaW: parser_ana_w_r.parser_ana_w_r, anaS: parser_ana_s_r.parser_ana_s_r) {

  def analyze(packetHeader: PacketHeader, inputState: ParserState): Parser.ParserState = {
    val baseOffset = inputState.ptr + inputState.aluOperation.calculate(inputState.w(2))

    val updatedWOffsets = Array(anaW.NEXT_W0_OFFSET().toShort, anaW.NEXT_W1_OFFSET().toShort, anaW.NEXT_W2_OFFSET().toShort).
      map(nextOffset => packetHeader.getWord(baseOffset + nextOffset))

    val nextState: Short = anaS.NEXT_STATE().toShort
    val nextStateMask: Short = anaS.NEXT_STATE_MASK().toShort
    val anaState = (inputState.state & ~nextStateMask) | (nextState & nextStateMask)

    val nextAluOperation = AluOperation(anaS.NEXT_OP().toShort)
    val outputPtr = baseOffset + anaW.SKIP().toShort

    ParserState(updatedWOffsets, nextAluOperation, anaState.toShort, outputPtr.toShort)
  }

}

object AnalyzerAction {

  def apply(anaW: parser_ana_w_r.parser_ana_w_r, anaS: parser_ana_s_r.parser_ana_s_r): AnalyzerAction =
    new AnalyzerAction(anaW, anaS)

}
