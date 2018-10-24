package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions

import com.intel.cg.hpfd.csr.generated.{parser_ana_s_r, parser_ana_w_r}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.PacketHeader
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.{AluOperation, Parser}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ParserState

class AnalyzerAction(nextOffsets: Array[Short], skip: Short, aluOperation: AluOperation,
                     nextState: Short, nextStateMask: Short) {

  def analyze(packetHeader: PacketHeader, inputState: ParserState): Parser.ParserState = {
    val baseOffset = inputState.ptr + aluOperation.calculate(inputState.w(2))
    val outputPtr = baseOffset + skip
    val updatedWOffsets = nextOffsets.map(nextOffset => packetHeader.getWord(baseOffset + nextOffset))
    val anaState = (inputState.state & ~nextStateMask) | (nextState & nextStateMask)
    ParserState(updatedWOffsets, aluOperation, anaState.toShort, outputPtr.toShort)
  }

}

object AnalyzerAction {

  def apply(anaW: parser_ana_w_r.parser_ana_w_r, anaS: parser_ana_s_r.parser_ana_s_r): AnalyzerAction =
    new AnalyzerAction(
      Array(anaW.NEXT_W0_OFFSET().toShort, anaW.NEXT_W1_OFFSET().toShort, anaW.NEXT_W2_OFFSET().toShort),
      anaW.SKIP().toShort,
      AluOperation(anaS.NEXT_OP().toShort),
      anaS.NEXT_STATE().toShort,
      anaS.NEXT_STATE_MASK().toShort
    )

}
