package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions

import com.intel.cg.hpfd.csr.generated.{parser_ana_s_r, parser_ana_w_r}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.{AluOperation, Parser}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ParserState
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.PacketHeader

class AnalyzerAction(w_off: List[Short], skip: Short, aluOperation: AluOperation,
                     next_state: Short, next_state_mask: Short) {

  def apply(packetHeader: PacketHeader, input: ParserState): Parser.ParserState = {
    val baseOffset = input.ptr + aluOperation(input.w(2))
    val outputPtr = baseOffset + skip
    val w = w_off.map(off => (packetHeader(baseOffset + off + 1) << 16 & packetHeader(baseOffset + off)).toShort)
    val state = (input.state & ~next_state_mask) | (next_state & next_state_mask)
    ParserState(w, aluOperation, state.toShort, outputPtr.toShort)
  }

}

object AnalyzerAction {

  def apply(anaW: parser_ana_w_r.parser_ana_w_r, anaS: parser_ana_s_r.parser_ana_s_r): AnalyzerAction = {
    new AnalyzerAction(List(anaW.NEXT_W0_OFFSET().toShort, anaW.NEXT_W1_OFFSET().toShort, anaW.NEXT_W2_OFFSET().toShort),
      anaW.SKIP().toShort,AluOperation(anaS.NEXT_OP().toShort), anaS.NEXT_STATE().toShort, anaS.NEXT_STATE_MASK().toShort)
  }

}
