package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.internal

import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.PacketHeader
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ParserState
import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.ExtInt.Implicits
import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.UIntegers._

class AnalyzerAction(anaW: parser_ana_w_r, anaS: parser_ana_s_r) {

  def analyze(packetHeader: PacketHeader, inputState: ParserState): Parser.ParserState = {
    val baseOffset = getLower16(inputState.ptr).
      addWithUShortSaturation(getLower16(inputState.aluOperation.calculate(inputState.w(2))))

    val nextW = Array(anaW.NEXT_W0_OFFSET(), anaW.NEXT_W1_OFFSET(), anaW.NEXT_W2_OFFSET()).map(nextOffset =>
      packetHeader.getWord(baseOffset + getLower16(nextOffset))
    )

    val nextState = getLower16(anaS.NEXT_STATE())
    val nextStateMask = getLower16(anaS.NEXT_STATE_MASK())
    val anaState = (getLower16(inputState.state) & (~nextStateMask)) | (nextState & nextStateMask)

    val nextAluOperation = AluOperation(anaS.NEXT_OP().toShort)
    val outputPtr = baseOffset.addWithUShortSaturation(getLower16(anaW.SKIP()))

    ParserState(nextW, nextAluOperation, anaState.toShort, outputPtr.toShort)
  }

}

object AnalyzerAction {

  def apply(anaW: parser_ana_w_r, anaS: parser_ana_s_r): AnalyzerAction = new AnalyzerAction(anaW, anaS)

}
