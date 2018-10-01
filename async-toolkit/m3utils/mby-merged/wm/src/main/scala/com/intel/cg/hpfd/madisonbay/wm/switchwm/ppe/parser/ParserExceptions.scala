package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

object ParserExceptions {

  class ParserException(val stageEncountered: Int)

  object ParserException {

    def apply(eos: Boolean = false, eop: Boolean = false, done: Boolean = false, stageEncountered: Int): ParserException = {
      if (done) {
        ParserDoneException(stageEncountered)
      } else if (eos & eop) {
        TruncatedHeaderException(stageEncountered)
      } else {
        ParseDepthExceededException(stageEncountered)
      }
    }

  }

  class AbortParserException(stageEncountered: Int) extends ParserException(stageEncountered)

  case class TruncatedHeaderException(override val stageEncountered: Int) extends AbortParserException(stageEncountered)

  case class ParseDepthExceededException(override val stageEncountered: Int) extends AbortParserException(stageEncountered)

  case class ParserDoneException(override val stageEncountered: Int) extends ParserException(stageEncountered)

}
