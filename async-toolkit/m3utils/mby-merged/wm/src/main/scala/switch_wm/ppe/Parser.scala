package switch_wm.ppe

import switch_wm._
import switch_wm.csr._
import Parser._

class ParserState(val w : List[Short], val op : AluOperation, val state : Short, val ptr : Short)
object ParserState {
  def apply(w : List[Short], op : AluOperation, state : Short, ptr : Short) =
  {
    new ParserState(w, op, state, ptr)
  }
}


class ParserStage(val csr : switch_wm.csr.mby_ppe_parser_map, val myindex : Int) {
  class AnalyzerAction(w_off : List[Short], skip: Short, op : AluOperation,
                       next_state : Short, next_state_mask : Short) {
    def x (ph : PacketHeader, input : ParserState) : ParserState = {
      val baseOffset = input.ptr + op(input.w(2))
      val outputPtr = baseOffset + skip
      val w = w_off.map(off => (ph(baseOffset + off + 1) << 16 & ph(baseOffset + off)).toShort)
      val state = (input.state & ~next_state_mask) | (next_state & next_state_mask)
      ParserState(w, op, state.toShort, outputPtr.toShort)
    }
  }
  object AnalyzerAction {
    def apply( anaW : parser_ana_w_r, anaS : parser_ana_s_r) : AnalyzerAction = {
      new AnalyzerAction(List(anaW.NEXT_W0_OFFSET.toShort, anaW.NEXT_W1_OFFSET.toShort, anaW.NEXT_W2_OFFSET.toShort),
        anaW.SKIP.toShort,AluOperation(anaS.NEXT_OP.toShort), anaS.NEXT_STATE.toShort, anaS.NEXT_STATE_MASK.toShort)
    }
  }

  class ExceptionAction( val exOffset : Short, val parsingDone : Boolean) {
    def x (input : (PacketFields, PacketFlags)) : (PacketFields, PacketFlags) = input

    // EOP = (if last byte of non-FCS payload is in current segment) ? TRUE : FALSE
    // EOS = adjustedSegmentLength < (currentPointer + currentStage.exceptionOffset)
    def eop (ph : PacketHeader, currentOffset : Int) = false
    def eos  (ph : PacketHeader, currentOffset : Int) : Boolean = {
      ph.adjustedSegmentLength < currentOffset + exOffset
    }
  }

  /**
    * Do the TCAM lookup, translate the result into actions
    */
  def matchingAction(w0 : Short, w1 : Short, state : Short) : Option[(AnalyzerAction, List[ExtractAction], ExceptionAction)]  = {
    val wcsr = csr.PARSER_KEY_W(myindex)
    val kcsr = csr.PARSER_KEY_S(myindex)
    // take the tcam relevant to _this_ stage, from configuration
    def tcamMatcher(input : Short, candidate : Short, candidateMask : Short) : Boolean = {
      // mask off the care bits from the candidate and the data in the tcam, match if the same
      (input & candidateMask) == (candidate & candidateMask)
    }
    def action(x : Int)  : AnalyzerAction = {
      val actionCsr = csr.PARSER_ANA_W(myindex)(x)
      val c = csr.PARSER_ANA_S(myindex)(x)
      AnalyzerAction(actionCsr, c)
    }
    def extractAction(x : Int) : List[ExtractAction] = {
      val eaCsr = csr.PARSER_EXT(myindex)
      val eaCsrPair = List(eaCsr(x), eaCsr(x + 16))
      eaCsrPair.map(x => ExtractAction(x))
    }
    def exceptionAction(x  : Int) : ExceptionAction = {
      val exCsr = csr.PARSER_EXC(myindex)(x)
      new ExceptionAction(exCsr.EX_OFFSET.toShort, exCsr.PARSING_DONE.apply == 1)
    }
    val tcamMatch: PartialFunction[Int, (AnalyzerAction, List[ExtractAction], ExceptionAction)] = {
      case x if (tcamMatcher(w0, wcsr(x).W0_VALUE.toShort, wcsr(x).W0_MASK.toShort) &&
        tcamMatcher(w1, wcsr(x).W1_VALUE.toShort, wcsr(x).W1_MASK.toShort) &&
        tcamMatcher(state, kcsr(x).STATE_VALUE.toShort, kcsr(x).STATE_MASK.toShort)) => (action(x), extractAction(x), exceptionAction(x))
    }
    (0 until 16) collectFirst tcamMatch
  }

  val x : (PacketHeader, ParserState, PacketFlags, Parser.ProtoOffsets, Boolean, Boolean) => (ParserState, PacketFlags, ProtoOffsets, Boolean, Boolean) = (ph, ps, pf, fields, eos, done) => {
    val action = matchingAction(ps.w(0), ps.w(1), ps.state)
    (eos, done, action) match {
      // if we're done, or there was an exception previously then do nothing
      case (true, _, _) => (ps, pf, fields, eos, done)
      case (_, true, _) => (ps, pf, fields, eos, done)
      case (_, _, None) => (ps, pf, fields, eos, done) // if no matches, then nothing to do
      case (_, _, Some((aa, extA, excA))) if excA.eos(ph, ps.ptr) => (ps, pf, fields, true, done) // if exception, do nothing except flag exception
      case (_, _, Some((aa, extA, excA))) => (ps, pf, fields, eos, excA.parsingDone)
    }
  }
}

class Parser(csr : switch_wm.csr.mby_ppe_parser_map) extends PipelineStage[PacketHeader, Metadata] {
  val parserStages = 16

  val stages = (0 until parserStages).map(i => new ParserStage(csr, i))

  def initialState(ph : PacketHeader, port : Int) : ParserState = {
    val portCfg = csr.PARSER_PORT_CFG(port)
    val initWOffsets = List(portCfg.INITIAL_W0_OFFSET, portCfg.INITIAL_W1_OFFSET, portCfg.INITIAL_W2_OFFSET)
    val w = initWOffsets.map(off => (ph(0 + off.toInt + 1) << 16 & ph(0 + off.toInt)).toShort)
    val aluOp = AluOperation(portCfg.INITIAL_OP_ROT.toShort, portCfg.INITIAL_OP_MASK.toShort)
    val state = portCfg.INITIAL_STATE.toShort
    val ptr = portCfg.INITIAL_PTR.toShort
    ParserState(w,aluOp, state, ptr)
  }

  def fieldVector(ph : PacketHeader, protoOffsets: ProtoOffsets) : PacketFields = {
    val extractorCsr = csr.PARSER_EXTRACT_CFG
    // each of the 80 fields of the vector has a configuration in the CSR
    PacketFields()
  }

  val x : (PacketHeader) => Metadata = ph => {
    val p = 0 // need to handle this via the function interface somehow...
    val is = initialState(ph, p)
    val emptyFlags = PacketFlags()
    //    val (ps : ParserState, pf: PacketFlags, eos : Boolean,  done: Boolean) = stages.map(_.x).compose(is, emptyFlags, false, false)
    val init = (is, emptyFlags, Parser.EmptyProtoOffsets, false, false)
    implicit val packetheader = ph
    // apply all the parser stages, the packetheader is the same, but pass the other state down the line
    // this needs cleanup!
    val stagesResult : (ParserState, PacketFlags, Parser.ProtoOffsets, Boolean, Boolean) = stages.map(_.x).foldLeft(init){
      (previous, f) => {
        f(ph, previous._1, previous._2, previous._3, previous._4, previous._5)
      }
    }
    // now we have the flags and the proto-offsets
    // the metadata is the flags + a conversion of the packetheader, proto-offsets, and proto-offset configuration into a field vector
    Metadata(stagesResult._2, fieldVector(ph, stagesResult._3))
  }
}

object Parser {
  type ProtoId = Int
  type BaseOffset = Int
  type ProtoOffsets = IndexedSeq[(ProtoId, BaseOffset)]
  val EmptyProtoOffsets : ProtoOffsets = Vector[(ProtoId, BaseOffset)]((0,0))
}