package switch_wm.ppe

import switch_wm._
import switch_wm.csr._
/**
  * ALU operation as used by parsing stages
  * @param rot rotation amount, up to 16 bits
  * @param mask mask field (12 bits)
  */
class AluOperation (val rot : Short, val mask : Short) {
  assert(rot < 16, "Rotate by 16 is the max allowed")
  assert((mask & 0xf000.toShort) == 0, "Only 12 bits of mask allowed")
  // no 'logical rotate' operator native to scala
  def apply(x : Short) : Short = (((x.toInt << 16 | x.toInt) >> rot.toInt) & mask).toShort
}
object AluOperation {
  // Build up from CSR encoding, High 4 bits are rotate, low 12 bits are mask
  def apply(x : Short) : AluOperation = {
    AluOperation(((x >> 12) & 0xF).toShort, (x & 0xFFF).toShort)
  }
  // Build up from CSR encoding, High 4 bits are rotate, low 12 bits are mask
  def apply(rot : Short, mask : Short) : AluOperation = {
    new AluOperation(rot.toShort, mask.toShort)
  }
}

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
  class ExtractAction(val protoId : Short, val keyOffset : Short, val flagNum : Option[Short], val flagVal : Boolean, val ptrNum : Option[Int]) {
    def x (input : (PacketFields, PacketFlags)) : (PacketFields, PacketFlags) = {
      val flags : PacketFlags = flagNum match {
        case None => input._2
        case Some(flagNum) => input._2.assign(flagNum, flagVal)
      }
      val fields : PacketFields = ptrNum match {
        case None => input._1
        case Some(ptrNum) => input._1.populateField(ptrNum, Seq(0,0))
      }
      (fields, flags)
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
      eaCsrPair.map(x => {
        // 0 is special as a flag number, means do _no_ flag annotation do not update _0_
        val flagNum = x.FLAG_NUM.apply match {
          case 0 => None
          case x => Some(x.toShort)
        }
        new ExtractAction(x.PROTOCOL_ID.toShort, x.KEY_OFFSET.toShort, flagNum, x.FLAG_VALUE() == 1l, Some(x.PTR_NUM.toShort))
      }
      )
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

  val x : (PacketHeader, ParserState, PacketFlags, PacketFields, Boolean, Boolean) => (ParserState, PacketFlags, PacketFields, Boolean, Boolean) = (ph, ps, pf, fields, eos, done) => {
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
  val stages = (0 until 16).map(i => new ParserStage(csr, i))

  def initialState(ph : PacketHeader, port : Int) : ParserState = {
    val portCfg = csr.PARSER_PORT_CFG(port)
    val initWOffsets = List(portCfg.INITIAL_W0_OFFSET, portCfg.INITIAL_W1_OFFSET, portCfg.INITIAL_W2_OFFSET)
    val w = initWOffsets.map(off => (ph(0 + off.toInt + 1) << 16 & ph(0 + off.toInt)).toShort)
    val aluOp = AluOperation(portCfg.INITIAL_OP_ROT.toShort, portCfg.INITIAL_OP_MASK.toShort)
    val state = portCfg.INITIAL_STATE.toShort
    val ptr = portCfg.INITIAL_PTR.toShort
    ParserState(w,aluOp, state, ptr)
  }

  val x : (PacketHeader) => Metadata = ph => {
    val p = 0 // need to handle this via the function interface somehow...
    val is = initialState(ph, p)
    val emptyFlags = PacketFlags()
    val emptyFields = PacketFields()
    //    val (ps : ParserState, pf: PacketFlags, eos : Boolean,  done: Boolean) = stages.map(_.x).compose(is, emptyFlags, false, false)
    val init = (is, emptyFlags, emptyFields, false, false)
    implicit val packetheader = ph
    // apply all the parser stages, the packetheader is the same, but pass the other state down the line
    // this needs cleanup!
    val result : (ParserState, PacketFlags, PacketFields, Boolean, Boolean) = stages.map(_.x).foldLeft(init){
      (previous, f) => {

        f(ph, previous._1, previous._2, previous._3, previous._4, previous._5)
      }
    }
    Metadata(result._2, result._3)
  }
}
