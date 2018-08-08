package switch_wm.ppe

import switch_wm._
import switch_wm.csr.{RdlRegister, _}
import Parser._
import Tcam._

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


    val analyzerActions = (csr.PARSER_ANA_W(myindex) zip csr.PARSER_ANA_S(myindex)).map(x => AnalyzerAction(x._1, x._2))
    val extractActions = (0 until 16).map(e => {
      List(ExtractAction(csr.PARSER_EXT(myindex)(e)), ExtractAction(csr.PARSER_EXT(myindex)(e + 16)))
    })
    val exceptionActions = csr.PARSER_EXC(myindex).map(x => new ExceptionAction(x.EX_OFFSET.toShort, x.PARSING_DONE.apply == 1))
    val matcher = tcamMatchSeq(parserAnalyzerTcamMatchBit) _

    (wcsr zip kcsr) zip ((analyzerActions, extractActions, exceptionActions).zipped.toIterable) collectFirst ({
      case (x, y) if matcher(Seq(
        (x._1.W0_MASK, x._1.W0_VALUE, w0),
        (x._1.W1_MASK, x._1.W1_VALUE, w1),
        (x._2.STATE_MASK, x._2.STATE_VALUE, state)
      )) => y
    })
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
    val w = initWOffsets.map(off => ph.getWord(off.toInt))
    val aluOp = AluOperation(portCfg.INITIAL_OP_ROT.toShort, portCfg.INITIAL_OP_MASK.toShort)
    val state = portCfg.INITIAL_STATE.toShort
    val ptr = portCfg.INITIAL_PTR.toShort
    ParserState(w,aluOp, state, ptr)
  }


  def packetType(pf : PacketFlags) : (PacketType, ExtractionIndex) = {
    val interface = 0
    val tcamCsr = csr.PARSER_PTYPE_TCAM(interface)
    val sramCsr = csr.PARSER_PTYPE_RAM(interface)
    val tc = tcamMatch.curried(standardTcamMatchBit)
    // TODO -- implement the TCAM encoded function
    (tcamCsr zip sramCsr).reverse.collectFirst( { case (x,y) if {
      tc(x.KEY_INVERT, x.KEY, pf.toLong) } => (y.PTYPE().toInt, y.EXTRACT_IDX().toInt)} ).getOrElse((0,0))
  }

  def saturatingIncrement(limit : Long)(field : RdlRegister[Long]#HardwareWritable with RdlRegister[Long]#HardwareReadable) = {
    field() = math.min(limit, field() + 1)
  }

  object Extractor extends PipelineStage[(PacketHeader, ProtoOffsets), PacketFields] {
    val x = { (t : (PacketHeader, ProtoOffsets)) =>
      val ph = t._1
      val protoOffsets = t._2
      val fieldProfile = 0
      val extractorCsr = csr.PARSER_EXTRACT_CFG(fieldProfile)

      def satacc8b(field : RdlRegister[Long]#HardwareWritable with RdlRegister[Long]#HardwareReadable) : Unit= saturatingIncrement (255)(field)
      // each of the 80 fields of the vector has a configuration in the CSR
      val f : IndexedSeq[Short] = extractorCsr.map(a => {
        (a.PROTOCOL_ID(), protoOffsets.collect({ case i if i._1 == a.PROTOCOL_ID() => i._2 })) match {
          case (0xFF, _) => 0.toShort
          case (_, pofs) => pofs.length match {
            case 0 => {
              satacc8b(csr.PARSER_COUNTERS.EXT_UNKNOWN_PROTID)
              0.toShort
            }
            case 1 => {
              ph.getWord(pofs.head + a.OFFSET().toInt)
            }
            case _ => { // i.e. > 1
              // behavior of duplicate condition is a bit arbitrary (should not happen in valid configuration)
              satacc8b(csr.PARSER_COUNTERS.EXT_DUP_PROTID)
              ph.getWord(pofs.head + a.OFFSET().toInt)
            }
          }
        }
      })
      PacketFields(f)
    }
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
    Metadata(stagesResult._2, Extractor.x(ph, stagesResult._3))
  }
}

object Parser {
  type ProtoId = Int
  type BaseOffset = Int
  type ProtoOffsets = IndexedSeq[(ProtoId, BaseOffset)]
  type PacketType = Int
  type ExtractionIndex = Int
  val EmptyProtoOffsets : ProtoOffsets = Vector[(ProtoId, BaseOffset)]((0,0))
}