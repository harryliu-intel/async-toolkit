package switch_wm.ppe

import switch_wm._
import com.intel.cg.hpfd.csr._
import com.intel.cg.hpfd.csr.generated._
import Parser._
import Tcam._

class ParserState(val w : List[Short], val op : AluOperation, val state : Short, val ptr : Short)
object ParserState {
  def apply(w : List[Short], op : AluOperation, state : Short, ptr : Short) =
  {
    new ParserState(w, op, state, ptr)
  }
}




class Parser(csr : mby_ppe_parser_map) extends PipelineStage[PacketHeader, ParserOutput] {
  val parserStages = 32

  val stages = (0 until parserStages).map(i => new ParserStage(csr, i))

  def initialState(ph : PacketHeader, port : PortIndex) : ParserState = {
    val portCfg = csr.PARSER_PORT_CFG(port.p)
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

  def saturatingIncrement(limit : Long)(field : RdlRegister[Long]#HardwareWritable with RdlRegister[Long]#HardwareReadable) : Unit = {
    val u : Long = math.min(limit, field() + 1)
    field.update(u)
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

  def csumValidate(ph : PacketHeader, pf : PacketFields) : Boolean = {
    false // wiki spec of validation behavior not complete (8/17/2018)
  }

  val x : PacketHeader => ParserOutput = ph => {
    // setup the initial state
    val rxport = new PortIndex(0) // need to handle this via the function interface somehow...
    val is = initialState(ph, rxport)
    val emptyFlags = PacketFlags()
    val init = (is, emptyFlags, Parser.EmptyProtoOffsets, Option.empty[ParserException])


    implicit val packetheader = ph

    // apply all the parser stages, the packetheader is the same, but pass the other state down the line
    // this needs cleanup!
    val stagesResult  = stages.foldLeft(init) {
      (previous, stage) => { stage(ph, previous._1, previous._2, previous._3, previous._4) }
    }


    // extract the keys
    val paKeysVal = Extractor.x(ph, stagesResult._3)
    // determine the packet type
    val paPacketTypeVal = packetType(stagesResult._2)
    // now we have the flags and the proto-offsets
    // the metadata is the flags + a conversion of the packetheader, proto-offsets, and proto-offset configuration into a field vector
    ParserOutput(
      rxPort = rxport,
      pktMeta = 0,
      rxFlags = 0,
      segMetaErr= false,
      paAdjSegLegLen = 0,
      paKeys = paKeysVal,
      paFlags = stagesResult._2,
      paPointers = stagesResult._3,
      paKeysValid = false,
      paPointersValid = false,
      paCsumOk = false,
      paExceptionStage = 0,
      paExceptionDepthExceeded = false,
      paExceptionTruncHeader = false,
      paExParsingDone = false,
      paDrop = false,
      paPacketType = paPacketTypeVal._1 // (what to do with the extract index
    )
  }
}

object Parser {
  type ProtoId = Int
  type BaseOffset = Int
  type ProtoOffsets = IndexedSeq[(ProtoId, BaseOffset)]
  type PacketType = Int
  type ExtractionIndex = Int
  val EmptyProtoOffsets : ProtoOffsets = Vector[(ProtoId, BaseOffset)]((0,0))

  case class ParserException(eos : Boolean = false,
                             eop: Boolean = false,
                             done : Boolean = false,
                             stageEncountered : Int,
                            )
}

