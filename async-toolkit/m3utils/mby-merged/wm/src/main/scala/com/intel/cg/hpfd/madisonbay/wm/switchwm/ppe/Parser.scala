package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe

import com.intel.cg.hpfd.csr._
import com.intel.cg.hpfd.csr.generated._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.{PacketHeader, PipelineStage}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.Parser._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.Tcam._
import com.intel.cg.hpfd.madisonbay.wm.util.ImplicitExtensions.nibbles
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.{IPVersion, ParserOutput, PortIndex}
import com.intel.cg.hpfd.madisonbay.wm.util.Packet

class ParserState(val w: List[Short], val op: AluOperation, val state: Short, val ptr: Short)
object ParserState {
  def apply(w: List[Short], op: AluOperation, state: Short, ptr: Short) =
  {
    new ParserState(w, op, state, ptr)
  }
}




class Parser(csr: mby_ppe_parser_map) extends PipelineStage[Packet, ParserOutput] {
  val parserStages = 32

  val stages = (0 until parserStages).map(i => new ParserStage(csr, i))

  def initialState(ph: PacketHeader, port: PortIndex): ParserState = {
    val portCfg = csr.PARSER_PORT_CFG(port.p)
    val initWOffsets = List(portCfg.INITIAL_W0_OFFSET, portCfg.INITIAL_W1_OFFSET, portCfg.INITIAL_W2_OFFSET)
    val w = initWOffsets.map(off => ph.getWord(off.toInt))
    val aluOp = AluOperation(portCfg.INITIAL_OP_ROT.toShort, portCfg.INITIAL_OP_MASK.toShort)
    val state = portCfg.INITIAL_STATE.toShort
    val ptr = portCfg.INITIAL_PTR.toShort
    ParserState(w,aluOp, state, ptr)
  }


  def packetType(pf: PacketFlags): (PacketType, ExtractionIndex) = {
    val interface = 0
    val tcamCsr = csr.PARSER_PTYPE_TCAM(interface)
    val sramCsr = csr.PARSER_PTYPE_RAM(interface)
    val tc = tcamMatch.curried(standardTcamMatchBit)
    (tcamCsr zip sramCsr).reverse.collectFirst( { case (x,y) if {
      tc(x.KEY_INVERT, x.KEY, pf.toLong) } => (y.PTYPE().toInt, y.EXTRACT_IDX().toInt)} ).getOrElse((0,0))
  }

  def saturatingIncrement(limit: Long)(field: RdlRegister[Long]#HardwareWritable with RdlRegister[Long]#HardwareReadable): Unit = {
    val u: Long = math.min(limit, field() + 1)
    field.update(u)
  }

  object Extractor extends PipelineStage[(PacketHeader, ProtoOffsets), PacketFields] {
    val x = { (t: (PacketHeader, ProtoOffsets)) =>
      val ph = t._1
      val protoOffsets = t._2
      val fieldProfile = 0
      val extractorCsr = csr.PARSER_EXTRACT_CFG(fieldProfile)

      def satacc8b(field: RdlRegister[Long]#HardwareWritable with RdlRegister[Long]#HardwareReadable): Unit= saturatingIncrement (255)(field)
      // each of the 80 fields of the vector has a configuration in the CSR
      val f: IndexedSeq[Short] = extractorCsr.map(a => {
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

  def csumValidate(ph: PacketHeader, pf: PacketFields): Boolean = {
    false // wiki spec of validation behavior not complete (8/17/2018)
  }

  /**
    * Validate Packet Length from Header. Only checked for IPv4 packets.
    *
    * @see https://en.wikipedia.org/wiki/IPv4#Header
    * @param ph
    * @return
    */
  def ipv4ihlValidate(ph: PacketHeader): Boolean = {
    require(ph.ipVersion == IPVersion.V4)
    val ipv4_ihl = ph.bytes(0).nib(1)
    val ihlLargeEnough = ipv4_ihl >= 5
    val headerLargeEnough = ph.totalLength >= (4 * ipv4_ihl)
    headerLargeEnough & ihlLargeEnough
  }

  /**
    * Check Payload Length
    */
    def payloadValidate(pkt: Packet, ph: PacketHeader, otr_l3_ptr: Int): Boolean = {
      // length check varies based on IPv4 (where the length includes the IP header)
      // versus IPv6 (where the payload length includes all extension headers but not the IP header itself)
      ph.ipVersion match {
        case IPVersion.V4 => ph.totalLength <= (pkt.bytes.length- otr_l3_ptr - 4)
        case IPVersion.V6 => ph.totalLength <= (pkt.bytes.length- otr_l3_ptr - 40 - 4)
      }
    }

  val x: Packet => ParserOutput = pkt => {
    val ph = PacketHeader(pkt.bytes.slice(0, 192))
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

    val (_, pf, po, pe) = stagesResult

    // extract the keys
    val paKeysVal = Extractor.x((ph, po))
    // determine the packet type
    val paPacketTypeVal = packetType(pf)
    // now we have the flags and the proto-offsets
    // the metadata is the flags + a conversion of the packetheader, proto-offsets, and proto-offset configuration into a field vector

    val exceptionStage = pe match {
      case Some(x) => pe.get.stageEncountered
      case _ => {
        assert(false, "No exception encountered in parse, likely buggy parser image")
        0
      }
    }
    val (depthExceeded, headerTruncated, parsingDone) = pe match {
      case _: Some[ParserDoneException] => (false, false, true)
      case _: Some[TruncatedHeaderException] => (false, true, false)
      case _: Some[ParseDepthExceededException] => (true, false, false)
    }

    ParserOutput(
      rxPort = rxport,
      pktMeta = 0,
      rxFlags = 0,
      segMetaErr= false,
      paAdjSegLegLen = 0,
      paKeys = paKeysVal,
      paFlags = pf,
      paPointers = po,
      paKeysValid = false,
      paPointersValid = false,
      paCsumOk = false,
      paExceptionStage = exceptionStage,
      paExceptionDepthExceeded = depthExceeded,
      paExceptionTruncHeader = headerTruncated,
      paExParsingDone = parsingDone,
      paDrop = false,
      paPacketType = paPacketTypeVal._1 // (what to do with the extract index)?
    )
  }
}

object Parser {
  type ProtoId = Int
  type BaseOffset = Int
  type ProtoOffsets = IndexedSeq[(ProtoId, BaseOffset)]
  type PacketType = Int
  type ExtractionIndex = Int
  val EmptyProtoOffsets: ProtoOffsets = Vector[(ProtoId, BaseOffset)]((0,0))

  class ParserException( val stageEncountered: Int)
  object ParserException {
    def apply(eos: Boolean = false, eop: Boolean = false, done: Boolean = false, stageEncountered: Int) = {
      if (done) new ParserDoneException(stageEncountered)
      else if (eos & eop) new TruncatedHeaderException(stageEncountered)
      else new ParseDepthExceededException(stageEncountered)
    }
  }
  class AbortParserException( stageEncountered: Int) extends ParserException(stageEncountered)
  case class TruncatedHeaderException( override val stageEncountered: Int ) extends AbortParserException(stageEncountered)
  case class ParseDepthExceededException( override val stageEncountered: Int ) extends AbortParserException(stageEncountered)
  case class ParserDoneException( override val stageEncountered: Int ) extends ParserException(stageEncountered)

}

