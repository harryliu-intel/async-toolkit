//scalastyle:off regex.tuples
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import com.intel.cg.hpfd.csr.generated._
import ParserTcam._
import ParserExceptions._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.PacketFields
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.{PacketFlags, ParserOutput}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.PortIndex
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.{IPVersion, Packet, PacketHeader}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.extensions.ExtLong.Implicits
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions.{AnalyzerAction, ExceptionAction, ExtractAction}
import scalaz.State
import scalaz.syntax.traverse._
import scalaz.std.list._

object Parser {

  def parse(csr: mby_ppe_parser_map.mby_ppe_parser_map): Packet => ParserOutput = packet => {

    // TODO: support split header to Interface 0 and Interface 1
    val packetHeader = PacketHeader(packet.bytes.slice(0, PacketHeader.portionSegmentFPP))

    // setup the initial state
    val rxPort = new PortIndex(0) // need to handle this via the function interface somehow...

    val (packetFlags, protoOffsets, parserException) = applyStage(csr, 0, packetHeader, initialState(csr, packetHeader, rxPort),
      PacketFlags(), Parser.EmptyProtoOffsets, Option.empty[ParserException])

    val (paKeysVal, updatedCsr) = extractKeys(csr, packetHeader, protoOffsets)

    val paPacketTypeVal = packetType(updatedCsr, packetFlags)
    // now we have the flags and the proto-offsets
    // the metadata is the flags + a conversion of the packetheader, proto-offsets, and proto-offset configuration into a field vector

    val (exceptionStage, depthExceeded, headerTruncated, parsingDone) = scanOutputExceptions(parserException)

    ParserOutput(updatedCsr,
      rxPort = rxPort,
      pktMeta = 0,
      rxFlags = 0,
      segMetaErr = false,
      paAdjSegLegLen = 0,
      paKeys = paKeysVal,
      paFlags = packetFlags,
      paPointers = protoOffsets,
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

  def applyStage(csr: mby_ppe_parser_map.mby_ppe_parser_map, idStage: Int, packetHeader: PacketHeader, parserState: ParserState,
                 packetFlags: PacketFlags, fields: ProtoOffsets, exception: Option[ParserException]):
                        (PacketFlags, ProtoOffsets, Option[ParserException]) = idStage match {

    case NumberOfParsingStages => (packetFlags, fields, exception)

    case id =>
      val action = matchingAction(csr, id, parserState.w(0), parserState.w(1), parserState.state)
      (exception, action) match {
        // if an exception has already been encountered, do nothing
        case(Some(exc), _) => (packetFlags, fields, Some(exc))
        // if nothing matches, do nothing
        case (exc, None) => (packetFlags, fields, exc)
        // otherwise, apply the action
        case (_, Some(act)) =>
          val stage = act(idStage, parserState, packetFlags, fields)(packetHeader)
          applyStage(csr, idStage + 1, packetHeader, stage._1, stage._2, stage._3, stage._4)
    }
  }

  private def scanOutputExceptions(parserException: Option[ParserException]): (Int, Boolean, Boolean, Boolean) = {
    val exceptionStage = parserException match {
      case Some(pexp) => pexp.stageEncountered
      case _ =>
        assert(assertion = false, "No exception encountered in parse, likely buggy parser image")
        0

    }
    val (depthExceeded, headerTruncated, parsingDone) = parserException match {
      case Some(ParserDoneException(_)) => (false, false, true)
      case Some(TruncatedHeaderException(_)) => (false, true, false)
      case Some(ParseDepthExceededException(_)) => (true, false, false)
      case _ => (false,false,false)
    }
    (exceptionStage, depthExceeded, headerTruncated, parsingDone)
  }

  /**
    * Do the TCAM lookup, translate the result into actions
    */
  private def matchingAction(csr: mby_ppe_parser_map.mby_ppe_parser_map, idStage: Int, w0: Short, w1: Short, state: Short): Option[Action]  = {
    val wcsr = csr.PARSER_KEY_W(idStage)
    val kcsr = csr.PARSER_KEY_S(idStage)

    val analyzerActions = (csr.PARSER_ANA_W(idStage).PARSER_ANA_W zip csr.PARSER_ANA_S(idStage).PARSER_ANA_S).map(x => AnalyzerAction(x._1, x._2))
    val extractActions = (0 until 16).map { e =>
      List(ExtractAction(csr.PARSER_EXT(idStage).PARSER_EXT(e)), ExtractAction(csr.PARSER_EXT(idStage).PARSER_EXT(e + 16)))
    }
    val exceptionActions = csr.PARSER_EXC(idStage).PARSER_EXC.map(x => new ExceptionAction(x.EX_OFFSET().toShort, x.PARSING_DONE.apply == 1))
    val matcher = tcamMatchSeq(parserAnalyzerTcamMatchBit) _

    (wcsr.PARSER_KEY_W zip kcsr.PARSER_KEY_S) zip ((analyzerActions, extractActions, exceptionActions).zipped.toIterable) collectFirst {
      case (x, y) if matcher(Seq(
        ParserTcam.TcTriple(x._1.W0_MASK, x._1.W0_VALUE, w0),
        ParserTcam.TcTriple(x._1.W1_MASK, x._1.W1_VALUE, w1),
        ParserTcam.TcTriple(x._2.STATE_MASK, x._2.STATE_VALUE, state)
      )) => Action(idStage, y._1, y._2, y._3)
    }
  }

  private case class Action(idStage: Int, analyzerAction: AnalyzerAction, extractActions: List[ExtractAction],
                            exceptionAction: ExceptionAction) {

    def apply(idStage: Int, ps: ParserState, pf: PacketFlags, po: Parser.ProtoOffsets)(ph: PacketHeader):
    (ParserState, PacketFlags, Parser.ProtoOffsets, Option[ParserException]) = {
      val currentOffset = ps.ptr
      // is there an error condition requiring an abort (i.e. without processing this stage)
      // or is the exception action to do nothing (or mark 'done')
      exceptionAction.test(ph, currentOffset, idStage) match {

        case e @ Some(_: AbortParserException) => (ps, pf, po, e)

        case e =>
          val newPs = analyzerAction(ph, ps) // setup the analyze actions for the next stage
        // do all of the extraction operations to add more to the flags and offsets
        val poPf: (ProtoOffsets, PacketFlags) = extractActions.foldLeft(po, pf)({ (prev, f) => f(prev)} )
          (newPs, poPf._2, poPf._1, e)
      }
    }
  }

  private def initialState(csr: mby_ppe_parser_map.mby_ppe_parser_map, packetHeader: PacketHeader, portIndex: PortIndex): Parser.ParserState = {
    val portCfg = csr.PARSER_PORT_CFG(portIndex.p)
    val initWOffsets = List(portCfg.INITIAL_W0_OFFSET, portCfg.INITIAL_W1_OFFSET, portCfg.INITIAL_W2_OFFSET)
    val w = initWOffsets.map(off => packetHeader.getWord(off().toInt))
    val aluOp = AluOperation(portCfg.INITIAL_OP_ROT().toShort, portCfg.INITIAL_OP_MASK().toShort)
    val state = portCfg.INITIAL_STATE().toShort
    val ptr = portCfg.INITIAL_PTR().toShort
    ParserState(w,aluOp, state, ptr)
  }

  private def packetType(csr: mby_ppe_parser_map.mby_ppe_parser_map, packetFlags: PacketFlags): (PacketType, ExtractionIndex) = {
    val interface = 0
    val tcamCsr = csr.PARSER_PTYPE_TCAM(interface).PARSER_PTYPE_TCAM
    val sramCsr = csr.PARSER_PTYPE_RAM(interface).PARSER_PTYPE_RAM

    val tc = tcamMatch(standardTcamMatchBit)(_)
    tcamCsr.zip(sramCsr).reverse.collectFirst{
      case (x,y) if tc(ParserTcam.TcTriple(x.KEY_INVERT, x.KEY, packetFlags.toLong)) => (y.PTYPE().toInt, y.EXTRACT_IDX().toInt)
    }.getOrElse((0,0))
  }

  private def extractKeys(csr: mby_ppe_parser_map.mby_ppe_parser_map, packetHeader: PacketHeader,
                          protoOffsets: ProtoOffsets): (PacketFields, mby_ppe_parser_map.mby_ppe_parser_map) = {
    val fieldProfile = 0
    val extractorCsr = csr.PARSER_EXTRACT_CFG(fieldProfile).PARSER_EXTRACT_CFG
    val countersL  = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_COUNTERS

    def modify(parserExtractCfgReg: parser_extract_cfg_r.parser_extract_cfg_r): State[(List[Short], mby_ppe_parser_map.mby_ppe_parser_map), Unit] =
      State(actualState => {
        val (result, mbyPpeParserMap) = actualState
        val protocolId = parserExtractCfgReg.PROTOCOL_ID()
        def toWordWithOffset(v: Int): Short = packetHeader.getWord(v + parserExtractCfgReg.OFFSET().toInt)

        (protocolId, protoOffsets.collect { case (pId, baseOffset) if pId == protocolId => baseOffset }.toList) match {
          case (ExtractAction.SpecialProtocolId, _) => ((0.toShort :: result, mbyPpeParserMap), ())
          case (_, Nil) =>
            val next = countersL.modify(_.EXT_UNKNOWN_PROTID.modify((v: Long) => v.incWithUByteSaturation))
            ((0.toShort :: result, next(mbyPpeParserMap)), ())
          case (_, h :: Nil) =>
            ((toWordWithOffset(h) :: result, mbyPpeParserMap),())
          case (_, h :: _) =>
            val next = countersL.modify(_.EXT_DUP_PROTID.modify((v: Long) => v.incWithUByteSaturation))
            ((toWordWithOffset(h) :: result, next(mbyPpeParserMap)), ())
        }
      })

    val ((result, updatedCsr), _) = extractorCsr.toList.traverseS(modify)((List.empty, csr))

    // TODO: handle the state here
    (PacketFields(result.reverse.toIndexedSeq), updatedCsr)
  }

  case class ParserState(w: List[Short], aluOperation: AluOperation, state: Short, ptr: Short)

  type ProtoId = Int
  type BaseOffset = Int
  type ProtoOffsets = IndexedSeq[(ProtoId, BaseOffset)]
  type PacketType = Int
  type ExtractionIndex = Int

  val EmptyProtoOffsets: ProtoOffsets = Vector[(ProtoId, BaseOffset)]((0,0))

  val NumberOfParsingStages = 32

  /**
    * Check Payload Length
    */
  def payloadValidate(packet: Packet, header: PacketHeader, otr_l3_ptr: Int): Boolean = {
    // length check varies based on IPv4 (where the length includes the IP header)
    // versus IPv6 (where the payload length includes all extension headers but not the IP header itself)
    header.ipVersion match {
      case IPVersion.IPV4 => header.totalLength <= (packet.bytes.length - otr_l3_ptr - 4)
      case IPVersion.IPV6 => header.totalLength <= (packet.bytes.length - otr_l3_ptr - 40 - 4)
    }
  }

}

