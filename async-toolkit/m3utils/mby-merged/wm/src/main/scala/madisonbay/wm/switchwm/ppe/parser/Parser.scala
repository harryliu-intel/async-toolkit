//scalastyle:off regex.tuples
package madisonbay.wm.switchwm.ppe.parser

import madisonbay.wm.switchwm.ppe.parser.output.ParserExceptions._
import madisonbay.wm.switchwm.csr.Csr.CsrParser
import madisonbay.wm.switchwm.epl.{Packet, PacketHeader}
import madisonbay.wm.switchwm.ppe.parser.output.{ParserOutput, ProtocolsOffsets}
import madisonbay.wm.switchwm.ppe.ppe.Port
import madisonbay.wm.switchwm.ppe.parser.internal._
import madisonbay.wm.utils.BitFlags
import madisonbay.wm.utils.extensions.UIntegers._

import scala.annotation.tailrec

object Parser {

  case class ParserState(w: Array[Short], aluOperation: AluOperation, state: Short, ptr: Short) {
    override def toString: String = s"ParserState(${w.toList.map(e => f"0x$e%X")},$aluOperation,state=$state,ptr=$ptr)"
  }

  val NumberOfParsingStages = 32

  def parse(csrParser: CsrParser, packet: Packet, rxPort: Port): ParserOutput = {

    // TODO: support split header to Interface 0 and Interface 1
    val packetHeader = PacketHeader(packet).trimmed

    val (packetFlags, protocolOffsets, parserExceptionOpt) = applyActions(csrParser, packetHeader, rxPort)

    val (packetType, extractionIndex) = PacketType.derive(csrParser, packetFlags)

    val (paKeysVal, csrExtractedKeys) = KeysExtractor.extractKeys(csrParser, packetHeader, protocolOffsets, extractionIndex)

    val checkSums = ParserValidation.validatePayload(csrExtractedKeys, rxPort, packet, packetHeader, protocolOffsets)

    // now we have the flags and the proto-offsets
    // the metadata is the flags + a conversion of the packetheader, proto-offsets, and proto-offset configuration into a field vector
    ParserOutput(
      updatedParserCsr          = csrExtractedKeys,
      rxPort                    = rxPort,
      adjustedSegmentLength     = packetHeader.adjustedSegmentLength,
      parserKeys                = paKeysVal,
      parserFlags               = packetFlags,
      parserPointers            = protocolOffsets,
      parserException           = parserExceptionOpt,
      packetType                = packetType,
      checkSums                 = checkSums
    )
  }

  def applyActions(csr: CsrParser, packetHeader: PacketHeader, rxPort: Port): (BitFlags, ProtocolsOffsets, Option[ParserException]) =
    applyStage(csr, packetHeader)(0, initialState(csr, packetHeader, rxPort), BitFlags(), ProtocolsOffsets(),
      Option.empty[ParserException])

  @tailrec
  private def applyStage(csr: CsrParser, packetHeader: PacketHeader)
       (idStage: Int, parserState: ParserState, packetFlags: BitFlags, fields: ProtocolsOffsets, exceptionOpt: Option[ParserException]):
                        (BitFlags, ProtocolsOffsets, Option[ParserException]) = idStage match {

    case NumberOfParsingStages => (packetFlags, fields, exceptionOpt)

    case stage =>
      val action = ParserAction.matchingAction(csr, stage, parserState)
      (exceptionOpt, action) match {

        case (Some(exc), _) => (packetFlags, fields, Some(exc))

            // if nothing matches, do nothing
        case (exOpt, None)  =>
          applyStage(csr, packetHeader)(idStage + 1, parserState, packetFlags, fields, exOpt)

        case (_, Some(act)) =>                                    // otherwise, apply the action
          val (actParsState, actPckFlags, actProtOffs, actPrsExcOpt) = act.run(idStage, parserState, packetFlags, fields)(packetHeader)
          applyStage(csr, packetHeader)(idStage + 1, actParsState, actPckFlags, actProtOffs, actPrsExcOpt)
    }
  }

  def initialState(csrParser: CsrParser, packetHeader: PacketHeader, port: Port): Parser.ParserState = {
    val portCfg = csrParser.ppeParserMap.PARSER_PORT_CFG(port.index)
    val w = Array(portCfg.INITIAL_W0_OFFSET(), portCfg.INITIAL_W1_OFFSET(), portCfg.INITIAL_W2_OFFSET()).map(offset =>
      packetHeader.getWordSafe(getLower16(offset))
    )
    val aluOp = AluOperation(portCfg.INITIAL_OP_ROT().toShort, portCfg.INITIAL_OP_MASK().toShort)
    ParserState(w, aluOp, portCfg.INITIAL_STATE().toShort, portCfg.INITIAL_PTR().toShort)
  }

}
