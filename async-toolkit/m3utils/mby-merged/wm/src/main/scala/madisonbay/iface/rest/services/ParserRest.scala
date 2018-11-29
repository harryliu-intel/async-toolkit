package madisonbay.iface.rest.services

import madisonbay.fs2app.http.dispatcher.IoUriDispatcher
import madisonbay.iface.UriConstants.{UriParameter, UriSegment}
import madisonbay.iface.model.CsrModel
import madisonbay.iface.rest.RestResponse
import madisonbay.iface.rest.services.RestProcessing.responseMessage
import madisonbay.wm.switchwm.csr.Csr.CsrParser
import madisonbay.wm.utils.progparser.ParserProgrammer
import spinoco.protocol.http.HttpStatusCode
import madisonbay.wm.switchwm.csr.CsrUpdater.CsrUpdaterParser
import madisonbay.wm.switchwm.epl.Packet
import madisonbay.wm.switchwm.ppe.parser.Parser
import madisonbay.wm.switchwm.ppe.ppe.Port
import madisonbay.wm.utils.json.JsonReader._
import madisonbay.wm.utils.json.JsonSerializer
import madisonbay.wm.utils.json.JsonSerializer.isInnerCaseClassField

import scala.util.{Try, Success, Failure}

object ParserRest extends RestProcessing {

  def processGetRequest(uri: List[String], parameters: List[UriParameter], csrModel: CsrModel): RestResponse = ???

  def processPostJson(uri: List[String], jsonMap: Map[String, Any], csrModel: CsrModel): RestResponse = uri match {

    case UriSegment.Parser :: Nil => processPacket(jsonMap, csrModel)

    case UriSegment.Parser :: UriSegment.Programmer :: Nil => programParser(jsonMap, csrModel)

    case _ => returnStdNotSupported(uriPath(uri))
  }

  private def processPacket(jsonMap: Map[String, Any], csrModel: CsrModel): RestResponse = {
    (jsonMap.getStringOpt("parse.packet"), jsonMap.getIntOpt("parse.port")) match {

      case (Some(packet), Some(port)) if Packet.strHexToPacketOpt(packet).isDefined =>
        val csrParserMap = CsrParser(0, csrModel.csr.getParser(0).ppeParserMap)
        val parserOutput = Parser.parse(csrParserMap, Packet.strHexToPacket(packet), Port(port))
        Try(JsonSerializer.toMap(parserOutput, bNameCaseClasses = false) {
          case (field, _) if isInnerCaseClassField(field) => false
          case (field, _) if field.getName == "updatedParserCsr" => false
          case _ => true
        }) match {
          case Success(resultJson) => returnJson(resultJson)
          case Failure(exception)  => returnInternalServerError(exception.getMessage)
        }


      case _ => returnJson(Map(RestProcessing.KeyMessage -> "Wrong input format"))
    }
  }

  private def programParser(jsonMap: Map[String, Any], csrModel: CsrModel): RestResponse = {
    val parserMap = ParserProgrammer.readVer2(jsonMap, csrModel.csr)
    val csrParser = CsrParser(0, parserMap)
    val updatedCsr = new CsrModel(csrModel.csr.updated(csrParser), IoUriDispatcher.LimitNumberOfNodes)
    RestResponse(
      uriSupported = true,
      error = false,
      responseMessage(s"Parser successfully programmed"),
      HttpStatusCode.Ok,
      Some(updatedCsr))
  }

}
