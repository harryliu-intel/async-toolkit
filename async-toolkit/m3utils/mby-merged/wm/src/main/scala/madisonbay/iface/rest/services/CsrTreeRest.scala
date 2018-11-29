package madisonbay.iface.rest.services

import madisonbay.iface.UriConstants.UriParameter
import madisonbay.iface.model.CsrModel
import madisonbay.iface.rest.Constants.Keys._
import madisonbay.iface.rest.Constants.Types._
import madisonbay.iface.rest.{RestProcessing, RestResponse}
import madisonbay.wm.utils.json.{JsonReader, JsonValues}
import spinoco.protocol.http.HttpStatusCode
import RestProcessing.responseMessage

object CsrTreeRest extends RestProcessing {

  def processGetRequest(uri: List[String], parameters: List[UriParameter], csrModel: CsrModel): RestResponse = {

    val path = uriPath(uri)

    JsonReader.getOptFromUri(csrModel.csrMap, path) match {

      case Some(value: Map[_, _]) =>
        val notNestedValue = csrNodeView(value.asInstanceOf[Map[String, Any]], path, fromList = false)
        returnJson(notNestedValue)

      case Some(value: List[_]) =>
        val result: Map[String, Any] = Map(KeyValues -> value)
        val notNestedValue = csrNodeView(result, path, fromList = true)
        returnJson(notNestedValue)

      case Some(value) if JsonValues.isValue(value) => returnJson(restResponseValue(value))

      case Some(any) => RestResponse(uriSupported = true, error = true,
        responseMessage(s"URI $path returned not supported value $any"),
        HttpStatusCode.Forbidden,
        None)

      case None => RestResponse(uriSupported = false, error = false,
        responseMessage(s"URI $path not supported"),
        HttpStatusCode.NotFound,
        None)
    }

  }

  private def csrNodeView(node: Map[String, Any], path: String, fromList: Boolean): Map[String, Any] = {
    val result = node.collect {
      case (k, v: Map[_, _]) if v.asInstanceOf[Map[String, Any]].contains(CsrModel.KeyValue) =>
        k -> (v.asInstanceOf[Map[String, Any]] + (KeyType -> TypeRegisterField))

      case (k, v: Map[_, _]) => k -> Map[String, Any](
        KeyType -> TypeMap,
        KeyUri -> s"/$path/$k",
        KeyKeys -> v.asInstanceOf[Map[String, Any]].keys.foldLeft(Map[String, Any]()) {
          case (acc, key) => acc.updated(key, s"/$path/$k/$key")
        }
      )

      case (k, v: List[_]) => k -> Map[String, Any](
        KeyType -> TypeList,
        KeyUri -> ( if (fromList) { "/" + path} else {s"/$path/$k"} ),
        KeySize -> v.length,
        KeyKeys ->  (
          if (fromList) {
            v.zipWithIndex.map { case (_, index) => s"/$path/$index" }
          } else  {
            v.zipWithIndex.map { case (_, index) => s"/$path/$k/$index" }
          }
          )
      )

      case (k, v) => k -> v
    }

    if (result.contains(CsrModel.KeyRange)) {
      result + (KeyType -> TypeRegister)
    } else {
      result
    }
  }

  def restResponseValue(value: Any): Map[String, Any] = Map(
      KeyType -> TypeRegisterField,
      KeyValue -> value
    )

}
