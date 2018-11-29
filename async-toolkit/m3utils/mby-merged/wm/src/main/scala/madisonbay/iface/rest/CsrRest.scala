package madisonbay.iface.rest

import madisonbay.iface.model.CsrModel
import madisonbay.wm.utils.json.{JsonReader, JsonValues}

import scala.util.{Failure, Success, Try}

object CsrRest {

  val KeyValue  = "value"
  val KeyValues = "values"
  val KeyType   = "type"
  val KeyUri    = "uri"
  val KeyKeys   = "keys"
  val KeySize   = "size"

  val TypeList  = "list"
  val TypeMap   = "map"

  val TypeRegister      = "register"
  val TypeRegisterField = "registerField"

  def processGetRequest(uri: List[String], csrModel: CsrModel): RestResponse = {

    val path = uri.mkString("/")

    JsonReader.getOptFromUri(csrModel.csrMap, path) match {

      case Some(value: Map[_, _]) =>
        val notNestedValue = csrNodeView(value.asInstanceOf[Map[String, Any]], path, fromList = false)
        returnJson(notNestedValue)

      case Some(value: List[_]) =>
        val result: Map[String, Any] = Map(KeyValues -> value)
        val notNestedValue = csrNodeView(result, path, fromList = true)
        returnJson(notNestedValue)

      case Some(value) if JsonValues.isValue(value) =>
        val singleValue: Map[String, Any] = Map(
          KeyType -> TypeRegisterField,
          KeyValue -> value
        )
        returnJson(singleValue)

      case Some(any) => RestResponse(uriSupported = true, error = true, Some(s"URI $path returned not supported value $any"))

      case None => RestResponse(uriSupported = false, error = false, Some(s"URI $path not supported"))
    }

  }

  private def returnJson(result: Map[String, Any]): RestResponse = Try(JsonReader.toJson(result)) match {
    case Success(v) => RestResponse(uriSupported = true, error = false, Some(v))
    case Failure(ex) => RestResponse(uriSupported = true, error = true, Some(ex.getMessage))
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

}
