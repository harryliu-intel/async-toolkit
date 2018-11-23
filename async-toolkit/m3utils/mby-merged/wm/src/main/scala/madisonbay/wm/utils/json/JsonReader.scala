package madisonbay.wm.utils.json

import com.fasterxml.jackson.databind.{ObjectMapper, SerializationFeature}
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import scala.util.Try

object JsonReader {

  val mapper = new ObjectMapper
  mapper.enable(SerializationFeature.INDENT_OUTPUT)
  mapper.registerModule(DefaultScalaModule)

  def parse(strJson: String): Try[Map[String, Any]] = Try {
    mapper.readValue(strJson, classOf[Map[String,Any]])
  }

  def toJson(value: Map[Symbol, Any]): String = {
    toJson(value.map { case (k,v) => k.name -> v })
  }

  def toJson(value: Any): String = {
    mapper.writeValueAsString(value)
  }

  val PatternListApplyElement = "[a-zA-Z_][a-zA-Z_0-9]*([\\(][0-9]+[\\)])+"
  val PatternNumber = "[0-9]+"

  val TokenIndex = "#"
  val TokenReference = "\\."

  //scalastyle:off cyclomatic.complexity
  def getOpt(json: Map[String, Any], path: List[String]): Option[Any] = {
    def getRec(res: Option[Any], keys: List[String], listIds: List[String]): Option[Any] =
      if (listIds.nonEmpty) {
        res match {
          case Some(l: List[_]) => getRec(l.lift(listIds.head.toInt), keys, listIds.tail)
          case _ => None
        }
      } else {
        keys match {
          case Nil => res
          case hKey :: tailKeys if hKey.startsWith(TokenIndex) => getRec(res, tailKeys, hKey.split(TokenIndex).toList.tail)
          case hKey :: tailKeys =>
            res match {
              case Some(m: Map[_, _]) => getRec(m.asInstanceOf[Map[String, Any]].get(hKey), tailKeys, List.empty)
              case Some(_) => None
              case None => None
            }
        }
      }
    getRec(Some(json), path, List.empty)
  }
  //scalastyle:on cyclomatic.complexity

  def getOpt(json: Map[String, Any], path: String): Option[Any] = getOpt(json, compilePath(path))

  def getOptFromUri(json: Map[String, Any], uriPath: String): Option[Any] = getOpt(json, compileUriPath(uriPath))

  // abc.def(3)(5).ghi(2).jkl  =>  List(abc, def, #3#5, ghi, #2, jkl)
  def compilePath(path: String): List[String] = path.split('.').toList.
    flatMap (s =>
      if (s.matches(PatternListApplyElement)) {
        s.replaceAll("\\)", "").split("\\(").toList match {
          case name :: ids => List(name, TokenIndex + ids.mkString(TokenIndex))
          case _ => List(s)
        }
      } else {
        List(s)
      }
    )

  // abc/def/3/5/ghi/2/jkl => List(abc, def, #3#5, ghi, #2, jkl)
  def compileUriPath(path: String): List[String] = {
    val (newPath, restIndexes) = path.split('/').foldLeft((List[String](), "")) {
      case ((result, indexes), element) if element.matches(PatternNumber) => (result, indexes + TokenIndex + element)
      case ((result, indexes), element) if indexes.nonEmpty => (element :: indexes :: result, "")
      case ((result, _), element) => (element :: result, "")
    }
    if (restIndexes.nonEmpty) {
      (restIndexes :: newPath).reverse
    } else {
      newPath.reverse
    }
  }

  def getListOpt(json: Map[String, Any], path: String): Option[List[Any]] = getOpt(json, path) match {
    case result@Some(_: List[_]) => result.asInstanceOf[Option[List[Any]]]
    case _ => None
  }

  def getMapOpt(json: Map[String, Any], path: String): Option[Map[String, Any]] = getOpt(json, path) match {
    case result@Some(_: Map[_, _]) => result.asInstanceOf[Option[Map[String, Any]]]
    case _ => None
  }

  def getStringOpt(json: Map[String, Any], path: String): Option[String] = getOpt(json, path) match {
    case result@Some(_: String) => result.asInstanceOf[Option[String]]
    case _ => None
  }

  def getIntOpt(json: Map[String, Any], path: String): Option[Int] = getOpt(json, path) match {
    case result@Some(_: Int) => result.asInstanceOf[Option[Int]]
    case _ => None
  }

  def getDoubleOpt(json: Map[String, Any], path: String): Option[Double] = getOpt(json, path) match {
    case result@Some(_: Double) => result.asInstanceOf[Option[Double]]
    case _ => None
  }

  def getBooleanOpt(json: Map[String, Any], path: String): Option[Boolean] = getOpt(json, path) match {
    case result@Some(_: Boolean) => result.asInstanceOf[Option[Boolean]]
    case _ => None
  }

  def getAnyIntOpt(json: Map[String, Any], path: String): Option[BigInt] = getOpt(json, path) match {
    case Some(i: Int)                   => Some(BigInt(i))
    case Some(l: Long)                  => Some(BigInt(l))
    case Some(b: java.math.BigInteger)  => Some(BigInt(b))
    case _                              => None
  }

  def getAnyDecimalOpt(json: Map[String, Any], path: String): Option[BigDecimal] = getOpt(json, path) match {
    case Some(d: Double)                => Some(BigDecimal(d))
    case Some(b: java.math.BigDecimal)  => Some(BigDecimal(b))
    case _                              => None
  }

  implicit class JsonMap(map: Map[String, Any]) {

    def getOpt(path: String): Option[Any] = JsonReader.getOpt(map, path)

    def getMapOpt(path: String): Option[Map[String, Any]] = JsonReader.getMapOpt(map, path)

    def getMap(path: String): Map[String, Any] = getMapOpt(path).get

    def getListOpt[A](path: String): Option[List[A]] = JsonReader.getListOpt(map, path).asInstanceOf[Option[List[A]]]

    def getList[A](path: String): List[A] = getListOpt(path).get

    def getStringOpt(path: String): Option[String] = JsonReader.getStringOpt(map, path)

    def getString(path: String): String = JsonReader.getStringOpt(map, path).get

    def getIntOpt(path: String): Option[Int] = JsonReader.getIntOpt(map, path)

    def getInt(path: String): Int = JsonReader.getIntOpt(map, path).get

    def getAnyIntOpt(path: String): Option[BigInt] = JsonReader.getAnyIntOpt(map, path)

    def getAnyInt(path: String): BigInt = getAnyIntOpt(path).get

    def getAnyDecimalOpt(path: String): Option[BigDecimal] = JsonReader.getAnyDecimalOpt(map, path)

    def getAnyDecimal(path: String): BigDecimal = getAnyDecimalOpt(path).get

  }

}
