package madisonbay.wm.utils

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import madisonbay.wm.utils.json.JsonReader

import scala.io.Source
import scala.util.Try

object FileService {

  def loadJson(filename: String): Try[Map[String, Any]] =
    for {
      src <- Try(Source.fromFile(filename))
      result <- JsonReader.parse(src.getLines().toList.mkString(""))
    } yield result

  def saveJson(filename: String, jsonMap: Map[String, Any]): Try[Unit] =
    for {
      json <- Try(JsonReader.toJson(jsonMap))
      _ <- Try(Files.write(Paths.get(filename), json.getBytes(StandardCharsets.UTF_8)))
    } yield ()

}
