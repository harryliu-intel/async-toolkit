package madisonbay.frontend.controller

import java.io.File
import java.nio.file.Files

import scala.io.Source

object ResourceProvider {

  def getCss(filename: String): String = {
    val src = Source.fromFile(s"src/main/resources/frontend/css/$filename")
    src.getLines().mkString("\n")
  }

  def getImg(filename: String): List[Byte] = {
    val src: File = new File(s"src/main/resources/frontend/img/$filename")
    Files.readAllBytes(src.toPath).toList
  }

}
