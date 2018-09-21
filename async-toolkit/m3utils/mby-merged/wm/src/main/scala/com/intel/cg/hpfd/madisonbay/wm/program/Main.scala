package com.intel.cg.hpfd.madisonbay.wm.program

import java.io.File

object Main {

  case class Config(serverDir: File = new File("."), serverName: File = new File("models.packetServer"))

  def createConfiguration(): scopt.OptionParser[Config] =  new scopt.OptionParser[Config]("whitemodel") {
    head("whitemodel", "0.1")
    opt[File]("ip").valueName("<file>").action { (x, c) =>
      c.copy(serverDir = x) } text "the directory of the generated network port file"
    opt[File]("if").valueName("<file>").action { (x, c) =>
      c.copy(serverName = x) } text "the name of the generated network port file"
  }

  def main(args: Array[String]): Unit = {

   createConfiguration().parse(args, Config()) match {
      case Some(config) =>
        val fullyQualifiedFile = new File(config.serverDir + "/" + config.serverName)
        WhiteModelServer.runModelServer(fullyQualifiedFile)

      case None =>  // arguments are bad, error message will have been displayed
    }
  }

}
