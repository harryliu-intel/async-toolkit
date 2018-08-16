import sbt.Keys.{logLevel, managedSourceDirectories, sLog, sourceGenerators}
import sbt.{AutoPlugin, Def, File, Level, file, taskKey}
import sbt.io.syntax._
import sbt.io.FileFilter._
import sbt.Compile

import sys.process._

object CsrCodeGeneration extends AutoPlugin {

  private val m3Path = "src/main/m3"
  private val m3BuildPath = s"$m3Path/genviews/src/build/mby"

  object autoImport {
    lazy val csrCodeGeneration = taskKey[Seq[File]]("Modula3 csr registers code generation")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    csrCodeGeneration := {
      val logger = sLog.value

      logger.info("Generating CSR set from RDL...")
      val targetDir = new File(m3BuildPath)
      val m3GenerationResult: Int = s"make -C $m3Path scalagen".!

      require(m3GenerationResult == 0, "Failed to build views")
      require(targetDir.isDirectory, s"$targetDir not a directory")

      val generatedScalaFiles = targetDir ** "*.scala"
      logger.info("Generation DONE")
      generatedScalaFiles.getPaths.map(new File(_))
    },
    logLevel in sourceGenerators in Compile := Level.Info,
    sourceGenerators in Compile += csrCodeGeneration.taskValue,
    managedSourceDirectories in Compile += file(s"${m3BuildPath}/src")
  )
}
