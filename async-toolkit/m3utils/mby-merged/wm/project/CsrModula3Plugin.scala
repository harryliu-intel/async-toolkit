import sbt.Keys.{
  logLevel,
  managedSourceDirectories,
  managedSources,
  baseDirectory,
  sLog,
  sourceGenerators,
  clean,
  packageSrc,
  mappings
}
import sbt.Path.{relativeTo, flat}
import sbt.{AutoPlugin, Def, File, Level, file, taskKey}
import sbt.io.syntax._
import sbt.io.FileFilter._
import sbt.io.PathFinder
import sbt.Compile

import java.io.PrintWriter
import sys.process._

object CsrModula3Plugin extends AutoPlugin {

  private val m3Path = "src/main/m3"
  private val m3BuildPath = s"$m3Path/genviews/src/build/mby"
  private val csrObject = "all"
  private val csrObjectPath = s"$m3BuildPath/src/$csrObject.scala"

  object autoImport {
    lazy val csrCodeGeneration = taskKey[Seq[File]]("Modula3 csr registers code generation")
    lazy val cleanModula3 = taskKey[Unit]("Runs clean target on modula3 sources")
  }

  import autoImport._

// Function that generates the CSR object declaration and returns
  // it as a StringBuffer
  def generateCsrObject(files: PathFinder): String = {
    // Obtain traits from file names
    val suffix = "_instance"
    val (_, fileNames) = files.pair(file => Option(file.toString().split("/").last)).unzip
    val h :: t = fileNames
      .filterNot(_ == s"$csrObject.scala")
      .map(_.split("\\.")(0) + suffix)
      .toList

    s"""|package madisonbay
        |package csr
        |
        |package object $csrObject extends $h
        |${t.sorted.map(name => s"  with $name").mkString("\n")}
        |""".stripMargin
  }

  // Generic function that creates a file basing on the return value of a
  // generator function, which returns the expected content of the file.
  // The return value of generator (StringBuffer) is directly written to the file.
  def generateFile[T](path: String, input: T)(f: T => String): File = {
    val csrFile = new File(path)
    val fileContent: String = f(input)

    val pw = new PrintWriter(csrFile)
    try {
      pw.write(fileContent)
    } finally {
      pw.close()
    }

    csrFile
  }

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    csrCodeGeneration := {
      val logger = sLog.value

      logger.info("Generating CSR set from RDL...")
      val targetDir = new File(m3BuildPath)
      val m3GenerationResult: Int = s"make -C $m3Path scalagen".!

      require(m3GenerationResult == 0, "Failed to build views")
      require(targetDir.isDirectory, s"$targetDir not a directory")

      val generatedScalaFiles = targetDir ** "*.scala"

      val csrFile = generateFile(csrObjectPath, generatedScalaFiles)(generateCsrObject)
      require(csrFile.isFile, s"$csrFile file not generated")

      logger.info("Generation DONE")
      generatedScalaFiles.getPaths.map(new File(_))
    },
    cleanModula3 := {
      val logger = sLog.value
      val result: Int = s"make -C $m3Path clean".!

      require(result == 0, "Failed to run modula3 'clean' target")
    },
    logLevel in sourceGenerators in Compile := Level.Info,
    sourceGenerators in Compile += csrCodeGeneration.taskValue,
    managedSourceDirectories in Compile += file(s"$m3BuildPath/src"),
    // publish generated sources
    mappings in (Compile, packageSrc) ++= {
      val srcs = (managedSources in Compile).value
      val sdirs = (managedSourceDirectories in Compile).value
      val base = baseDirectory.value
      (((srcs --- sdirs --- base) pair (relativeTo(sdirs) | relativeTo(base) | flat)) toSeq)
    },
    clean := clean.dependsOn(cleanModula3).value
  )
}
