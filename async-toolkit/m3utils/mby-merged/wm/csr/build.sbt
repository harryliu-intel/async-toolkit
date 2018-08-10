import sys.process._

val m3BuildPath = "src/main/m3/genviews/src/build/mby"

lazy val csrCodeGeneration = taskKey[Seq[File]]("Modula3 csr registers code generation")
csrCodeGeneration := {
  val logger = sLog.value

  logger.info("Generating CSR set from RDL...")
  val targetDir = new File(m3BuildPath)
  val m3GenerationResult: Int = "make -C src/main/m3 scalagen".!

  require(m3GenerationResult == 0, "Failed to build views")
  require(targetDir.isDirectory, s"$targetDir not a directory")

  val generatedScalaFiles = targetDir ** "*.scala"
  logger.info("Generation DONE")
  generatedScalaFiles.getPaths.map(new File(_))
}

lazy val csr = (project in file("."))
  .settings(
    Settings.commonSettings,
    name := "csr-model",
    logLevel in sourceGenerators in Compile := Level.Info,
    sourceGenerators in Compile += csrCodeGeneration.taskValue,
    managedSourceDirectories in Compile += file(s"$m3BuildPath/src")
  )
