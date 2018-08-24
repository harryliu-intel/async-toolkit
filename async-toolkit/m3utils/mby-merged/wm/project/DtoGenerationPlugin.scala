import DtoGenerationPlugin.autoImport.createWmServerDtos
import sbt.Keys.{logLevel, managedSourceDirectories, sLog, sourceGenerators}
import sbt.{AutoPlugin, Compile, Def, File, Level, file, taskKey}
import sbt.io.syntax._
import sbt.io.FileFilter._

object DtoGenerationPlugin extends AutoPlugin {

  object autoImport {
    lazy val createWmServerDtos = taskKey[Seq[File]]("Generates wm server code with m3 toolchain.")
  }

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    createWmServerDtos := Def.task {
      import sys.process._
      val logger  =  sLog.value
      val m3dir = "src/main/m3"

      logger.info("Generating WM server from Scheme")

      val result : Int = s"make -C $m3dir rpc_structs".!
      require(result == 0, "Scheme-based generation step failed")

      (new File(s"$m3dir/wm_net/scala_generated") ** "*.scala")
        .getPaths
        .map(new File(_))
    }.value,
    logLevel in sourceGenerators in Compile := Level.Info,
    managedSourceDirectories in Compile += file("src/main/m3/wm_net/scala_generated"),
    sourceGenerators in Compile += createWmServerDtos
  )
}
