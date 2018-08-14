
lazy val createWmServerDtos = taskKey[Seq[File]]("Generates wm server code with m3 toolchain.")
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
}.value

lazy val wmServerDto = (project in file("."))
  .settings(
    Settings.commonSettings,
    name := "wm-server-dto",
    managedSourceDirectories in Compile += file("src/main/m3/wm_net/scala_generated"),
    sourceGenerators in Compile += createWmServerDtos
  )
