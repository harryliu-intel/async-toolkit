
// to break current task with C-c
cancelable in sbt.Global := true

lazy val path = new File(sys.env("MODEL_ROOT") + "/target/GenRTL/wm/mbay_wm.jar")

lazy val csr = (project in file("csr"))
  .enablePlugins(CsrCodeGeneration)
  .settings(
    Settings.commonSettings,
    name := "csr-model"
  )
lazy val wmServerDto = (project in file("wm-server-dto"))
  .enablePlugins(DtoCodeGeneration)
  .settings(
    Settings.commonSettings,
    name := "wm-server-dto"
  )

lazy val root = (project in file("."))
  .settings(
    Settings.commonSettings,
    name := "wm",
    libraryDependencies ++= Dependencies.whiteModelDeps,
    mainClass in Compile := Some("switch_wm.WhiteModelServer"),
    mainClass in assembly := Some("switch_wm.WhiteModelServer"),
    assemblyOutputPath in assembly := path
  )

val buildOnNhdk = taskKey[Unit]("Build task for nhdk environment.")
buildOnNhdk := Def.sequential(
  clean in csr,
  clean in wmServerDto,
  clean in root,
  publishLocal in csr,
  publishLocal in wmServerDto,
  update in root,
  compile in Compile in root,
  doc in Compile in root,
  assembly in root,
  publish in root,
  publish in csr,
  publish in wmServerDto
).value
