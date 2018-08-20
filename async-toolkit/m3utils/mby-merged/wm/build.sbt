
// to break current task with C-c
cancelable in sbt.Global := true

lazy val path = new File(sys.env("MODEL_ROOT") + "/target/GenRTL/wm/mbay_wm.jar")

lazy val common = (project in file("common"))
  .settings(
    Settings.commonSettings,
    libraryDependencies ++= Dependencies.commonDeps
  )

lazy val csrMacros = (project in file("csr-macros"))
  .dependsOn(common)
  .settings(
    Settings.commonSettings,
    name := "csr-macros",
    libraryDependencies ++= Dependencies.csrMacrosDeps,
    addCompilerPlugin(Dependencies.scalaMacros),
    autoCompilerPlugins := true
  )

lazy val csr = (project in file("csr"))
  .enablePlugins(CsrCodeGeneration)
  .dependsOn(csrMacros)
  .settings(
    Settings.commonSettings,
    name := "csr-model"
  )

lazy val wmServerDto = (project in file("wm-server-dto"))
  .enablePlugins(DtoCodeGeneration)
  .dependsOn(common)
  .settings(
    Settings.commonSettings,
    name := "wm-server-dto"
  )

lazy val root = (project in file("."))
  .dependsOn(common, csrMacros)
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
