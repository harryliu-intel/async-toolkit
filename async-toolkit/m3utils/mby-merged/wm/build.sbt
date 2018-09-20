import RdlGitHashPlugin.autoImport._
import sbt.Keys._

// to break current task with C-c
cancelable in sbt.Global := true

lazy val path = new File(sys.env("MODEL_ROOT") + "/target/GenRTL/wm/mbay_wm.jar")

lazy val common = (project in file("common"))
  .settings(
    Settings.commonSettings,
    libraryDependencies ++= Dependencies.commonDeps,
    scalacOptions -= "-Ywarn-unused:patvars"
  )

lazy val csrMacros = (project in file("csr-macros"))
  .dependsOn(common)
  .settings(
    Settings.commonSettings,
    name := "csr-macros",
    libraryDependencies ++= Dependencies.csrMacrosDeps,
    addCompilerPlugin(Dependencies.scalaMacros),
    autoCompilerPlugins := true,
    scalacOptions -= "-Ywarn-unused:patvars"
  )

lazy val csr = (project in file("csr"))
  .enablePlugins(CsrGenerationPlugin, RdlGitHashPlugin)
  .dependsOn(csrMacros)
  .settings(
    Settings.commonSettings,
    name := "csr-model",
    version := rdlGitHashShortProjectVersion.value,
    // some imports are unused among generated hierarchy
    scalacOptions -= "-Ywarn-unused:imports"
  )

lazy val wmServerDto = (project in file("wm-server-dto"))
  .enablePlugins(DtoGenerationPlugin)
  .dependsOn(common)
  .settings(
    Settings.commonSettings,
    // TODO: to be removed
    scalastyleFailOnError := false,
    scalastyleFailOnWarning := false,
    name := "wm-server-dto",
    // some imports are unused for generated classes
    scalacOptions -= "-Ywarn-unused:imports"
  )

lazy val root = (project in file("."))
  .dependsOn(common, csrMacros)
  .enablePlugins(RdlGitHashPlugin)
  .settings(
    Settings.commonSettings,
    name := "wm",
    libraryDependencies ++= Dependencies.whiteModelDeps(rdlGitHashShortProjectVersion.value),
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
