
// to break current task with C-c
cancelable in sbt.Global := true

lazy val path = new File(sys.env("MODEL_ROOT") + "/target/GenRTL/wm/mbay_wm.jar")

lazy val common = (project in file("common"))
  .settings(
    Settings.commonSettings,
    name := Settings.commonName,
    libraryDependencies ++= Dependencies.commonDeps,
    scalacOptions -= "-Ywarn-unused:patvars"
  )

lazy val csrMacros = (project in file("csr-macros"))
  .dependsOn(common)
  .settings(
    Settings.commonSettings,
    name := Settings.csrMacrosName,
    libraryDependencies ++= Dependencies.csrMacrosDeps,
    addCompilerPlugin(Dependencies.scalaMacrosParadise),
    autoCompilerPlugins := true,
    scalacOptions -= "-Ywarn-unused:patvars"
  )

lazy val csr = (project in file("csr"))
  .enablePlugins(CsrModula3Plugin, RdlGitHashPlugin)
  .dependsOn(csrMacros)
  .settings(
    Settings.commonSettings,
    name := Settings.csrName,
    addCompilerPlugin(Dependencies.scalaMacrosParadise),
    libraryDependencies ++= Dependencies.csrDeps,
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
    name := Settings.wmServerDtoName,
    // some imports are unused for generated classes
    scalacOptions -= "-Ywarn-unused:imports"
  )

lazy val tcp = (project in file("tcp"))
  .dependsOn(common,csrMacros)
  .settings(
    Settings.commonSettings,
    libraryDependencies ++= Dependencies.tcpDeps
  )

lazy val main = (project in file("main"))
  .dependsOn(tcp)
  .enablePlugins(RdlGitHashPlugin)
  .settings(
    Settings.commonSettings,
    addCompilerPlugin(Dependencies.kindProjector),
    libraryDependencies ++= Dependencies.mainDeps(rdlGitHashShortProjectVersion.value),
    fork in run := true
  )

lazy val root = (project in file("."))
  .dependsOn(common, csrMacros)
  .enablePlugins(RdlGitHashPlugin)
  .settings(
    Settings.commonSettings,
    name := Settings.rootName,
    libraryDependencies ++= Dependencies.whiteModelDeps(rdlGitHashShortProjectVersion.value),
    mainClass in Compile := Some("com.intel.cg.hpfd.madisonbay.wm.program.Main"),
    mainClass in assembly := Some("com.intel.cg.hpfd.madisonbay.wm.program.Main"),
    assemblyOutputPath in assembly := path,
    fork in run := true
  )

val publishArtifacts = taskKey[Unit]("Publish artifacts only if current user is npgadmin.")
publishArtifacts := Def.taskDyn {
  val log = streams.value.log
  if (sys.env.get("USER").contains("npgadmin"))
    Def.sequential(publish in csr, publish in wmServerDto)
  else
    Def.task(log.warn("Will not publish artifacts! $USER != npgadmin"))
}.value

lazy val testAll = "; all common/test csr/test root/test"
lazy val cleanAll =
  "; common/clean; csr/clean; csrMacros/clean; wmServerDto/clean; root/clean"
lazy val publishArtifactsLocally = "; csr/publishLocal; wmServerDto/publishLocal"
lazy val cleanIvyIntelCache =
  s"""; cleanCache "${Settings.intelOrganization}" % "${Settings.csrName}"""" +
  s"""; cleanCache "${Settings.intelOrganization}" % "${Settings.wmServerDtoName}""""

addCommandAlias("cleanAll", cleanAll)
addCommandAlias("cleanIvyIntelCache", cleanIvyIntelCache)
addCommandAlias("publishArtifactsLocally", publishArtifactsLocally)
addCommandAlias("compileCurrent",
  cleanAll + publishArtifactsLocally + cleanIvyIntelCache + "; root/Compile/compile")
addCommandAlias("runCurrent",
  cleanAll + publishArtifactsLocally + cleanIvyIntelCache + "; root/run")
addCommandAlias("testAll",
  cleanAll + publishArtifactsLocally + cleanIvyIntelCache + testAll)
addCommandAlias("buildOnNhdk",
  "; testAll" +
    "; root/Compile/doc; root/assembly" +
    "; publishArtifacts"
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
