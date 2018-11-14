
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

lazy val csrMacroTests = (project in file("csr-macro-tests"))
  .dependsOn(csrMacros)
  .settings(
    Settings.commonSettings,
    name := Settings.csrMacroTestsName,
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
    scalacOptions -= "-Ywarn-unused:imports",
    publishArtifact in Test := true,
    publishArtifact in packageDoc in Test := false
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
  .dependsOn(common, csrMacros)
  .settings(
    Settings.commonSettings,
    libraryDependencies ++= Dependencies.tcpDeps
  )

lazy val root = (project in file("."))
  .dependsOn(common, csrMacros, tcp)
  .enablePlugins(RdlGitHashPlugin)
  .settings(
    Settings.commonSettings,
    name := Settings.rootName,
    addCompilerPlugin(Dependencies.kindProjector),
    libraryDependencies ++= Dependencies.whiteModelDeps(rdlGitHashShortProjectVersion.value),
    mainClass in Compile := Some("madisonbay.Main"),
    mainClass in assembly := Some("madisonbay.Main"),
    test in assembly := {},
    assemblyOutputPath in assembly := path,
    fork in run := true,
    fork in Test := true,
    javaOptions in Test += s"-Dconfig.file=${baseDirectory.value}/src/main/resources/application.conf"
  )

val publishArtifacts = taskKey[Unit]("Publish artifacts only if current user is npgadmin.")
publishArtifacts := Def.taskDyn {
  val log = streams.value.log
  if (sys.env.get("USER").contains("npgadmin"))
    Def.sequential(publish in csr, publish in wmServerDto)
  else
    Def.task(log.warn("Will not publish artifacts! $USER != npgadmin"))
}.value

lazy val testAll = "; all csrMacros/test common/test csr/test root/test tcp/test"
lazy val cleanAll =
  "; common/clean; csr/clean; csrMacros/clean; wmServerDto/clean; root/clean; tcp/clean"
lazy val publishArtifactsLocally = "; csr/publishLocal"
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
