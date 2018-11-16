import sbt._

object Dependencies {

  lazy val scalaTest = "org.scalatest" %% "scalatest" % Versions.scalaTest
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % Versions.scalaCheck
  lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" % Versions.scalaXml
  lazy val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % Versions.parserCombinators
  lazy val scopt = "com.github.scopt" %% "scopt" % Versions.scopt
  lazy val shapeless = "com.chuusai" %% "shapeless" % Versions.shapeless
  lazy val reflect = "org.scala-lang" % "scala-reflect" % Versions.reflect
  lazy val refined = "eu.timepit" %% "refined" % Versions.refined
  lazy val scalaMacrosParadise = "org.scalamacros" % "paradise" % Versions.macroParadise cross CrossVersion.full
  lazy val kindProjector = "org.spire-math" %% "kind-projector" % Versions.kindProjector
  lazy val monocleCore = "com.github.julien-truffaut" %%  "monocle-core"  % Versions.monocle
  lazy val monocleMacro = "com.github.julien-truffaut" %%  "monocle-macro" % Versions.monocle
  lazy val fs2 = "co.fs2" %% "fs2-core" % Versions.fs2
  lazy val fs2io = "co.fs2" %% "fs2-io" % Versions.fs2
  lazy val csrModel = "com.intel.cg.hpfd" %% "csr-model" % Versions.csrModel changing()
  lazy val monocleState = "com.github.julien-truffaut" %%  "monocle-state" % Versions.monocle
  lazy val scalaz = "org.scalaz" %% "scalaz-core" % Versions.scalaz
  lazy val wmServerDto = "com.intel.cg.hpfd" %% "wm-server-dto" % Versions.wmServerDto changing()
  lazy val jackson = "com.fasterxml.jackson.module" %% "jackson-module-scala" % Versions.jackson
  lazy val sourcecode = "com.lihaoyi" %% "sourcecode" % Versions.sourcecode
  lazy val logback = "ch.qos.logback" % "logback-classic" % Versions.logback
  lazy val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % Versions.scalaLogging
  lazy val pureConfig = "com.github.pureconfig" %% "pureconfig" % Versions.pureconfig
  lazy val spinoco = "com.spinoco" %% "fs2-http" % Versions.spinoco
  def csrModel(csrVersion: String): ModuleID = "com.intel.cg.hpfd" %% "csr-model" % csrVersion

  lazy val csrMacrosDeps = Seq(shapeless, refined)
  lazy val commonDeps = Seq(
    monocleCore,
    monocleMacro,
    shapeless,
    reflect,
    scalaTest % "test",
    scalaCheck % "test"
  )
  lazy val csrDeps = Seq(
    monocleCore,
    monocleMacro,
    scalaTest % "test",
    scalaCheck % "test"
  )
  lazy val tcpDeps = Seq(shapeless, scalaTest % "test", scalaz)
  def whiteModelDeps(csrVersion: String) = Seq(
    scalaTest % "test",
    scalaCheck % "test",
    shapeless,
    scalaz,
    monocleCore,
    monocleState,
    jackson,
    fs2,
    fs2io,
    sourcecode,
    logback,
    scalaLogging,
    pureConfig,
    spinoco,
    csrModel(csrVersion),
    // use test code as dependency
    // "compile->compile;test->test" doesn't work
    csrModel(csrVersion) % "test" classifier("tests")
  )

}
