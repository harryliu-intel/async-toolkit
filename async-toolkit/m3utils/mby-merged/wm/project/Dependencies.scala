import sbt._

object Dependencies {

  lazy val scalaTest = "org.scalatest" %% "scalatest" % Versions.scalaTest
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % Versions.scalaCheck
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
  def csrModel(csrVersion: String): ModuleID = "com.intel.cg.hpfd" %% "csr-model" % csrVersion

  lazy val csrMacrosDeps = Seq(shapeless, refined)

  lazy val commonDeps = Seq(monocleCore, monocleMacro, shapeless, reflect, scalaTest % "test", scalaCheck % "test")
  lazy val csrDeps = Seq(monocleCore, monocleMacro, scalaTest % "test", scalaCheck % "test")
  lazy val tcpDeps = Seq(shapeless, scalaTest % "test", scalaz)
  def mainDeps(csrVersion: String) = Seq(fs2, fs2io, csrModel(csrVersion))
  def whiteModelDeps(csrVersion: String) = Seq(
    scalaTest % "test",
    scalaCheck % "test",
    scopt,
    shapeless,
    csrModel(csrVersion),
    wmServerDto,
    scalaz,
    monocleCore,
    monocleState,
    jackson
  )

}
