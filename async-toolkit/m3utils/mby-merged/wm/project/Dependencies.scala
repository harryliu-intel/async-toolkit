import sbt._

object Dependencies {

  lazy val scalaTest = "org.scalatest" %% "scalatest" % Versions.scalaTest
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"
  lazy val scopt = "com.github.scopt" %% "scopt" % Versions.scopt
  lazy val shapeless = "com.chuusai" %% "shapeless" % Versions.shapeless
  lazy val reflect = "org.scala-lang" % "scala-reflect" % Versions.reflect
  lazy val refined = "eu.timepit" %% "refined" % Versions.refined
  lazy val scalaMacrosParadise = "org.scalamacros" % "paradise" % Versions.macroParadise cross CrossVersion.full
  lazy val monocleCore = "com.github.julien-truffaut" %%  "monocle-core"  % Versions.monocle
  lazy val monocleMacro = "com.github.julien-truffaut" %%  "monocle-macro" % Versions.monocle
  lazy val csrModel = "com.intel.cg.hpfd" %% "csr-model" % Versions.csrModel changing()
  lazy val monocleState = "com.github.julien-truffaut" %%  "monocle-state" % Versions.monocle
  lazy val scalaz = "org.scalaz" %% "scalaz-core" % Versions.scalaz
  lazy val wmServerDto = "com.intel.cg.hpfd" %% "wm-server-dto" % Versions.wmServerDto changing()
  def csrModel(csrVersion: String): ModuleID = "com.intel.cg.hpfd" %% "csr-model" % csrVersion

  lazy val csrMacrosDeps = Seq(shapeless, refined)

  lazy val commonDeps = Seq(reflect, scalaTest % "test", scalaCheck % "test")
  lazy val csrDeps = Seq(monocleCore, monocleMacro, scalaTest % "test", scalaCheck % "test")
  def whiteModelDeps(csrVersion: String) = Seq(
    scalaTest % "test",
    scalaCheck % "test",
    scopt,
    shapeless,
    csrModel(csrVersion),
    wmServerDto,
    scalaz,
    monocleCore,
    monocleState
  )

}
