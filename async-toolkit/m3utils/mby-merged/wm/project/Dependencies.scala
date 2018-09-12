import sbt._

object Dependencies {

  lazy val scalaTest = "org.scalatest" %% "scalatest" % Versions.scalaTest
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
  lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" % Versions.scalaXml
  lazy val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % Versions.parserCombinators
  lazy val scopt = "com.github.scopt" %% "scopt" % Versions.scopt
  lazy val shapeless = "com.chuusai" %% "shapeless" % Versions.shapeless
  lazy val reflect = "org.scala-lang" % "scala-reflect" % Versions.reflect
  lazy val refined = "eu.timepit" %% "refined" % Versions.refined
  lazy val scalaMacros = "org.scalamacros" % "paradise" % Versions.scalaMacros cross CrossVersion.full
  lazy val wmServerDto = "com.intel.cg.hpfd" %% "wm-server-dto" % Versions.wmServerDto changing()
  def csrModel(csrVersion: String): ModuleID = "com.intel.cg.hpfd" %% "csr-model" % csrVersion

  lazy val csrMacrosDeps = Seq(shapeless, refined)

  lazy val commonDeps = Seq(reflect, scalaTest, scalaCheck)

  def whiteModelDeps(csrVersion: String) = Seq(
    scalaTest % Test,
    scalaCheck,
    scalaXml % Compile,
    scalaParserCombinators,
    scopt,
    shapeless,
    csrModel(csrVersion),
    wmServerDto
  )

}
