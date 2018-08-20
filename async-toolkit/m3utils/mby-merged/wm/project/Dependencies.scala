import sbt._

object Dependencies {

  lazy val scalaTest = "org.scalatest" %% "scalatest" % Versions.scalaTest
  lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" % Versions.scalaXml
  lazy val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % Versions.parserCombinators
  lazy val scopt = "com.github.scopt" %% "scopt" % Versions.scopt
  lazy val shapeless = "com.chuusai" %% "shapeless" % Versions.shapeless
  lazy val reflect = "org.scala-lang" % "scala-reflect" % Versions.reflect
  lazy val refined = "eu.timepit" %% "refined" % Versions.refined
  lazy val scalaMacros = "org.scalamacros" % "paradise" % Versions.scalaMacros cross CrossVersion.full
  lazy val csrModel = "com.intel.cg.hpfd" %% "csr-model" % Versions.csrModel changing()
  lazy val wmServerDto = "com.intel.cg.hpfd" %% "wm-server-dto" % Versions.wmServerDto changing()

  lazy val csrMacrosDeps = Seq(shapeless, refined)

  lazy val commonDeps = Seq(reflect)

  lazy val whiteModelDeps = Seq(
    scalaTest % Test,
    scalaXml % Compile,
    scalaParserCombinators,
    scopt,
    shapeless,
    csrModel,
    wmServerDto
  )

}
