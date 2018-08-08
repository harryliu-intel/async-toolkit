import sbt._

object Dependencies {

  lazy val scalaTest = "org.scalatest" %% "scalatest" % Versions.scalaTest
  lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" % Versions.scalaXml
  lazy val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % Versions.parserCombinators
  lazy val scopt = "com.github.scopt" %% "scopt" % Versions.scopt
  lazy val shapeless = "com.chuusai" %% "shapeless" % Versions.shapeless
  lazy val csrModel = "com.intel.cg.hpfd" %% "csr-model" % Versions.csrModel

  lazy val whiteModelDeps = Seq(
    scalaTest % Test,
    scalaXml % Compile,
    scalaParserCombinators,
    scopt,
    shapeless
    // TODO: uncomment when whole pipeline with artifactory would be enabled
    // csrModel
  )

}
