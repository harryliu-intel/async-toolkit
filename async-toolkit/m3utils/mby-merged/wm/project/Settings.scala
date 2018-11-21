import sbt.Keys._
import sbt.librarymanagement.ivy.Credentials
import sbt.librarymanagement.syntax._
import sbt.{Def, inThisBuild}
import scalafix.sbt.ScalafixPlugin.autoImport._
import sbt.addCompilerPlugin

object Settings {

  val csrName = "csr-model"
  val wmServerDtoName = "wm-server-dto"
  val csrMacrosName = "csr-macros"
  val csrMacroTestsName = "csr-macro-tests"
  val commonName = "common"
  val rootName = "wm"

  val intelOrganization = "com.intel.cg.hpfd"

  val artifactoryResolver =
    "Artifactory Realm" at "https://ubit-artifactory-or.intel.com/artifactory/dcg-cg-hpfd-or-local/"

  val scalacOpts = Seq(
    "-feature",
    "-deprecation",
    "-encoding", "utf-8",            // Specify character encoding used by source files.
    "-explaintypes",                 // Explain type errors in more detail.
    "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds",         // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-language:postfixOps",          // Enable postfix operators
    "-Xfatal-warnings",              // Warnings to compile errors
    "-Xlint:unsound-match",          // Pattern match may not be typesafe.
    "-Ypartial-unification",         // Enable partial unification in type constructor inference
    "-Ywarn-infer-any",              // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-unused:implicits",       // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",         // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",          // Warn if a local definition is unused.
    "-Ywarn-unused:params",          // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",         // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",        // Warn if a private member is unused.
    "-Ywarn-value-discard",          // Warn when non-Unit expression results are unused.
    "-Ywarn-dead-code",              // Warn about dead code
    "-Yrangepos"                     // Required by scalafix
  )

  val testDisabledOpts = Seq(
    "-Ywarn-value-discard"           // _ shouldEqual _ produces Assertion type
  )

  val commonSettings: Seq[Def.Setting[_]] = inThisBuild(Seq(
    organization := intelOrganization,
    version := Versions.applicationVersion,
    scalaVersion := Versions.scalaVersion,
    publishTo := Some(artifactoryResolver),
    credentials += Credentials(
      "Artifactory Realm",
      "ubit-artifactory-or.intel.com",
      "ddagiel",
      "APBW9qc5WhKpG6PgYnwcL17wsyz"
    ),
    resolvers += artifactoryResolver,
    parallelExecution in Test := false,
    scalacOptions in Compile ++= scalacOpts.toSeq,
    scalacOptions in Test ++= (scalacOpts diff testDisabledOpts).toSeq,
    addCompilerPlugin(scalafixSemanticdb)
  )) ++ Seq(
    (compile in Compile) := ((compile in Compile) dependsOn scalafix.in(Compile).toTask("")).value,
    (compile in Test) := ((compile in Test) dependsOn scalafix.in(Test).toTask("")).value
  )
}
