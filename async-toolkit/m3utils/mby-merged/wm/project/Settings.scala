import sbt.Keys._
import sbt.librarymanagement.ivy.Credentials
import sbt.librarymanagement.syntax._
import sbt.{Def, inThisBuild}
import org.scalastyle.sbt.ScalastylePlugin.autoImport._

object Settings {
  val artifactoryResolver =
    "Artifactory Realm" at "https://ubit-artifactory-or.intel.com/artifactory/dcg-cg-hpfd-or-local/"

  val commonSettings: Seq[Def.Setting[_]] = inThisBuild(Seq(
    organization := "com.intel.cg.hpfd",
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
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-encoding", "utf-8",            // Specify character encoding used by source files.
      "-explaintypes",                 // Explain type errors in more detail.
      "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
      "-language:experimental.macros", // Allow macro definition (besides implementation and application)
      "-language:higherKinds",         // Allow higher-kinded types
      "-language:implicitConversions", // Allow definition of implicit functions called views
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
      "-Ywarn-dead-code"               // Warn about dead code
    )
    // enable publishing only for npgadmin user
    //    streams in publish := Def.sequential(
    //      Def.task {
    //        val user = sys.env.get("USER")
    //        require(user.contains("npgadmin"), "Publish check failed. Only npgadmin can publish artifacts!")
    //      },
    //      streams in publish
    //    ).value
  )) ++ Seq(
    scalastyleFailOnError := true,
    scalastyleFailOnWarning := true,
    (compile in Compile) := ((compile in Compile) dependsOn scalastyle.in(Compile).toTask("")).value,
    (compile in Test) := ((compile in Test) dependsOn scalastyle.in(Test).toTask("")).value
  )
}
