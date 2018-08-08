import sbt.Keys._
import sbt.librarymanagement.ivy.Credentials
import sbt.librarymanagement.syntax._

object Settings {
  val artifactoryResolver = "Artifactory Realm" at "https://ubit-artifactory-or.intel.com/artifactory/dcg-cg-hpfd-or-local/"

  val commonSettings = Seq(
    organization := "com.intel.cg.hpfd",
    version := Versions.applicationVersion,
    scalaVersion := Versions.scalaVersion,
    publishTo := Some(artifactoryResolver),
    credentials += Credentials(
      "Artifactory Realm",
      "ubit-artifactory-or.intel.com",
      "kczulko",
      "AP39GKaJqAVMRgQ1xiYzA82NrHX"
    ),
    resolvers += artifactoryResolver,
    parallelExecution in Test := false
  )
}
