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
      "kczulko",
      "AP39GKaJqAVMRgQ1xiYzA82NrHX"
    ),
    resolvers += artifactoryResolver,
    parallelExecution in Test := false,
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
