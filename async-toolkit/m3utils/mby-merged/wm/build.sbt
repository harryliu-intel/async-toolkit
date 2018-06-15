import Dependencies._
//


def makeWmServerCode : Seq[File] = {
  import sys.process._
  import sbt.io._
  println("Generating WM server from Scheme")
  val m3dir = new File("src/main/m3")
  val result : Int = "make -C src/main/m3 rpc_structs".!
  require(result == 0, "Scheme-based generation step failed")
  val pf = (m3dir / "wm_net") ** "*.scala"
  pf.get.map(x => new File(x.getCanonicalPath))
}



lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.intel",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "wm",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaXml % Compile,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
    libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"
    //libraryDependencies ++= Seq(
    //  "io.grpc" % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion,
    //  "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion
    //)
  )

sourceGenerators in Compile += Def.task { makeWmServerCode }.taskValue
mainClass in Compile := Some("switch_wm.WhiteModelServer")


// PB.targets in Compile := Seq(
//  scalapb.gen() -> (sourceManaged in Compile).value
//)

// below, to enable IDEA to automatically context-complete with content from these the scheme-based generator
managedSourceDirectories in Compile += file("src/main/m3/wm_net/scala_generated")
unmanagedSourceDirectories in Compile += file("src/main/m3/wm_net/scala_src")


parallelExecution in Test := false
mainClass in assembly := Some("switch_wm.WhiteModelServer")
// assemblyJarName in assembly := "mbay_wm.jar"

val path =  new File(sys.env("MODEL_ROOT") + "/target/GenRTL/wm/mbay_wm.jar")
// val success = path.mkdirs()
assemblyOutputPath in assembly := path
