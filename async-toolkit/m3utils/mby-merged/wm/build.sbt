import Dependencies._

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
    libraryDependencies ++= Seq(
      "io.grpc" % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion,
      "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion
    )
  )

sourceGenerators in Compile += Def.task { makeWmServerCode }.taskValue
mainClass in Compile := Some("switch_wm.WhiteModelServer")


PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)

parallelExecution in Test := false
