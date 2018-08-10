import sbt.Keys.{managedSourceDirectories, unmanagedSourceDirectories}
import sbt.file

def makeWmServerCode: Seq[File] = {
  import sys.process._
  println("Generating WM server from Scheme")
  val m3dir = new File("src/main/m3")
  val result : Int = "make -C src/main/m3 rpc_structs".!
  require(result == 0, "Scheme-based generation step failed")
  val pf = (m3dir / "wm_net" / "scala_generated") ** "*.scala"
  pf.get.map(x => new File(x.getCanonicalPath))
}

lazy val path = new File(sys.env("MODEL_ROOT") + "/target/GenRTL/wm/mbay_wm.jar")

lazy val csr = project in file("csr")

lazy val root = (project in file("."))
  .settings(
    Settings.commonSettings,
    name := "wm",
    libraryDependencies ++= Dependencies.whiteModelDeps,
    managedSourceDirectories in Compile += file("src/main/m3/wm_net/scala_generated"),
    unmanagedSourceDirectories in Compile += file(s"${baseDirectory.value}/src/main/m3/wm_net/scala_src"),
    mainClass in Compile := Some("switch_wm.WhiteModelServer"),
    mainClass in assembly := Some("switch_wm.WhiteModelServer"),
    assemblyOutputPath in assembly := path,
    sourceGenerators in Compile += Def.task { makeWmServerCode }.taskValue
  )

val buildOnNhdk = taskKey[Unit]("Build task for nhdk environment.")
buildOnNhdk := Def.sequential(
  clean in csr,
  clean in root,
  publishLocal in csr,
  update in root,
  compile in Compile in root,
  doc in Compile in root,
  assembly in root,
  publish in root,
  publish in csr
).value
