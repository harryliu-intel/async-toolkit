import sbt.Def._
import sbt.Keys._
import sbt.{AutoPlugin, Def, Keys, ThisBuild}

import scala.sys.process._

object RdlGitHashPlugin extends AutoPlugin {

  private val rdlDirectory = "tools/srdl"
  private val shortHashLength = 8


  object autoImport {
    val gitHashProvider = settingKey[GitHashProvider]("GitTopHashProvider for this plugin.")
    val rdlGitHashLast = settingKey[String]("Version of the csr project based on the git hash from/tools/srdl directory.")
    val rdlGitHashLastShort = settingKey[String](
      s"Version of the csr project based on the git hash from/tools/srdl catalog. Hash of length $shortHashLength."
    )
    val rdlGitHashShortProjectVersion = settingKey[String](s"Project version including $rdlDirectory top git hash.")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    gitHashProvider               := GitHashProvider(s"${(Keys.baseDirectory in ThisBuild).value.toString}/../$rdlDirectory"),
    rdlGitHashLast                := gitHashProvider.value.getLastHash,
    rdlGitHashLastShort           := gitHashProvider.value.getLastHash.take(shortHashLength),
    rdlGitHashShortProjectVersion := (version in ThisBuild).zipWith(rdlGitHashLastShort)((v, h) => s"$h-$v").value
  )
}

case class GitHashProvider(path: String) {
  def getLastHash: String = s"git log --pretty=format:'%H' -n 1 $path".!!.replace("'", "")
}
