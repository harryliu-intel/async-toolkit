package madisonbay
package config

import pureconfig.ConfigReader
import java.util.concurrent.TimeUnit

trait Config[F[_]] {
  def string(path: String): F[String]
  def boolean(path: String): F[Boolean]
  def int(path: String): F[Int]
  def double(path: String): F[Double]
  def stringList(path: String): F[List[String]]
  def duration(path: String, unit: TimeUnit): F[Long]
  def as[T: ConfigReader]: F[T]
}
object Config {
  def apply[F[_]](implicit conf: Config[F]): Config[F] = conf
}

trait ConfigLoader[F[_]] {
  def load(prefix: String = ""): Config[F]
}

