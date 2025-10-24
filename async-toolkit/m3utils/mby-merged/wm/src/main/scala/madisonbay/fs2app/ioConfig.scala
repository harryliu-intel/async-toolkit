package madisonbay
package fs2app

import cats.effect.IO
import java.util.concurrent.TimeUnit
import madisonbay.config.{Config, ConfigLoader}
import com.typesafe.config.{ Config => TypesafeConfig }
import scala.collection.JavaConverters._
import pureconfig._
import pureconfig.error._
import pureconfig.generic.auto._

package object ioConfig {

  implicit object IOPureConfigLoader extends ConfigLoader[IO] {

    def load(prefix: String = ""): Config[IO] = {
      val failuresToThrowable: ConfigReaderFailures => Throwable =
        crf => new Throwable(crf.toString)

      val ioConf = IO.suspend(
        IO.fromEither(
          loadConfig[TypesafeConfig](prefix).left.map(failuresToThrowable)
        )
      )

      new Config[IO] {
        def as[T: ConfigReader]: IO[T] = for {
          config <- ioConf
          either  = loadConfig[T](config, prefix).left.map(failuresToThrowable)
          t      <- IO.fromEither(either)
        } yield t
        def boolean(path: String): IO[Boolean] = ioConf.map(_.getBoolean(path))
        def double(path: String): IO[Double] = ioConf.map(_.getDouble(path))
        def duration(path: String, unit: TimeUnit): IO[Long] = ioConf.map(_.getDuration(path, unit))
        def int(path: String): IO[Int] = ioConf.map(_.getInt(path))
        def string(path: String): IO[String] = ioConf.map(_.getString(path))
        def stringList(path: String): IO[List[String]] = ioConf.map(_.getStringList(path).asScala.toList)
      }
    }
  }
}

