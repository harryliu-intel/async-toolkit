package madisonbay.fs2app

import java.net.{InetAddress, InetSocketAddress}
import java.nio.channels.AsynchronousChannelGroup

import cats.effect.{ConcurrentEffect, Timer}
import fs2.Stream
import madisonbay.config.Config
import madisonbay.logger.Logger
import spinoco.fs2.http.{HttpResponse, HttpServer}
import spinoco.protocol.http.{HttpRequestHeader, HttpStatusCode}


package object http {

  def fs2HttpStream[F[_]: Logger: Config: UriDispatcher: ConcurrentEffect: Timer]
    (implicit acg: AsynchronousChannelGroup): MbyHttpServer[F] =
    new MbyHttpServer[F] {
      val logger        = Logger[F]
      val config        = Config[F]
      val uriDispatcher = UriDispatcher[F]

      override def create: Stream[F, Stream[F, Unit]] =
        for {
          port      <- Stream.eval(config.int("http.port"))
          hostname  <- Stream.eval(config.string("http.hostname"))
          inetAddr  = InetAddress.getByName(hostname)
          isa       = new InetSocketAddress(inetAddr, port)
          _         <- Stream.eval(logger.info(s"Http server should be started at [$hostname:$port]! Good luck!"))
        } yield HttpServer[F](
          bindTo = isa,
          service = httpService,
          requestFailure = handleFs2RequestParseError _,
          sendFailure = HttpServer.handleSendFailure[F] _
        )

      def httpService(request: HttpRequestHeader, body: Stream[F,Byte]): Stream[F,HttpResponse[F]] = {
        val result = uriDispatcher.processRequest(request, body)
        Stream.emit(result)
      }

      def handleFs2RequestParseError(err: Throwable): Stream[F, HttpResponse[F]] = {
        Stream.emit(HttpResponse(HttpStatusCode.BadRequest).withUtf8Body(s"Request fail: ${err.getMessage}"))
      }

    }

}
