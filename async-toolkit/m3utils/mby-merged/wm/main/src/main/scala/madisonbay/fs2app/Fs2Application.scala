package madisonbay
package fs2app

import fs2._
import fs2.concurrent.{ Queue, SignallingRef }
import fs2.io.tcp._
import cats.effect.{ Concurrent, ConcurrentEffect }
import cats.effect.concurrent.Ref
import java.net.InetSocketAddress
import java.net.InetAddress

import madisonbay.logger.Logger
import madisonbay.fs2app.algebra._
import madisonbay.fs2app.algebra.messages._
import madisonbay.fs2app.deserialization._


import scalaz.MonadError
import scalaz.StateT
import scalaz.syntax.all._

import java.nio.channels.AsynchronousChannelGroup

object Fs2Application {

  def fs2ServerSocket[F[_]: Logger: ConcurrentEffect: Concurrent]
    (implicit acg: AsynchronousChannelGroup): ServerSocket[F] =
    new ServerSocket[F] {
      def create: Stream[F,Socket[F]] = {
        val CF = Concurrent[F]
        val logger = Logger[F]

        val serverPort = 5888
        val serverHostname = InetAddress.getLocalHost().getHostName()
        val isa = new InetSocketAddress(serverHostname, serverPort)

        for {
          _        <- Stream.eval(logger.info(s"Server should be started at [$serverHostname:$serverPort]! Good luck!"))
          _        <- Stream.bracket(CF.delay(acg))(asyncCg => CF.delay(asyncCg.shutdown))
          resource <- server[F](bind = isa)
          socket   <- Stream.resource(resource)
        } yield socket
      }
    }

  def fs2PublisherSocket[F[_]: ConcurrentEffect: Concurrent]
    (implicit acg: AsynchronousChannelGroup): PublisherSocket[F] =
    new PublisherSocket[F] {
      def create(egressSocketInfo: EgressSocketInfo): Stream[F,Socket[F]] = {
        val EgressSocketInfo(hostname, port) = egressSocketInfo
        val clientIsa = new InetSocketAddress(InetAddress.getByName(hostname), port)
        Stream.resource(client[F](to = clientIsa, noDelay = true))
      }
    }

  private class Application[
    RegistersState,
    F[_]:
        Logger:
        ConcurrentEffect:
        Concurrent:
        ServerSocket:
        PublisherSocket:
        λ[G[_] => MonadError[G,Throwable]]:
        λ[G[_] => MessageHandler[G,RegistersState]]
  ](stateR: Ref[F,RegistersState],
    egressSocketInfoQ: Queue[F,EgressSocketInfo],
    quitS: SignallingRef[F,Boolean]) {

    val logger = Logger[F]
    val serverSocket = ServerSocket[F]
    val publisherSocket = PublisherSocket[F]
    val messageHandler = MessageHandler[F,RegistersState]
    val me = MonadError[F,Throwable]

    def serverStream: Stream[F,Stream[F,Unit]] =
      for {
        socket     <- serverSocket.create
        bufferSize  = 16384 //scalastyle:ignore magic.number
      } yield socket.reads(bufferSize)
        .chunks
        .map(_.toArray)
        .map(deserialize[F])
        .evalMap(identity)
        .flatMap(identity)
        .through(messageDispatcher)
        .through(errorHandler)
        .flatMap(Stream(_: _*))
        .to(socket.writes())

    def publisherStream: Stream[F,Stream[F,Unit]] =
      egressSocketInfoQ.dequeue
        .flatMap(publisherSocket.create)
        .map(socket => Stream.eval(socket.close))

    private val emptyResponse: Any => Array[Byte] = _ => Array.empty[Byte]

    private def errorHandler: Pipe[F,Array[Byte],Array[Byte]] =
      _.attempt.evalMap(
        _.fold(
          error => logger.error(
            s"""|Error occured: ${error.getMessage}
                |${error.getStackTrace().map(_.toString).mkString("\n")}
                |""".stripMargin
          ).map(emptyResponse),
          _.point[F]
        )
      )

    private def messageDispatcher: Pipe[F,Message,Array[Byte]] = {
      def stateTransition(stateTransition: StateT[F,RegistersState,Array[Byte]]): F[Array[Byte]] = for {
        s <- stateR.get
        result <- stateTransition.run(s)
        (next,arr) = result
        _ <- stateR.set(next)
      } yield (arr)

      _.evalMap {
        case io  @ IosfRegBlkWrite(_, _) => stateTransition(messageHandler.regBlkWrite(io))
        case io  @ IosfRegWrite(_)       => stateTransition(messageHandler.regWrite(io))
        case io  @ IosfRegRead(_)        => stateTransition(messageHandler.regRead(io))
        case p   @ Packets(_)            => stateTransition(messageHandler.packets(p))
        case esi @ EgressSocketInfo(_,_) =>
          messageHandler.egressSocketInfo(esi)
            .flatMap(_ => egressSocketInfoQ.enqueue1(esi))
            .map(emptyResponse)
        case NotSupported                =>
          messageHandler.notSupported.map(emptyResponse)
        case Quit                        =>
          messageHandler.quit.flatMap(_ => quitS.set(true)).map(emptyResponse)
      }
    }
  }

  def fs2Program[
    RegistersState,
    F[_]:
      Logger:
      Concurrent:
      ConcurrentEffect:
      ServerSocket:
      PublisherSocket:
      λ[G[_] => MonadError[G,Throwable]]:
      λ[G[_] => MessageHandler[G,RegistersState]]
  ](init: RegistersState): F[Unit] = {
    val queueSize = 1

    val stream = for {
      stateR    <- Stream.eval(Ref.of[F,RegistersState](init))
      egressQ   <- Stream.eval(Queue.bounded[F,EgressSocketInfo](queueSize))
      quitS     <- Stream.eval(SignallingRef[F,Boolean](false))
      app        =  new Application[RegistersState,F](stateR, egressQ, quitS)
      maxStreams =  10 //scalastyle:ignore magic.number
      _       <- {
        import app._
        (serverStream concurrently publisherStream)
          .interruptWhen(quitS)
          .parJoin(maxStreams)
      }
    } yield ()

    stream.compile.drain
  }

}
