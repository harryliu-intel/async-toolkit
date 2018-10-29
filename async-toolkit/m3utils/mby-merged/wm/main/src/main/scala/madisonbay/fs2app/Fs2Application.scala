package madisonbay
package fs2app

import fs2._
import fs2.concurrent.{ Queue, SignallingRef }
import fs2.io.tcp._
import cats.effect.{ Concurrent, ConcurrentEffect }
import java.net.InetSocketAddress
import java.net.InetAddress

import madisonbay.logger.Logger
import madisonbay.fs2app.algebra.messages._
import madisonbay.fs2app.deserialization._
import madisonbay.fs2app.algebra._

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
          _        <- Stream.eval(logger.info(s"Starting server..."))
          _        <- Stream.bracket(CF.delay(acg))(asyncCg => CF.delay(asyncCg.shutdown))
          resource <- server[F](bind = isa)
          _        <- Stream.eval(logger.info(s"Server started at $serverHostname:$serverPort"))
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

  private class Application[F[_]:
      Logger:
      ConcurrentEffect:
      Concurrent:
      ServerSocket:
      PublisherSocket:
      λ[G[_] => MessageHandler[G,Int]]
  ](egressSocketInfoQ: Queue [F,EgressSocketInfo], quitS: SignallingRef[F,Boolean])
    (implicit me: MonadError[F,Throwable]) {

    val logger = Logger[F]
    val serverSocket = ServerSocket[F]
    val publisherSocket = PublisherSocket[F]
    val messageHandler = MessageHandler[F,Int]

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
        .map(array => socket.write(Chunk.boxed(array)))
        .evalMap(identity)

    def publisherStream: Stream[F,Stream[F,Unit]] =
      egressSocketInfoQ.dequeue
        .flatMap(publisherSocket.create)
        .map(socket => Stream.eval(socket.close))

    private def messageDispatcher: Pipe[F,Message,Array[Byte]] = {
      def stateTransition(state: StateT[F,Int,Array[Byte]]): Stream[F,Array[Byte]] =
        Stream.eval(state.run(0).map { case (_,arr) => arr })

      in => in.flatMap {
        case io  @ IosfRegBlkWrite(_, _) => stateTransition(messageHandler.regBlkWrite(io))
        case io  @ IosfRegWrite(_)       => stateTransition(messageHandler.regWrite(io))
        case io  @ IosfRegRead(_)        => stateTransition(messageHandler.regRead(io))
        case p   @ Packets(_)            => stateTransition(messageHandler.packets(p))
        case esi @ EgressSocketInfo(_,_) => Stream.eval(
          messageHandler.egressSocketInfo(esi)
            .flatMap(_ => egressSocketInfoQ.enqueue1(esi))
            .map(_ => Array.empty[Byte])
        )
        case NotSupported                => Stream.eval(
          messageHandler.notSupported
            .map(_ => Array.empty[Byte])
        )
        case Quit                        => Stream.eval(
          messageHandler.quit
            .flatMap(_ => quitS.set(true))
            .map(_ => Array.empty[Byte])
        )
      }
    }
  }

  def fs2Program[F[_]:
      Logger:
      Concurrent:
      ConcurrentEffect:
      ServerSocket:
      PublisherSocket:
      λ[G[_] => MonadError[G,Throwable]]:
      λ[G[_] => MessageHandler[G,Int]]
  ]: F[Unit] = {
    val queueSize = 1

    val stream = for {
      egressQ <- Stream.eval(Queue.bounded[F,EgressSocketInfo](queueSize))
      quitS   <- Stream.eval(SignallingRef[F,Boolean](false))
      app     =  new Application[F](egressQ, quitS)
      par     =  10 //scalastyle:ignore magic.number
      _       <- {
        import app._
        (serverStream concurrently publisherStream)
          .interruptWhen(quitS)
          .parJoin(par)
      }
    } yield ()

    stream.compile.drain
  }

}
