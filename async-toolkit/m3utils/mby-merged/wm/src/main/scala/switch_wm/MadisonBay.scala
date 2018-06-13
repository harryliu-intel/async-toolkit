package switch_wm
/*
import java.util.logging.Logger

import io.grpc.{Server, ServerBuilder}
import switch_wm.whitemodel._

import scala.concurrent.{ExecutionContext, Future}

object MadisonBay {
  private val logger = Logger.getLogger(classOf[MadisonBay].getName)

  def main(args: Array[String]): Unit = {
    val server = new MadisonBay(ExecutionContext.global)
    server.start()
    server.blockUntilShutdown()
  }

  private val port = 50051
}

class MadisonBay (executionContext: ExecutionContext) { self =>
  private[this] var server: Server = null

  private def start(): Unit = {
    server = ServerBuilder.forPort(MadisonBay.port).addService(WhiteModelServerGrpc.bindService(new MadisonBayImpl, executionContext)).build.start
    MadisonBay.logger.info("Server started, listening on " + MadisonBay.port)
    sys.addShutdownHook {
      System.err.println("*** shutting down gRPC server since JVM is shutting down")
      self.stop()
      System.err.println("*** server shut down")
    }
  }

  private def stop(): Unit = {
    if (server != null) {
      server.shutdown()
    }
  }

  private def blockUntilShutdown(): Unit = {
    if (server != null) {
      server.awaitTermination()
    }
  }

  class MadisonBayImpl extends WhiteModelServerGrpc.WhiteModelServer {
    def readRegister(request: ConfigurationOperation): Future[ConfigurationResult] = {
      println("Received configuration operation from client" + request)
      Future.successful(ConfigurationResult(true, 0))
    }
    def setRegister(request: ConfigurationOperation): Future[ConfigurationResult] = {
      println("Received configuration read operation from client" + request)
      Future.successful(ConfigurationResult(true, 0))
    }

    def deliverPacket(request: switch_wm.whitemodel.Packet): Future[ConfigurationResult] = {
      println("Received packet from client for port " + request.port)
      Future.successful(ConfigurationResult(true, 0))
    }
  }
}
*/