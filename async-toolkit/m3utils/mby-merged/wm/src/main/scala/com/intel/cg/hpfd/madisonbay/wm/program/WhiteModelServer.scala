//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.program

import java.io._

import com.intel.cg.hpfd.madisonbay.wm.server.dto.Implicits._
import com.intel.cg.hpfd.madisonbay.wm.server.dto._

object WhiteModelServer {
  val legacyProtocol = false

  // prototype example! -- obviously this ought to be the 'real' CSR state/white model
  val csrSpace = new scala.collection.mutable.HashMap[Int, Byte]

  import java.net._
  val egressPortToSocketAndStreamMap = new collection.mutable.HashMap[Int, (Socket, DataOutputStream)]()

  val iosfHandling = new IosfHandling(csrSpace)
  val packetHandling = new PacketHandling(egressPortToSocketAndStreamMap, legacyProtocol)
  val egressInfoHandling = new EgressInfoHandling(egressPortToSocketAndStreamMap, legacyProtocol)

  def processCommandQuit(): Unit = {
    println("Received quit operation!")
  }

  def processMessage(is: DataInputStream, os: DataOutputStream): Any = {

    val hdr: FmModelMessageHdr = is.readFmModelMessageHdr()

    println ("Processing message with hdr" + hdr)
    val toGo = hdr.Msglength - 12


    hdr.Type match  {
      case FmModelMsgType.Packet => packetHandling.processPacket(is, os, hdr)
      case FmModelMsgType.Mgmt => Unit
      case FmModelMsgType.Iosf => iosfHandling.processIosf(is, os)
      case FmModelMsgType.SetEgressInfo => egressInfoHandling.processEgressInfo(is, toGo, hdr)
      case FmModelMsgType.CommandQuit => assert(false)
      case _ =>
    }
  }

  def runModelServer(file: File): Unit = {
    val server = new ServerSocket(0) // 0 means pick any available port

    val port = server.getLocalPort
    val hostname = InetAddress.getLocalHost.getCanonicalHostName
    val descText = "0:" + hostname + ":" + port
    println("Scala White Model Server for Madison Bay Switch Chip")
    println("Socket port open at " +  descText)
    println("Write server description file to " + file)
    val psFile = new FileWriter(file)
    psFile.write(descText)
    psFile.write("\n")
    psFile.close()
    file.deleteOnExit()

    while (true) {
      val s: Socket = server.accept()
      s.setTcpNoDelay(true)
      println("Accepted new connection:" + s)
      // for some reason, doesn't appear to work if Buffered* is removed from the stack
      // this is suspicious
      val is = new DataInputStream(new BufferedInputStream(s.getInputStream))
      val os = new DataOutputStream(new BufferedOutputStream(s.getOutputStream))

      try {
        while (true) processMessage(is, os)
      } catch {
        case _: EOFException =>
          println("Termination of IO from client without shutdown command " + s.getInetAddress.getHostName)

      }
      println("Disconnected.")
      // cleanup the socket, and remove any egress port mappings to that socket
      is.close()
      os.flush()
      os.close()
      s.close()
      egressPortToSocketAndStreamMap.retain {
        case (_, (socket, _)) => socket != s
      }
    }
    server.close()
  }
}
