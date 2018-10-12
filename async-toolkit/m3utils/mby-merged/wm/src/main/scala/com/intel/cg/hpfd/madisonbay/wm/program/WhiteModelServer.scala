//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.program

import java.io._

import com.intel.cg.hpfd.madisonbay.wm.server.dto.Implicits._
import com.intel.cg.hpfd.madisonbay.wm.server.dto._
import java.net._

import com.intel.cg.hpfd.madisonbay.BitVector
import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.Memory._
import monocle.Optional

import scala.annotation.tailrec

object WhiteModelServer {
  val legacyProtocol = false

  val initialAddress: Address = Address at (0 bytes)

  val csrs = mby_top_map(initialAddress)
  val paths: Map[Address, Optional[mby_top_map, BitVector]] = mby_top_map.genOpticsLookup(csrs, Optional.id)

  val egressPortToSocketAndStreamMap = new collection.mutable.HashMap[Int, (Socket, DataOutputStream)]()

  val iosfHandling = new IosfHandling(paths)
  val packetHandling = new PacketHandling(egressPortToSocketAndStreamMap, legacyProtocol)
  val egressInfoHandling = new EgressInfoHandling(egressPortToSocketAndStreamMap, legacyProtocol)

  def processCommandQuit(): Unit = {
    println("Received quit operation!")
  }

  @tailrec
  def processMessages(mtm: mby_top_map, is: DataInputStream, os: DataOutputStream): mby_top_map = {
    val hdr: FmModelMessageHdr = is.readFmModelMessageHdr()

    println ("Processing message with hdr" + hdr)
    val toGo = hdr.Msglength - 12


    val newRegs = hdr.Type match  {
      case FmModelMsgType.Packet => packetHandling.processPacket(is, os, hdr); mtm
      case FmModelMsgType.Mgmt => mtm
      case FmModelMsgType.Iosf => iosfHandling.processIosf(mtm, is, os)
      case FmModelMsgType.SetEgressInfo => egressInfoHandling.processEgressInfo(is, toGo, hdr); mtm
      case FmModelMsgType.CommandQuit => assert(false); mtm
      case _ => mtm
    }

    processMessages(newRegs, is, os)
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
        processMessages(csrs, is, os)
      } catch {
        case _: IOException =>
          println("IO error while handling client " + s.getInetAddress.getHostName)

      }
      println("Disconnected.")
      // cleanup the socket, and remove any egress port mappings to that socket
      is.close()
      os.flush()
      os.close()
      s.close()
    }
    server.close()
  }
}
