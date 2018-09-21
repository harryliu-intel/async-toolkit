//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.program

import java.io.{BufferedOutputStream, DataInputStream, DataOutputStream}
import java.net.{InetAddress, Socket}

import com.intel.cg.hpfd.madisonbay.wm.server.dto._
import com.intel.cg.hpfd.madisonbay.wm.server.dto.Implicits._

import scala.collection.mutable.HashMap

class EgressInfoHandling(egressPortToSocketAndStreamMap: HashMap[Int, (Socket, DataOutputStream)],
                         legacyProtocol: Boolean)  {
  /** Implement configuration of an egress port
    *
    * In 'new' protocol, a port-machine tuple defines a socket as well as a connection. I.e. if ports 1 and 2 are
    * connected to machine sccj0010 on port 8000 then only a single socket connection is created.
    *
    * After each ingress process is completed being processed, an Eot signal is sent on _all_ opened conections, including
    * the 'original' server socket.
    *
    * The most typical usage of this should be that clients provide their own hostname and the port of the port of their
    * _already opened_ socket (i.e. the IOSF, etc., communication channel). But this approach admits the possibility of
    * communication against several 'clients' at the same time. A client timeout in this model (waiting for an EOT) indicates
    * that an error condition exists.
    *
    * In the legacy protocol, new connections are opened on every egress info signal. No EOTs are sent, the model
    * depends on timing out on the connections to decide there is no more data to receive.
    */
  def processEgressInfo(is: DataInputStream, toGo: Int, hdr: FmModelMessageHdr): Unit = {
    //val eih = is.readFmModelMsgSetEgressInfoHdr()
    val switchPort = hdr.Port
    val ei = is.readFmModelSetEgressInfoHdr()
    val tcpPort = ei.Tcpport.toInt & 0xffff
    val stringSize = toGo - 2
    val hostnameArray = Array.ofDim[Byte](stringSize)
    is.readFully(hostnameArray)

    val hostname =  hostnameArray.map(_.toChar).mkString
    val remote = (InetAddress.getByName(hostname), tcpPort)
    println(s"Configuring = $hostname:$tcpPort as egress to switch port $switchPort")

    // first determine whether we already have a connection to the client on this port
    egressPortToSocketAndStreamMap.get(switchPort) match {
      case Some((s, _)) => {
        s.close()
        egressPortToSocketAndStreamMap.remove(switchPort)
        assert(false, "Reassinging an egress port not tested functionality")
      }
      case None => { }
    }

    def matchingSocket(s: Socket) = {
      (s.getInetAddress.getCanonicalHostName == remote._1.getCanonicalHostName) && (s.getPort == remote._2)
    }

    def newSocketFunc(mapElement: (Socket, DataOutputStream)): Boolean =
      if (legacyProtocol) false else matchingSocket(mapElement._1)

    egressPortToSocketAndStreamMap(switchPort) = egressPortToSocketAndStreamMap.values.filter(newSocketFunc).headOption match {
      case Some((s, os)) => {
        println(" * Socket connection already exists!")
        (s, os)
      }
      case None => {
        val s = new Socket(hostname, tcpPort)
        println(s" * Allocating new socket connection to $hostname:$tcpPort")
        val os = new DataOutputStream(new BufferedOutputStream(s.getOutputStream()))
        (s, os)
      }
    }
  }
}
