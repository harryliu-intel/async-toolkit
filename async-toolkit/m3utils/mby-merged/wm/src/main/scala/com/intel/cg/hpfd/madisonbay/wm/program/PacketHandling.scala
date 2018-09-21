//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.program

import java.io.{DataInputStream, DataOutputStream}
import java.net.Socket

import com.intel.cg.hpfd.madisonbay.wm.server.dto._
import com.intel.cg.hpfd.madisonbay.wm.server.dto.Implicits._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.extensions.ExtArrayByte.RichByteArray

import scala.collection.mutable.HashMap

class PacketHandling(egressPortToSocketAndStreamMap: HashMap[Int, (Socket, DataOutputStream)],
                     legacyProtocol: Boolean) {

  def portToOs(i: Int): DataOutputStream = egressPortToSocketAndStreamMap(i)._2

  def pushEot(os: DataOutputStream): Unit = {
    val resultHdr = new FmModelMessageHdr(
      // outer header + inner header (type + integer) + size of contents
      Msglength = 12 + 2,
      Version = 2.shortValue(), Type = FmModelMsgType.PacketEot, Sw = 0.shortValue(), Port = 0.shortValue)
    os.writeFmModelMessageHdr(resultHdr)
    os.writeFmModelMsgPacketEot(FmModelMsgPacketEot(0.shortValue))
    os.flush()
  }

  def pushPacket(port: Short, contents: Array[Byte]): Unit = {
    val resultHdr = new FmModelMessageHdr(
      // outer header + inner header (type + integer) + size of contents
      Msglength = 12 + (FmModelDataType.Length + 4) + contents.length,
      Version = 2.shortValue(), Type = FmModelMsgType.Packet, Sw = 0.shortValue(), Port = port)
    val os = portToOs(port)

    os.writeFmModelMessageHdr(resultHdr)
    os.writeFmModelDataType(FmModelDataType.Packet)
    os.writeInt(contents.length)
    os.write(contents)
    os.flush()

  }

  def processPacket(is: DataInputStream, os: DataOutputStream, hdr: FmModelMessageHdr): Unit = {
    def extractFragment(): (FmModelDataType.Value, Array[Byte]) = {
      val t = is.readFmModelDataType()
      val len = is.readInt()
      val contents = Array.ofDim[Byte](len)
      is.readFully(contents)
      t match {
        case FmModelDataType.PacketMeta => { }
        case FmModelDataType.SbId => { assert(false, "SbID not supported")}
        case FmModelDataType.SbTc => { assert(false, " SbTc not supported")}
        case FmModelDataType.Packet => {}
      }
      (t, contents)
    }
    var toParse = hdr.Msglength - 12
    while(toParse > 0) {
      val thisFrag = extractFragment()
      toParse -= (thisFrag._2.length + 4 + FmModelDataType.Length)
      println("Got fragment. To go in this frame " + toParse)
      thisFrag._2.hexdump

      thisFrag match {
        case (FmModelDataType.Packet, contents) => {
          println("Reflecting back packet of size: " + contents.length)
          pushPacket(hdr.Port, contents)
        }
        case (FmModelDataType.PacketMeta, contents) => {
          println("Ignoring " + contents.length + " bytes of meta data")
        }
        case _ => { assert(false, "Unmatched packet fragment")}
      }
    }
    //done 'reflecting', signal we are done with this packet on all ports (transmission size at 0, not sure
    // what the point of this is)
    pushEot(os)
    // We have to make sure we are sending EOT just once per socket, hence toSet (look @ method doc)
    if (!legacyProtocol)
      { for (otherOs <- egressPortToSocketAndStreamMap.values.map(_._2).toSet) pushEot(otherOs) }
  }
}
