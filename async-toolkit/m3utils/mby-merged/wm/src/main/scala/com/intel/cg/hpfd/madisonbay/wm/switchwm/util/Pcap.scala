package com.intel.cg.hpfd.madisonbay.wm.switchwm.util

import java.io.{DataInputStream, FileInputStream}

import com.intel.cg.hpfd.madisonbay.wm.extensions.ExtInt.reverseInt
import com.intel.cg.hpfd.madisonbay.wm.extensions.ExtShort.reverseShort

object Pcap {


  case class PcapHeaderS(reverse: Boolean, /* magic number */
                         version_major: Short, /* major version number */
                         version_minor: Short, /* minor version number */
                         thiszone: Int, /* GMT to local correction */
                         sigfigs: Int, /* accuracy of timestamps */
                         snaplen: Int, /* max length of captured packets, in octets */
                         network: Int /* data link type */
                        )

  object PcapHeaderS {

    //scalastyle:off magic.number
    def apply(is: DataInputStream): PcapHeaderS = {
      val magic_number = is.readInt()
      val rev = magic_number match {
        case 0xa1b2c3d4 => false
        case 0xd4c3b2a1 => true
        case _ => assert (assertion = false, s"Totally invalid magic number ${magic_number.toHexString}")
          false
      }
      if (rev) {
        new PcapHeaderS(rev, reverseShort(is.readShort()), reverseShort(is.readShort()),
          reverseInt(is.readInt()), reverseInt(is.readInt()), reverseInt(is.readInt()), reverseInt(is.readInt()))
      } else {
        new PcapHeaderS(rev, is.readShort(), is.readShort(), is.readInt(), is.readInt(), is.readInt(), is.readInt())
      }
    }
    //scalastyle:on magic.number
  }

  /**
    * Bring in a PCAP file's contents as packets
    *
    * @see https://wiki.wireshark.org/Development/LibpcapFileFormat
    * @param f pcap file to load
    * @return
    */
  def loadPcap(f: java.io.File): Seq[Packet] = {

    case class PcapRecHeaderS(ts_sec: Int, /* timestamp seconds */
                              ts_usec: Int, /* timestamp microseconds */
                              incl_len: Int, /* number of octets of packet saved in file */
                              orig_len: Int /* actual length of packet */
                              )

    object PcapRecHeaderS {

      def apply(is: DataInputStream)(implicit main: PcapHeaderS): PcapRecHeaderS =
        if (main.reverse) {
          new PcapRecHeaderS(reverseInt(is.readInt()), reverseInt(is.readInt()), reverseInt(is.readInt()), reverseInt(is.readInt()))
        } else {
          new PcapRecHeaderS(is.readInt(), is.readInt(), is.readInt(), is.readInt())
        }

    }

    //scalastyle:off
    val is = new DataInputStream(new FileInputStream(f))
    implicit val hdr: PcapHeaderS = PcapHeaderS(is)
    assert(hdr.version_major == 2, s"Expected version 2, got hdr $hdr")
    assert(hdr.version_minor == 4, s"Expected minor version 4, got hdr $hdr")
    println(f"Reading pcap with hdr: $hdr")

    val pkt_hdr = PcapRecHeaderS(is)
    println(f"Reading packet with hdr: $pkt_hdr")
    val pktArray = Array.ofDim[Byte](pkt_hdr.incl_len)
    is.readFully(pktArray)
    println(s"Read packet of length ${pktArray.length}, captured at ${new java.util.Date(pkt_hdr.ts_sec.toLong * 1000)}")
    // first incarnation of this just reads the first packet and returns a seq of length 1, need to handle reading many (or 0) packets!
    //scalastyle:on
    Seq(Packet(pktArray))
  }

}
