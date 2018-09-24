package com.intel.cg.hpfd.madisonbay.wm.switchwm.util

import java.io.{DataInputStream, FileInputStream}

import com.intel.cg.hpfd.madisonbay.wm.switchwm.extensions.ExtInt.reverseInt
import com.intel.cg.hpfd.madisonbay.wm.switchwm.extensions.ExtShort.reverseShort
//scalastyle:off
case class Packet(bytes: Array[Byte])

object Packet {

  /**
    * Bring in a PCAP file's contents as packets
    *
    * @see https://wiki.wireshark.org/Development/LibpcapFileFormat
    * @param f pcap file to load
    * @return
    */
  def loadPcap(f: java.io.File): Seq[Packet] = {

    case class pcap_hdr_s ( reverse: Boolean,      /* magic number */
                            version_major: Short,  /* major version number */
                            version_minor: Short,  /* minor version number */
                            thiszone: Int,         /* GMT to local correction */
                            sigfigs: Int,          /* accuracy of timestamps */
                            snaplen: Int,          /* max length of captured packets, in octets */
                            network: Int           /* data link type */
                          )

    object pcap_hdr_s {

      def apply(is: DataInputStream): pcap_hdr_s = {
        val magic_number = is.readInt()
        val rev = magic_number match {
          case 0xa1b2c3d4 => false
          case 0xd4c3b2a1 => true
          case _ => assert (assertion = false, s"Totally invalid magic number ${magic_number.toHexString}")
            false
        }
        if (rev)
          new pcap_hdr_s(rev, reverseShort(is.readShort()), reverseShort(is.readShort()), reverseInt(is.readInt()), reverseInt(is.readInt()), reverseInt(is.readInt()), reverseInt(is.readInt()))
        else
          new pcap_hdr_s(rev, is.readShort(), is.readShort(), is.readInt(), is.readInt(), is.readInt(), is.readInt())
      }

    }

    case class pcaprec_hdr_s (ts_sec: Int,          /* timestamp seconds */
                              ts_usec: Int,         /* timestamp microseconds */
                              incl_len: Int,        /* number of octets of packet saved in file */
                              orig_len: Int         /* actual length of packet */
                              )

    object pcaprec_hdr_s {

      def apply(is: DataInputStream)(implicit main: pcap_hdr_s): pcaprec_hdr_s =
        if (main.reverse)
          new pcaprec_hdr_s (reverseInt(is.readInt ()), reverseInt(is.readInt ()), reverseInt(is.readInt ()), reverseInt(is.readInt () ))
        else
          new pcaprec_hdr_s (is.readInt (), is.readInt (), is.readInt (), is.readInt () )

    }

    val is = new DataInputStream(new FileInputStream(f))
    implicit val hdr: pcap_hdr_s = pcap_hdr_s(is)
    assert(hdr.version_major == 2, s"Expected version 2, got hdr $hdr")
    assert(hdr.version_minor == 4, s"Expected minor version 4, got hdr $hdr")
    println(f"Reading pcap with hdr: $hdr")

    val pkt_hdr = pcaprec_hdr_s(is)
    println(f"Reading packet with hdr: $pkt_hdr")
    val pktArray = Array.ofDim[Byte](pkt_hdr.incl_len)
    is.readFully(pktArray)
    println(s"Read packet of length ${pktArray.length}, captured at ${new java.util.Date(pkt_hdr.ts_sec.toLong * 1000)}")
    // first incarnation of this just reads the first packet and returns a seq of length 1, need to handle reading many (or 0) packets!
    Seq(new Packet(pktArray))
  }

}
