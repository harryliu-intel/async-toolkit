package com.intel.cg.hpfd.madisonbay.wm

import java.io.{DataInputStream, FileInputStream}

package object switch_wm {
  case class Packet(bytes : Array[Byte]) {

  }

  /**
    * Bring in a PCAP file's contents as packets
    *
    * @see https://wiki.wireshark.org/Development/LibpcapFileFormat
    * @param f
    * @return
    */
  def loadPcap(f : java.io.File) : Seq[Packet] = {
    def reverse(s : Short) : Short = {
      (((s >> 8) & 0xff) | (s << 8)).toShort
    }
    def reverseI(i : Int) : Int = {
        ((i >> 24) & 0xff) |
        ((i >> 8) & 0xff00) |
        ((i << 8) & 0x00ff0000) |
        ((i << 24) & 0xff000000)
    }

    case class pcap_hdr_s (
      val reverse : Boolean,   /* magic number */
      val version_major : Short,  /* major version number */
      val version_minor : Short,  /* minor version number */
      val  thiszone : Int,       /* GMT to local correction */
      val sigfigs : Int,        /* accuracy of timestamps */
      val snaplen : Int,        /* max length of captured packets, in octets */
      val network : Int        /* data link type */
                     ) {
    }
    object pcap_hdr_s {
      def apply(is : DataInputStream) : pcap_hdr_s = {
        val magic_number = is.readInt()
        val rev = magic_number match {
          case 0xa1b2c3d4 => false
          case 0xd4c3b2a1 => true
          case _ => { assert (false, s"Totally invalid magic number ${magic_number.toHexString}") ; false }
        }
        rev match {
          case true => new pcap_hdr_s(rev, reverse(is.readShort()), reverse(is.readShort()), reverseI(is.readInt()), reverseI(is.readInt()), reverseI(is.readInt()), reverseI(is.readInt()))
          case false => new pcap_hdr_s(rev, is.readShort(), is.readShort(), is.readInt(), is.readInt(), is.readInt(), is.readInt())
        }
      }
    }

    case class pcaprec_hdr_s (
      ts_sec : Int,         /* timestamp seconds */
      ts_usec : Int,        /* timestamp microseconds */
      incl_len : Int,       /* number of octets of packet saved in file */
      orig_len : Int      /* actual length of packet */
    )
    object pcaprec_hdr_s {
      def apply(is : DataInputStream)(implicit main : pcap_hdr_s) : pcaprec_hdr_s = {
        main.reverse match {
          case false => new pcaprec_hdr_s (is.readInt (), is.readInt (), is.readInt (), is.readInt () )
          case true => new pcaprec_hdr_s (reverseI(is.readInt ()), reverseI(is.readInt ()), reverseI(is.readInt ()), reverseI(is.readInt () ))
        }
      }
    }

    val is = new DataInputStream(new FileInputStream(f))
    implicit val hdr = pcap_hdr_s(is)
    assert(hdr.version_major == 2, s"Expected version 2, got hdr ${hdr}")
    assert(hdr.version_minor == 4, s"Expected minor version 4, got hdr ${hdr}")
    println(f"Reading pcap with hdr: ${hdr}")

    val pkt_hdr = pcaprec_hdr_s(is)
    println(f"Reading packet with hdr: ${pkt_hdr}")
    val pktArray = Array.ofDim[Byte](pkt_hdr.incl_len)
    is.readFully(pktArray)
    println(s"Read packet of length ${pktArray.length}, captured at ${new java.util.Date(pkt_hdr.ts_sec.toLong * 1000)}")
    // first incarnation of this just reads the first packet and returns a seq of length 1, need to handle reading many (or 0) packets!
    Seq(new Packet(pktArray))
  }

  /**
    * Compute 1's complement based IPv4 checksum over an array of bytes
    * @see https://en.wikipedia.org/wiki/IPv4_header_checksum
    * @param bytes
    * @return
    */
  def checksum(bytes : Seq[Byte]) : Short = {
    def foldToShort(x : Int) : Short = {
      val lo = x & 0xffff
      val hi = (x >> 16) & 0xffff
      val folded = lo + hi
      if ((folded >> 16) != 0) foldToShort(folded)
      else folded.toShort
    }
    def addUnsignedShort(acc : Int, addend : Short) : Int = acc + (addend.toInt & 0xffff)

    // sum up the 16-bit words
    val sum = bytes.sliding(2,2).toList.foldLeft(0)(
      { (acc, byte) =>
        if (byte.length == 1) {
          acc + (byte.head & 0xFF)
        }
        else {
          require(byte.length == 2, "Something wrong if sliding give either 1/2 sized lists")
          val hibits = (byte(0) << 8) & 0xff00
          val lowbits = byte(1) & 0xff
          val addend : Int = lowbits + hibits
          addUnsignedShort(acc, addend.toShort )
        }
    })
    // fold the high and low 16 bit components together and bitwise invert the result to get the checksum
    (~foldToShort(sum) & 0xFFFF).toShort
  }

  implicit class RichByteArray(val self : Array[Byte]) {
    def hexdump: Unit = {
      print("Dump is: ")
      var count: Int = 0

      def cr(): Unit = {
        if ((count % 16) == 0) print("\n" + count.toHexString + "\t")
        count += 1
      }

      self.foreach(s => {
        cr()
        print(f"$s%02X" + " ")
      }
      )
      println()
    }
  }
  implicit class RichByte(val self : Byte) {
    def hex: String = {
      f"0x$self%02X"
    }
  }

  implicit class nibbles(val x : Int) {
    def nib(i : Int) : Int = {
      val mask = 0xf << i
      (x & mask) >> (4 * i)
    }
  }
}
