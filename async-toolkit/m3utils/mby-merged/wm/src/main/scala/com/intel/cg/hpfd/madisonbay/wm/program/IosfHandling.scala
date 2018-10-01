//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.program

import java.io.{DataInputStream, DataOutputStream}
import java.nio.ByteBuffer

import scala.collection.immutable.HashMap
import scala.language.reflectiveCalls

import com.intel.cg.hpfd.csr.generated.mby_top_map
import com.intel.cg.hpfd.madisonbay.Memory._
import com.intel.cg.hpfd.madisonbay.wm.server.dto._
import com.intel.cg.hpfd.madisonbay.wm.server.dto.Implicits._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.extensions.ExtArrayByte.Implicits
import monocle.Optional

class IosfHandling(csrSpace: scala.collection.mutable.HashMap[Int,Byte], paths: HashMap[Address, Optional[mby_top_map.mby_top_map,Long]]) {
  // use "duck-typing" to specify which classes are have certain IOSF characteristics
  // (we do not not provide subclasses or trait definitions in the scheme-based generator)
  type dataSig = { def data0: Long ; def data1: Long }
  type addrSig = { def addr0: Long ; def addr1: Long; def addr2: Long  }
  type iosfSig = dataSig with addrSig

  // Rich-Wrappers pattern automatically enables 'addr' and 'data' shorthands when appropriate
  implicit class IosfHasData(i: dataSig) {
    def data: Long = {
      i.data0 | (i.data1 << 32)
    }
  }
  implicit class IosfHasAddress(i: addrSig) {
    def addr: Long = {
      i.addr0 | (i.addr1 << 16) | (i.addr2 << (16 + 12))
    }
  }

  // better to generate these automatically from scheme
  // and put them in a companion object, etc.
  object IosfReadExtractor {
    def unapply(a: Array[Byte]): Option[IosfRegReadReq] = {
      if (a.length != IosfRegReadReq.LengthBits / 8) { None }
      else {
        val candidate = IosfRegReadReq(a)
        if (candidate.opcode != IOSF.RegRead) { None }
        else { Some(candidate) }
      }
    }
  }
  object IosfBlkWriteExtractor {
    def unapply(a: Array[Byte]): Option[IosfRegBlkWriteReqHdr] = {
      if (a.length != IosfRegBlkWriteReqHdr.LengthBits / 8) { None }
      else {
        val candidate = IosfRegBlkWriteReqHdr(a)
        if (candidate.opcode != IOSF.RegBlkWrite) { None }
        else { Some(candidate) }
      }
    }
  }
  object IosfRegWriteExtractor {
    def unapply(a: Array[Byte]): Option[IosfRegWriteReq] = {
      if (a.length != 24) return None
      val candidate = IosfRegWriteReq(a)
      if (candidate.opcode != IOSF.RegWrite) {
        // println("Rejecting, opcode is " + candidate.opcode)
        // println("Rejecting, byte array is " + a.toList )
        // println("Rejecting data is " + candidate.data.toHexString)
        // println("Rejecting address is " + candidate.address.toHexString)
        // println("Rejecting, operation is: " + candidate)
        None
      }
      else { Some(candidate) }
    }
  }

  def processWriteBlk(iosf: IosfRegBlkWriteReqHdr, is: DataInputStream, os: DataOutputStream): Unit = {
    val addr = iosf.addr
    println("Processing block write @" + addr.toHexString + " of "  + iosf.ndw + " words")
    val array = Array.ofDim[Byte](iosf.ndw.toInt * 4)
    is.readFully(array)
    array.hexdump()
    //    + array.toList.map(f => f"$f%x"))
    for(i <- addr until addr + 4 * iosf.ndw) {
      csrSpace.put(i.toInt, array((i - addr).toInt))
    }
    val response = makeResponse(iosf)
    val hdr = FmModelMessageHdr(20, 2.shortValue(), FmModelMsgType.Iosf, 0x0.shortValue, 0.shortValue)
    os.writeFmModelMessageHdr(hdr)
    os.writeIosfRegCompNoData(response)
    os.flush()
    println(" Wrote the response, ok")
  }

  type respondable = { def opcode: Long; def dest: Long;  def source: Long; def tag: Long }

  def makeResponse[T <: respondable] (req: T): IosfRegCompNoData = {
    require (req.opcode != IOSF.RegRead && req.opcode != IOSF.RegBlkRead)
    new IosfRegCompNoData(
      sai = 1,
      dest = req.source,
      source = req.dest,
      tag = req.tag,
      rsp = 0,
      rsvd0 = 0)
  }

  def makeReadResponse[T <: respondable] (req: T): IosfRegCompDataHdr = {
    require (req.opcode == IOSF.RegRead || req.opcode == IOSF.RegBlkRead)
    new IosfRegCompDataHdr(
      sai = 1,
      dest = req.source,
      source = req.dest,
      tag = req.tag,
      rsp = 0,
      rsvd0 = 0)
  }


  def processWriteReg(iosf: IosfRegWriteReq, os: DataOutputStream): Unit = {
    val response = makeResponse(iosf)
    val hdr = FmModelMessageHdr(3 * 4 + 2*4, 2.shortValue(), FmModelMsgType.Iosf, 0x0.shortValue, 0.shortValue)
    os.writeFmModelMessageHdr(hdr)
    os.writeIosfRegCompNoData(response)
    os.flush()
  }

  def processReadReg(csrs: mby_top_map.mby_top_map, iosf: IosfRegReadReq, os: DataOutputStream): Unit = {
    val msgLength = 3*4 + IosfRegCompDataHdr.LengthBits / 8 + 8 // 12 bytes of ModelMsgHdr, 8 bytes of IOSF header, 8 bytes of data
    val registerValue = paths
      .get(Address at (iosf.addr bytes))
      .flatMap(_.getOption(csrs))
      .getOrElse(0L)

    os.writeFmModelMessageHdr( FmModelMessageHdr(msgLength, 2.shortValue(), FmModelMsgType.Iosf, 0x0.shortValue, 0.shortValue))
    val response = makeReadResponse(iosf)
    os.writeIosfRegCompDataHdr(response)

    ByteBuffer
      .allocate(8)
      .putLong(registerValue)
      .array()
      .foreach(os.writeByte(_))

    os.flush()
    println(f"Wrote the response back $registerValue%x")
  }

  def processIosf(csrs: mby_top_map.mby_top_map, is: DataInputStream, os: DataOutputStream): Unit = {
    val array = Array.ofDim[Byte](128 / 8)  // IOSF headers are 16 bytes, except for reg-write, which is 24

    is.readFully(array)
    array match {
      case IosfBlkWriteExtractor(writeReg) => processWriteBlk(writeReg, is, os)
      case IosfReadExtractor(readReg) => processReadReg(csrs, readReg, os)
      case _ =>
        val extra = Array.ofDim[Byte](64 / 8)
        is.readFully(extra)
        val expandedArray = array ++ extra
        expandedArray match {
          case IosfRegWriteExtractor(writeReg) => processWriteReg(writeReg, os)
          case _ => assert(assertion = false, "Failed to parse IOSF packet, after trying 192-bit sized regwrite")
        }

    }
  }
}
