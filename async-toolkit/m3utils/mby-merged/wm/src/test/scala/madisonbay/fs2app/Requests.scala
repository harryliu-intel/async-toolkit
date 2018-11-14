package madisonbay
package fs2app

import madisonbay.tcp._
import madisonbay.tcp.iosf._

import com.intel.cg.hpfd.madisonbay.Memory._

import shapeless.{::, HNil}

//scalastyle:off magic.number
trait Requests {

  private val headerEnc = ByteArrayEncoder[FmModelMessageHdr]

  case class ReadRegisterRequest(hdr: FmModelMessageHdr, iosf: IosfRegReadReq)

  case class SetEgressInfoRequest(
    hdr: FmModelMessageHdr,
    infoHdr: FmModelSetEgressInfoHdr,
    hostname: Array[Byte]
  )

  case class WriteBlkRegisterRequest(
    hdr: FmModelMessageHdr,
    iosf: IosfRegBlkWriteReqHdr,
    blk: Array[Byte]
  )

  object SetEgressInfoRequest {
    def apply(port: Short, hostname: String): SetEgressInfoRequest = {
      val constSize = BitSize[FmModelMessageHdr :: FmModelSetEgressInfoHdr :: HNil].getBytes
      val hdr = FmModelMessageHdr(
        constSize + hostname.getBytes.size, 2, FmModelMsgType.SetEgressInfo, 0, 1
      )
      val infoHdr = FmModelSetEgressInfoHdr(port)

      SetEgressInfoRequest(hdr, infoHdr, hostname.getBytes)
    }
  }

  object ReadRegisterRequest {
    def apply(address: Address): ReadRegisterRequest = {
      val size = BitSize[FmModelMessageHdr].getBytes + BitSize[IosfRegReadReq].getBytes
      val header = FmModelMessageHdr(size, 2, FmModelMsgType.Iosf, 0, 0)
      val iosfRead = IosfRegReadReq(
        BitField(1), BitField(0), BitField(0), BitField(0), BitField(0), BitField(1),
        BitField(0), BitField(0), BitField(0), BitField(0), BitField(0), BitField(0), BitField(15),
        BitField(15), BitField(0), BitField(address.fullBytes.toLong), BitField(0), BitField(0)
      )

      ReadRegisterRequest(header, iosfRead)
    }

    implicit val encoder: ByteArrayEncoder[ReadRegisterRequest] = instance => {
      headerEnc.encode(instance.hdr) ++
      ByteArrayEncoder[IosfRegReadReq].encode(instance.iosf)
    }
  }

  object WriteBlkRegisterRequest {
    def apply(address: Address, data: Long*): WriteBlkRegisterRequest = {
      val constSize = BitSize[FmModelMessageHdr].getBytes +
      BitSize[IosfRegBlkWriteReqHdr].getBytes
      val dataSize = data.size * BitSize[Long].getBytes
      val header = FmModelMessageHdr(constSize + dataSize,2,FmModelMsgType.Iosf,0,0)
      val iosf = IosfRegBlkWriteReqHdr(
        BitField(1), BitField(0), BitField(17), BitField(0), BitField(0), BitField(1),
        BitField(0), BitField(0), BitField(0), BitField(0), BitField(0), BitField(0), BitField(2),
        BitField(0), BitField(address.fullBytes.toLong), BitField(0), BitField(0)
      )
      val dataArray = data.map(ByteArrayEncoder[Long].encode).reduce(_ ++ _)

      WriteBlkRegisterRequest(header, iosf, dataArray)
    }

    implicit val encoder: ByteArrayEncoder[WriteBlkRegisterRequest] = instance => {
      headerEnc.encode(instance.hdr) ++
      ByteArrayEncoder[IosfRegBlkWriteReqHdr].encode(instance.iosf) ++
      instance.blk
    }
  }
}
