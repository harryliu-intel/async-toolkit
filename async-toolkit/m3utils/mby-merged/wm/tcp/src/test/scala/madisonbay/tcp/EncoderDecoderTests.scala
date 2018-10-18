//scalastyle:off
package madisonbay.tcp
import com.intel.cg.hpfd.csr.macros.SizedArray
// import com.intel.cg.hpfd.madisonbay.wm.server.dto.{IosfRegWriteReq => Old}
// import com.intel.cg.hpfd.madisonbay.wm.server.dto.{IosfRegBlkAddr => Old}
import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._
import org.scalatest.{FlatSpec, Matchers}
import scala.reflect._
import eu.timepit.refined.W
import scalaz.MonadError
import scalaz.Id._

import madisonbay.tcp.iosf._

class ByteArrayEncoderTest extends FlatSpec with Matchers {

  implicit def idMonadError[T]: MonadError[Id,T] = new MonadError[Id,T] {
    def point[A](a: => A): Id[A] = a
    def bind[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
    def handleError[A](fa: Id[A])(f: T => Id[A]): Id[A] = fa
    def raiseError[A](e: T): Id[A] = ???
  }

  def idempotentEncoderDecoder[A : ByteArrayEncoder : ByteArrayDecoder : ClassTag](initialValue: A) {
    val className = implicitly[ClassTag[A]].runtimeClass.getSimpleName

    it should s"encode and decode to/from the same value for ${className}" in {
      val encoder = ByteArrayEncoder[A]
      val decoder = ByteArrayDecoder[A]

      initialValue shouldEqual decoder.decode[Id](
        encoder.encode(initialValue)
      )
    }
  }

  it should behave like idempotentEncoderDecoder(FmModelMsgMgmt32(FmModelMgmtType.Write64, 2, 2L))
  it should behave like idempotentEncoderDecoder(FmModelMessageHdr(1,2,FmModelMsgType.Attr, 3, 4))
  it should behave like idempotentEncoderDecoder(
    {
      val a = new SizedArray[U8, W.`256`.T](256)
      val b = new SizedArray[U8, W.`256`.T](256)
      a.updateUnsafe(6, _ => 9.toByte)
      b.updateUnsafe(5, _ => 3.toByte)
      FmModelMsgAttr(FmModelAttrType.SetAck, 12, a, b)
    }
  )
  it should behave like idempotentEncoderDecoder(FmModelMsgErrorHdr(7.toByte))
  it should behave like idempotentEncoderDecoder(
    {
      val padding = new SizedArray[U8,W.`51`.T](51)
      padding.updateUnsafe(10, _ => 5.toByte)
      FmModelMsgGetInfo(FmModelInfoType.Request, 4.toShort, padding)
    }
  )
  it should behave like idempotentEncoderDecoder(FmModelMsgPacketEot(14.toShort))
  it should behave like idempotentEncoderDecoder(FmModelMsgSetEgressInfoHdr(3.toShort))
  it should behave like idempotentEncoderDecoder(FmModelMsgVersionHdr(5.toShort))
  it should behave like idempotentEncoderDecoder(FmModelPortLinkState(1.toByte))
  it should behave like idempotentEncoderDecoder(FmModelSetEgressInfoHdr(9.toShort))
  it should behave like idempotentEncoderDecoder(
    {
      val pktmeta = new SizedArray[U8, W.`32`.T](32)
      pktmeta.updateUnsafe(6, _ => 69.toByte)
      FmModelSideBandData(4.toInt, 1.toByte, pktmeta)
    }
  )

  it should behave like idempotentEncoderDecoder(
    IosfRegWriteReq(
      dest = BitField(1L),
      source = BitField(1L),
      opcode = BitField(1L),
      tag = BitField(1L),
      bar = BitField(1L),
      al = BitField(1L),
      eh0 = BitField(1L),
      exphdr = BitField(1L),
      eh1 = BitField(1L),
      sai = BitField(1L),
      rs = BitField(1L),
      rsvd0 = BitField(1L),
      fbe = BitField(1L),
      sbe = BitField(1L),
      fid = BitField(1L),
      addr0 = BitField(1L),
      addr1 = BitField(1L),
      addr2 = BitField(1L),
      data0 = BitField(1L),
      data1 = BitField(1L)
    )
  )
  it should behave like idempotentEncoderDecoder(IosfRegBlkAddr(BitField(2), BitField(22)))
  it should behave like idempotentEncoderDecoder(IosfRegBlkData(BitField(16)))
  it should behave like idempotentEncoderDecoder(
    IosfRegBlkReadReqHdr(
      dest = BitField(5),
      source = BitField(2),
      opcode = BitField(0x10),
      tag = BitField(1),
      bar = BitField(2),
      al = BitField(0x1),
      eh0 = BitField(1),
      exphdr = BitField(0x0),
      eh1 = BitField(0x0),
      sai = BitField(11),
      rs = BitField(0x0),
      rsvd0 = BitField(7),
      ndw = BitField(120),
      fid = BitField(0x0),
      addr0 = BitField(125),
      addr1 = BitField(30),
      addr2 = BitField(0x0)
    )
  )
  it should behave like idempotentEncoderDecoder(
    IosfRegBlkWriteReqHdr(
      dest = BitField(50),
      source = BitField(63),
      opcode = BitField(0x11),
      tag = BitField(2),
      bar = BitField(2),
      al = BitField(0x1),
      eh0 = BitField(1),
      exphdr = BitField(0x0),
      eh1 = BitField(0x0),
      sai = BitField(120),
      rs = BitField(0x0),
      rsvd0 = BitField(2),
      ndw = BitField(33),
      fid = BitField(0x0),
      addr0 = BitField(120),
      addr1 = BitField(70),
      addr2 = BitField(0x0)
    )
  )
  it should behave like idempotentEncoderDecoder(
    IosfRegCompDataHdr (
      dest = BitField(8),
      source = BitField(7),
      opcode = BitField(0x21),
      tag = BitField(1),
      rsp = BitField(2),
      rsvd0 = BitField(2),
      eh0 = BitField(0x1),
      exphdr = BitField(0x0),
      eh1 = BitField(0x0),
      sai = BitField(120),
      rs = BitField(0x0)
    )
  )
  it should behave like idempotentEncoderDecoder(
    IosfRegCompNoData(
      dest = BitField(7),
      source = BitField(3),
      opcode = BitField(0x20),
      tag = BitField(3),
      rsp = BitField(2),
      rsvd0 = BitField(2),
      eh0 = BitField(0x1),
      exphdr = BitField(0x0),
      eh1 = BitField(0x0),
      sai = BitField(120),
      rs = BitField(0x0)
    )
  )
  it should behave like idempotentEncoderDecoder(
    IosfRegReadReq(
      dest = BitField(2),
      source = BitField(2),
      opcode = BitField(0x0),
      tag = BitField(3),
      bar = BitField(3),
      al = BitField(0x1),
      eh0 = BitField(1),
      exphdr = BitField(0x0),
      eh1 = BitField(0x0),
      sai = BitField(120),
      rs = BitField(0x0),
      rsvd0 = BitField(2),
      fbe = BitField(0xf),
      sbe = BitField(2),
      fid = BitField(0x0),
      addr0 = BitField(120),
      addr1 = BitField(70),
      addr2 = BitField(0x0)
    )
  )
}
