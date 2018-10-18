package madisonbay.tcp

import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._
import shapeless.{ CNil, Generic, HList, Lazy, :+: }
import shapeless.ops.coproduct.Inject
import scalaz.MonadError
import scalaz.syntax.all._

package object iosf {

  type Iosf = IosfRegReadReq :+:
    IosfRegCompNoData :+:
    IosfRegCompDataHdr :+:
    IosfRegBlkWriteReqHdr :+:
    IosfRegBlkReadReqHdr :+:
    IosfRegBlkData :+:
    IosfRegBlkAddr :+:
    IosfRegWriteReq :+:
    CNil

  val RegRead: U8 = 0x0
  val RegWrite: U8 = 0x1
  val RegWrPrivCtrl: U8 = 0x7
  val RegBlkRead: U8 = 0x10
  val RegBlkWrite: U8 = 0x11
  val CompNoData: U8 = 0x20
  val CompData: U8 = 0x21
  val FuseReq: U8 = 0x45
  val IpReady: U8 = 0xd0

  /**
    * Here we are converting byte array into array of bits
    * where each bit is encoded on single byte.
    * This hack is used for having consistent typeclasses api
    * for serializing/deserializing data structures.
    */
  implicit def iosfGenericDecoder[A, Repr <: HList](implicit
    inj: Inject[Iosf,A],
    gen: Generic.Aux[A,Repr],
    badEv: Lazy[ByteArrayDecoder[Repr]],
    bitSize: BitSize[A]
  ): ByteArrayDecoder[A] = {
    val _ = List(inj,gen,badEv,bitSize)
    new ByteArrayDecoder[A] {
      def decode[F[_]](array: Array[Byte])(implicit me: MonadError[F,Throwable]): F[A] = {
        def decompressToBits: Array[Byte] =
          for {
            byte <- array.take(bitSize.getBytes)
            idx <- (0 to 7)
            bit = if ((byte & (1 << idx)) == 0) 0.toByte else 1.toByte
          } yield bit

        badEv.value.decode[F](decompressToBits).map(gen.from)
      }
    }
  }

  implicit def iosfGenericEncoder[A <: Product, Repr <: HList](implicit
    inj: Inject[Iosf,A],
    generic: Generic.Aux[A,Repr],
    reprEv: Lazy[ByteArrayEncoder[Repr]]
  ): ByteArrayEncoder[A] = {
    val _ = inj

    def compressToBytes(array: Array[Byte]): Array[Byte] = {
      val bitsPerByte = 8
      val initial = 0.toByte
      for {
        bits <- array.grouped(bitsPerByte).toArray
      } yield bits.zipWithIndex.foldLeft(initial) {
        case (result, (b,i)) => (result | (b << i)).toByte
      }
    }

    a => compressToBytes(reprEv.value.encode(generic.to(a)))
  }

}
