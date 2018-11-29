package madisonbay.tcp

import com.intel.cg.hpfd.csr.macros.SizedArray
import shapeless.{HNil, HList, ::, Lazy, Generic}
import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._
import java.nio.ByteBuffer

trait ByteArrayEncoder[T] {
  def encode(t: T): Array[Byte]
}

object ByteArrayEncoder {
  //scalastyle:off magic.number
  implicit val id: ByteArrayEncoder[Array[Byte]] = identity
  implicit val u8bae: ByteArrayEncoder[U8] = Array(_)
  implicit val u16bae: ByteArrayEncoder[U16] = u16 =>
    ByteBuffer.allocate(2).putShort(u16).array()
  implicit val u32bae: ByteArrayEncoder[U32] = u32 =>
    ByteBuffer.allocate(4).putInt(u32).array()
  implicit val u64bae: ByteArrayEncoder[U64] = u64 =>
    ByteBuffer.allocate(8).putLong(u64).array()
  implicit def sizedArrayEncoder[A,B <: Int](
    implicit baEn: ByteArrayEncoder[A]
  ): ByteArrayEncoder[SizedArray[A,B]] = _.array.flatMap(baEn.encode)

  implicit val hnilEncoder: ByteArrayEncoder[HNil] = _ => Array.empty[Byte]

  implicit def hlistEncoder[H, T <: HList](
    implicit
    hEv: Lazy[ByteArrayEncoder[H]],
    tEv: ByteArrayEncoder[T]
  ): ByteArrayEncoder[H::T] =
    hlist => hEv.value.encode(hlist.head) ++ tEv.encode(hlist.tail)

  implicit def genericEncoder[A, Repr <: HList](
    implicit
    generic: Generic.Aux[A,Repr],
    reprEv: Lazy[ByteArrayEncoder[Repr]]
  ): ByteArrayEncoder[A] = a => reprEv.value.encode(generic.to(a))

  def apply[T](implicit bae: ByteArrayEncoder[T]): ByteArrayEncoder[T] = bae
}

