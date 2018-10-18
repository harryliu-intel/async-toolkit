package madisonbay.tcp

import java.io.EOFException
import java.nio.ByteBuffer

import scala.reflect._

import com.intel.cg.hpfd.csr.macros.SizedArray
import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._
import scalaz.MonadError
import scalaz.std.list._
import scalaz.syntax.all._
import shapeless.{::, Generic, HNil, Lazy, Witness, HList}

trait ByteArrayDecoder[A] {
  def decode[F[_]](array: Array[Byte])(implicit me: MonadError[F,Throwable]): F[A]
}

object ByteArrayDecoder {

  def decoder[A](
    errorCondition: Array[Byte] => Boolean,
    error: => Throwable,
    value: Array[Byte] => A
  ): ByteArrayDecoder[A] = new ByteArrayDecoder[A] {
    def decode[F[_]](array: Array[Byte])
      (implicit me: MonadError[F,Throwable]): F[A] =
      if (errorCondition(array)) me.raiseError(error) else me.point(value(array))
  }

  implicit val uint8bad: ByteArrayDecoder[U8] =
    decoder[U8](_.isEmpty, new EOFException(), _.apply(0))
  implicit val uint16bad: ByteArrayDecoder[U16] =
    decoder[U16](_.size < 2, new EOFException(), a => ByteBuffer.wrap(a).getShort())
  implicit val uint32bad: ByteArrayDecoder[U32] =
    decoder[U32](_.size < 4, new EOFException(), a => ByteBuffer.wrap(a).getInt())
  implicit val uint64bad: ByteArrayDecoder[U64] =
    decoder[U64](_.size < 8, new EOFException(), a => ByteBuffer.wrap(a).getLong())
  //scalastyle:off disallow.space.before.token
  implicit def sizedArrayDecoder[A : ClassTag, B <: Int](implicit
    w: Witness.Aux[B],
    bs: BitSize[A],
    aDec: ByteArrayDecoder[A]
  ): ByteArrayDecoder[SizedArray[A,B]] = {
    new ByteArrayDecoder[SizedArray[A,B]] {
      def decode[F[_]](array: Array[Byte])(implicit me: MonadError[F,Throwable]): F[SizedArray[A,B]] = {
        val elems = array.grouped(bs.getBytes).take(w.value).toList
        if (elems.size < w.value) {
          me.raiseError(new EOFException())
        } else {
          elems.traverse(aDec.decode[F]).map { list =>
            val sarray = new SizedArray[A,B](w.value)
            list.zipWithIndex.foreach {
              case (elem, idx) => sarray.updateUnsafe(idx,elem)
            }
            sarray
          }
        }
      }
    }
  }
  //scalastyle:on disallow.space.before.token

  implicit val hnilDecoder: ByteArrayDecoder[HNil] =
    decoder[HNil](_ => false, new EOFException(), _ => HNil)

  implicit def hlistDecoder[H,T <: HList](implicit
    hEv: Lazy[ByteArrayDecoder[H]],
    sizeEv: Lazy[BitSize[H]],
    tEv: ByteArrayDecoder[T]
  ): ByteArrayDecoder[H::T] = new ByteArrayDecoder[H::T] {
      def decode[F[_]](array: Array[Byte])(implicit me: MonadError[F,Throwable]): F[H::T] =
        for {
          h <- hEv.value.decode(array)
          t <- tEv.decode(array.drop(sizeEv.value.getBytes))
        } yield h :: t
    }

  implicit def genericDecoder[A <: Product, Repr <: HList](implicit
    genA: Generic.Aux[A,Repr],
    reprEv: Lazy[ByteArrayDecoder[Repr]]
  ): ByteArrayDecoder[A] = new ByteArrayDecoder[A] {
    def decode[F[_]](array: Array[Byte])(implicit me: MonadError[F,Throwable]): F[A] =
      reprEv.value.decode(array).map(genA.from)
  }

  def apply[A](implicit bad: ByteArrayDecoder[A]): ByteArrayDecoder[A] = bad
}

