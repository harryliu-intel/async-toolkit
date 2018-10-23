package madisonbay.tcp

import java.io.EOFException
import java.nio.ByteBuffer

import scala.reflect._

import com.intel.cg.hpfd.csr.macros.SizedArray
import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._
import scalaz.MonadError
import scalaz.StateT
import scalaz.syntax.all._
import shapeless.{::, Generic, HNil, Lazy, Witness, HList}

trait ByteArrayDecoder[A] {
  def decode[F[_]](implicit me: MonadError[F,Throwable]): StateT[F,Array[Byte],A]
}

object ByteArrayDecoder {

  def decoder[A](
    error: => Throwable,
    decoderImpl: Array[Byte] => A
  )(implicit bs: BitSize[A]): ByteArrayDecoder[A] = new ByteArrayDecoder[A] {
    def decode[F[_]](implicit me: MonadError[F,Throwable]): StateT[F,Array[Byte],A] =
      StateT(array =>
        if (bs.getBytes > array.size) {
          me.raiseError[A](error).map(a => (array,a))
        } else {
          me.point((array.drop(bs.getBytes), decoderImpl(array)))
        }
      )
  }

  implicit val uint8bad: ByteArrayDecoder[U8] =
    decoder[U8](new EOFException(), _.apply(0))
  implicit val uint16bad: ByteArrayDecoder[U16] =
    decoder[U16](new EOFException(), a => ByteBuffer.wrap(a).getShort())
  implicit val uint32bad: ByteArrayDecoder[U32] =
    decoder[U32](new EOFException(), a => ByteBuffer.wrap(a).getInt())
  implicit val uint64bad: ByteArrayDecoder[U64] =
    decoder[U64](new EOFException(), a => ByteBuffer.wrap(a).getLong())
  //scalastyle:off disallow.space.before.token
  implicit def sizedArrayDecoder[A : ClassTag, B <: Int](implicit
    w: Witness.Aux[B],
    bs: BitSize[A],
    aDec: ByteArrayDecoder[A]
  ): ByteArrayDecoder[SizedArray[A,B]] = {
    new ByteArrayDecoder[SizedArray[A,B]] {
      def decode[F[_]](implicit me: MonadError[F,Throwable]): StateT[F,Array[Byte],SizedArray[A,B]] =
        StateT(array => {
          val minArraySize = bs.getBytes * w.value
          if (minArraySize > array.size) {
            me.raiseError[SizedArray[A,B]](new EOFException()).map(sa => (array, sa))
          } else {
            val sarray = new SizedArray[A,B](w.value)
            List.fill(w.value)(aDec.decode[F])
              .zipWithIndex
              .foldLeft(me.point(array,sarray)) {
                case (f,(st,idx)) =>
                  for {
                    pair <- f
                    (arr,sarr) = pair
                    pairRes <- st.run(arr)
                    (nextArr, a) = pairRes
                  } yield {
                    sarr.updateUnsafe(idx,a)
                    (nextArr,sarr)
                  }
              }
          }
        })
    }
  }
  //scalastyle:on disallow.space.before.token

  implicit val hnilDecoder: ByteArrayDecoder[HNil] =
    decoder[HNil](new EOFException(), _ => HNil)

  implicit def hlistDecoder[H,T <: HList](implicit
    hEv: Lazy[ByteArrayDecoder[H]],
    tEv: ByteArrayDecoder[T]
  ): ByteArrayDecoder[H::T] = new ByteArrayDecoder[H::T] {
    def decode[F[_]](implicit me: MonadError[F,Throwable]): StateT[F, Array[Byte], H::T] =
      for {
        h <- hEv.value.decode[F]
        t <- tEv.decode[F]
      } yield h :: t
  }

  implicit def genericDecoder[A <: Product, Repr <: HList](implicit
    genA: Generic.Aux[A,Repr],
    reprEv: Lazy[ByteArrayDecoder[Repr]]
  ): ByteArrayDecoder[A] = new ByteArrayDecoder[A] {
    def decode[F[_]](implicit me: MonadError[F,Throwable]): StateT[F, Array[Byte], A] =
      reprEv.value.decode[F].map(genA.from)
  }

  def apply[A](implicit bad: ByteArrayDecoder[A]): ByteArrayDecoder[A] = bad
}

