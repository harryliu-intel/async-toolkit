package madisonbay.tcp

import shapeless.Witness
import java.io.EOFException

case class BitField[Width <: Int](value: Long)

object BitField {
  implicit def bitSize[W <: Int](implicit witness: Witness.Aux[W]): BitSize[BitField[W]] =
    new BitSize[BitField[W]] {
      def get: Int = witness.value
      override def getBytes = get
    }

  implicit def byteArrayEncoder[W <: Int](implicit witness: Witness.Aux[W]): ByteArrayEncoder[BitField[W]] =
    bitField => for {
      idx <- (0 until witness.value).toArray
    } yield if ((bitField.value & (1 << idx)) == 0) 0.toByte else 1.toByte

  implicit def byteArrayDecoder[W <: Int](implicit witness: Witness.Aux[W]): ByteArrayDecoder[BitField[W]] =
    ByteArrayDecoder.decoder(
      _.size < witness.value,
      new EOFException(),
      array => BitField[W](
        array.take(witness.value).zipWithIndex.foldLeft(0L){
          case (l, (bit,idx)) => l | (bit << idx)
        }
      )
    )
}

