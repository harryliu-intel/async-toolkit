package madisonbay.tcp

import com.intel.cg.hpfd.csr.macros.SizedArray
import shapeless.{ ::, Generic, HList, HNil, Lazy, Witness }
import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._

trait BitSize[A] {
  def get: Int
  def getBytes: Int = get/8
}
object BitSize {
  def bitSizeOf[A](n: Int): BitSize[A] = new BitSize[A] { def get: Int = n }

  //scalastyle:off magic.number
  implicit val uint8BitSize = bitSizeOf[U8](8)
  implicit val uint16BitSize = bitSizeOf[U16](16)
  implicit val uint32BitSize = bitSizeOf[U32](32)
  implicit val uint64itSize = bitSizeOf[U64](64)
  implicit def sizedArrayBitSize[A,B <: Int](implicit
    abs: BitSize[A],
    bWitness: Witness.Aux[B]
  ): BitSize[SizedArray[A,B]] = bitSizeOf(abs.get * bWitness.value)

  implicit val hnilBitSize: BitSize[HNil] = bitSizeOf(0)
  implicit def hlistBitSize[H, T <: HList](implicit
    hEv: Lazy[BitSize[H]],
    tEv: BitSize[T]
  ): BitSize[H :: T] = bitSizeOf(hEv.value.get + tEv.get)

  implicit def genericBitSize[A <: Product, Repr <: HList](implicit
    a: Generic.Aux[A,Repr],
    reprEv: Lazy[BitSize[Repr]]
  ): BitSize[A] = {
    val _ = a
    bitSizeOf(reprEv.value.get)
  }

  def apply[A](implicit bs: BitSize[A]): BitSize[A] = bs
}
