package madisonbay.wm.utils

import scala.collection.BitSet

class BitFlags(flags: BitSet) {

  def get: BitSet = flags

  def set(x: Int): BitFlags = if (x == 0) { this } else { BitFlags(flags + x) }

  def clear(x: Int): BitFlags = if (x == 0) { this } else { BitFlags(flags - x) }

  def assign(x: Int, v: Boolean): BitFlags = if (v) { set(x) } else { clear(x) }

  def toLong: Long = flags.foldLeft(0L)((acc, bit) => acc | (1 << bit))

  def toInt: Int = toLong.toInt

  override def toString: String = s"BitFlags(${flags.toString()})"

}

object BitFlags {

  def apply(): BitFlags = new BitFlags(BitSet.empty)

  def apply(bitSet: BitSet): BitFlags = new BitFlags(bitSet)

}

