// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.memory

import scala.annotation.tailrec


/** Any units of raw memory (bits, bytes etc). Should be a power of two (2 pow N) in bits.
  *
  * @note All methods return in bits.
  * */
trait MemoryUnit extends Any with Ordered[MemoryUnit] {
  /** Main numerical value, as expressed by "N units", e.g. for 8.bytes it returns 8 */
  def toLong: Long

  /** Bit value */
  def toBits: Bits

  /** Byte value if full bytes, None otherwise. */
  def tryBytes: Option[Bytes]

  /** Floor of full byte value, e.g. 15.bits.fullBytes == 1 (as 15/8 < 2) */
  def fullBytes: Bytes = (toBits.toLong / 8).bytes

  /** For easier implementation of specializations. */
  protected def nextPowerLong(value: Long): Long = {
    @tailrec
    def nextPowFrom(value: Long, power: Long): Long =
      if (power >= value) { power } else { nextPowFrom(value, power << 1) }

    if (value == 0L) {
      value
    } else {
      nextPowFrom(value, 1L)
    }
  }

  /** Calculates next power of two (2 pow N) no less than the caller. */
  def nextPowerBits: Bits = nextPowerLong(toBits.toLong).bits

  /** Whenever it is a power of two (2 pow N). */
  def isPower: Boolean = this.toBits == this.nextPowerBits

  /** General addition in bits */
  def +[M <: MemoryUnit](other: M): Bits = (toBits.toLong + other.toBits.toLong).bits

  /** General subtraction in bits */
  def -[M <: MemoryUnit](other: M): Bits = (toBits.toLong - other.toBits.toLong).bits

  /** How many blocks would fit here, e.g. 4.bytes / 3.bits == 5 (as 5 <= 16/3 < 6) */
  def /[M <: MemoryUnit](other: M): Long = toLong / other.toLong

  /** Bit shift from full block, e.g. 4.bytes / 3.bits == 1 (as 16 - (16/3)*3 == 1) */
  def %[M <: MemoryUnit](other: M): Bits = (toLong % other.toLong).bits

  def compare(other: MemoryUnit): Int = toBits.toLong.compare(other.toBits.toLong)
}