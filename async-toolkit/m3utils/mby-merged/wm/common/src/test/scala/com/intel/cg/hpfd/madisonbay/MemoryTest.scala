package com.intel.cg.hpfd.madisonbay

import com.intel.cg.hpfd.madisonbay.Memory.{Address, AddressRange, Alignment, Bits, Bytes}
import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers

class MemoryTest extends FlatSpec with Matchers with Checkers{

  "toLong in Bits" should "return number of bits as long" in {
    check(Prop.forAll((n: Long) => Bits(n).toLong == n))
  }

  "toBits in Bits" should "return number of bits as Bits" in {
    check(Prop.forAll((n: Long) => Bits(n).toBits == Bits(n)))
  }

  "tryBytes in Bits" should "return None if there is less than 8 bits" in {
    val bits = Bits(7)
    bits.tryBytes should equal(None)
  }
  it should "return number of whole bytes as Bytes if there is at least 8 bits" in {
    val bits = Bits(8)
    bits.tryBytes should equal(Some(Bytes(1)))
  }

  "nextPower in Bits" should "return 1 for 1 bit" in {
    val bits = Bits(1)
    bits.nextPower should equal(Bits(1))
  }
  it should "return 2 for 2 bits" in {
    val bits = Bits(2)
    bits.nextPower should equal(Bits(2))
  }
  it should "return 4 for 3 bits" in {
    val bits = Bits(3)
    bits.nextPower should equal(Bits(4))
  }

  "fullBytes in Bits" should "return 0 as Byte if there is less than 8 bits" in {
    val bits = Bits(7)
    bits.fullBytes should equal(Bytes(0))
  }
  it should "return number of whole bytes as Bytes if there is at least 8 bits" in {
    val bits = Bits(8)
    bits.fullBytes should equal(Bytes(1))
  }

  "isPower in Bits" should "return True if number of bits is 1" in {
    val bits = Bits(1)
    bits.isPower should equal(true)
  }
  it should "return True if number of bits is 2" in {
    val bits = Bits(2)
    bits.isPower should equal(true)
  }
  it should "return False if number of bits is not power of 2" in {
    val bits = Bits(3)
    bits.isPower should equal(false)
  }

  "plus(+) in Bits" should "sum bits of two Bits objects" in {
    check(Prop.forAll((n: Long, m:Long) => Bits(n) + Bits(m) == Bits(n+m)))
  }

  "minus(-) in Bits" should "subtract bits of two Bits objects" in {
    check(Prop.forAll((n: Long, m:Long) => Bits(n) - Bits(m) == Bits(n-m)))
  }

  "slash(/) in Bits" should "return floor of Bits division" in {
    val bits = Bits(10)
    val bits2 = Bits(6)
    bits / bits2 should equal(1)
  }
  it should "throw ArithmeticException while dividing by 0" in {
    val bits = Bits(1)
    val bits2 = Bits(0)
    an [ArithmeticException] should be thrownBy bits/bits2
  }

  "mod(%) in Bits" should "return mod of Bits division as Bits" in {
    val bits = Bits(10)
    val bits2 = Bits(6)
    bits % bits2 should equal(Bits(4))
  }

  "compare in Bits" should "return 1 if first op has more bits" in {
    val bits = Bits(10)
    val bits2 = Bits(6)
    bits compare bits2 should equal(1)
  }
  it should "return 0 if both ops have the same number of bits" in {
    val bits = Bits(1)
    val bits2 = Bits(1)
    bits compare bits2 should equal(0)
  }
  it should "return -1 if second op has more bits" in {
    val bits = Bits(1)
    val bits2 = Bits(5)
    bits compare bits2 should equal(-1)
  }

  "tryAlignment in Bytes" should "return Alignment if there is 2 pow N bytes" in {
    val bytes = Bytes(1)
    bytes.tryAlignment should equal(Some(Alignment(bytes)))
  }
  it should "return None otherwise" in {
    val bytes = Bytes(3)
    bytes.tryAlignment should equal(None)
  }

  "bits in Address" should "return number of bits offset from full words" in {
    val a = Address(Bits(65))
    a.bits should equal(Bits(1))
  }

  "toBits in Address" should "return number of all owned bits" in {
    val a = Address(Bits(65))
    a.toBits should equal(Bits(65))
  }

  "plus(+) in Address" should "add bytes to Address" in {
    val address = Address(Bits(2))
    val bytes = Bytes(2)
    val sum = address + bytes
    sum.toBits should equal(Bits(18))
  }
  it should "add bits to Address" in {
    val address = Address(Bits(2))
    val bits = Bits(2)
    val sum = address + bits
    sum.toBits should equal(Bits(4))
  }

  "minus(-) in Address" should "subtract bytes from address" in {
    val address = Address(1, Bits(0))
    val bytes = Bytes(1)
    val sub = address - bytes
    sub.toBits should equal(Bits(56))
  }
  it should "subtract bits from address" in {
    val address = Address(Bits(2))
    val bits = Bits(2)
    val sum = address + bits
    sum.toBits should equal(Bits(4))
  }

  "mod(%) in Address" should "return the reminder of bits division" in {
    val address = Address(1, Bits(1))
    val bits = Bits(5)
    address % bits should equal(Bits(0))
  }
  it should "throw ArithmeticException if second op has 0 bits" in {
    val address = Address(1,Bits(1))
    val bits = Bits(0)
    an [ArithmeticException] should be thrownBy address % bits
  }

  "alignTo(Align) in Address" should "align address to block of size 2 pow N" in {
    val address = Address(1, Bits(1))
    val al = Alignment(Bytes(4))
    val modA = address alignTo al
    modA.bits should equal(Bits(32))
  }

  "AddressRange" should "throw IllegalArgumentException if created with range <= 0" in {
    an [IllegalArgumentException] should be thrownBy AddressRange(Address(Bits(4)),Bits(0))
  }
  it should "throw IllegalArgumentException if created with first address param <= second address param" in {
    an [IllegalArgumentException] should be thrownBy AddressRange(Address(Bits(4)),Address(Bits(0)))
  }

  "placeReg in AddressRange" should "return properly aligned address range" in {
    val ar = AddressRange.placeReg(Address(Bits(17)), Alignment(Bytes(2)))
    ar.pos.toBits should equal(Bits(32))
    ar.width should equal(Bits(16))
  }



}