//scalastyle:off magic.number
//scalastyle:off regex.tuples
package com.intel.cg.hpfd.csr.testData

import com.intel.cg.hpfd.csr.macros.annotations._
import com.intel.cg.hpfd.madisonbay.BitVector
import com.intel.cg.hpfd.madisonbay.Memory._

import scala.collection.immutable.HashMap
import monocle.Optional
import monocle.macros.Lenses

package object common {
  @Lenses("_")
  case class Register(address: Address, value: Long) {
    def range: AddressRange = AddressRange(address, 64.bits)
    def companion: Register.type = Register
    def serialize: BitVector = BitVector(value)
  }
  object Register {
    def deserialize(bvec: BitVector): Register = Register(Address(0.bits), bvec.extract[Long])

    def apply(address: Address): Register =
      Register(AddressRange.placeReg(address, Alignment(8 bytes)).pos, 0xDEADBEEF)

    def genOpticsLookup[A](me: Register, path: Optional[A, Register]): HashMap[Address, Optional[A, BitVector]] = {
      val valueOpt = Optional[Register, BitVector] {
        r => Some(r.serialize)
      } {
        newValue => _.companion.deserialize(newValue)
      }

      HashMap(me.range.pos -> (path composeOptional valueOpt))
    }
  }
}

package object optics {
  @Lenses("_")
  @Initialize
  @GenOpticsLookup
  case class AddressMap(
    range: AddressRange,
    @OfSize(2) regFilesA: List[RegisterFileA],
    @At(0x68) regFileB: RegisterFileB
  )
  @Lenses("_")
  @Initialize
  @GenOpticsLookup
  case class RegisterFileA(
    range: AddressRange,
    @At(0x18) reg: common.Register,
    @OfSize(2) l: List[common.Register]
  )
  @Initialize
  @Lenses("_")
  @GenOpticsLookup
  case class RegisterFileB(
    range: AddressRange,
    @OfSize(2) regs: List[common.Register],
    r: common.Register
  )
}

package object at {
  @Initialize
  case class AddressMap(
    range: AddressRange,
    @OfSize(2) atRegFilesA: List[RegisterFileA],
    // move 13 words forward
    @At(0x68) atRegFileB: RegisterFileB
  )
  @Initialize
  case class RegisterFileA(
    range: AddressRange,
    // move three words forward
    @At(0x18) reg: common.Register,
    @OfSize(2) l: List[common.Register]
  )
  @Initialize
  case class RegisterFileB(
    range: AddressRange,
    @OfSize(2) regs: List[common.Register],
    r: common.Register
  )
}

package object modulo {
  @Initialize
  case class AddressMap(
    range: AddressRange,
    @At(0x200) @OfSize(1)
    moduloRegFilesA: List[RegisterFileA],
    @Modulo(0x20)
    moduloRegFileB: RegisterFileB
  )
  @Initialize
  case class RegisterFileA(
    range: AddressRange,
    reg: common.Register,
    @OfSize(1)
    l: List[common.Register]
  )
  @Initialize
  case class RegisterFileB(
    range: AddressRange,
    @OfSize(5)
    regs: List[common.Register],
    @Modulo(0x80)
    r: common.Register
  )
}

package object increment {
  @Initialize
  case class AddressMap(
    range: AddressRange,
    @OfSize(3) @Increment(0x280)
    incrementRegFilesA: List[RegisterFileA],
    incrementRegFileB: RegisterFileB
  )
  @Initialize
  case class RegisterFileA(
    range: AddressRange,
    reg: common.Register,
    @OfSize(5) @Increment(0x10)
    l: List[common.Register]
  )
  @Initialize
  case class RegisterFileB(
    range: AddressRange,
    @OfSize(3) @Increment(0x20)
    regs: List[common.Register],
    r: common.Register
  )
}

package object all {
  /**
    * '@Initialize macro should generate following apply method within companion object
    * {{{
    * {
    *   object AddressMap extends scala.AnyRef {
    *     def <init>() = {
    *       super.<init>();
    *       ()
    *     };
    *     def apply(address: Address): AddressMap = {
    *       val initialAddress: Address = address;
    *       <synthetic> <artifact> private[this] val x$41 = 1.to(3).foldRight(scala.Tuple2(initialAddress, List.empty[RegisterFileA]))(<empty> match {
    *         case scala.Tuple2(_, scala.Tuple2((previousAddress @ _), (result @ _))) => {
    *           val nextAddress = previousAddress;
    *           val child = RegisterFileA.apply(nextAddress);
    *           scala.Tuple2(child.range.lim, {
    *             <synthetic> <artifact> val x$40 = child;
    *             result.$colon$colon(x$40)
    *           })
    *         }
    *       }): @scala.unchecked match {
    *         case (result @ scala.Tuple2(_, $colon$colon((lastChild @ _), _))) => scala.Tuple2(result, lastChild)
    *       };
    *       val result = x$41._1;
    *       val lastChild = x$41._2;
    *       <synthetic> <artifact> private[this] val x$42 = scala.Tuple2(lastChild.range.lim, result._2.reverse): @scala.unchecked match {
    *         case scala.Tuple2((regFilesANextFreeAddress @ _), (regFilesA @ _)) => scala.Tuple2(regFilesANextFreeAddress, regFilesA)
    *       };
    *       val regFilesANextFreeAddress = x$42._1;
    *       val regFilesA = x$42._2;
    *       {
    *         val initialAddress: Address = address.$plus(64000L.bytes);
    *         <synthetic> <artifact> private[this] val x$38 = 1.to(2).foldRight(scala.Tuple2(initialAddress, List.empty[RegisterFileB]))(<empty> match {
    *           case scala.Tuple2(_, scala.Tuple2((previousAddress @ _), (result @ _))) => {
    *             val nextAddress = previousAddress;
    *             val child = RegisterFileB.apply(nextAddress);
    *             scala.Tuple2(child.range.pos.$plus(32768L.bytes), {
    *               <synthetic> <artifact> val x$37 = child;
    *               result.$colon$colon(x$37)
    *             })
    *           }
    *         }): @scala.unchecked match {
    *           case (result @ scala.Tuple2(_, $colon$colon((lastChild @ _), _))) => scala.Tuple2(result, lastChild)
    *         };
    *         val result = x$38._1;
    *         val lastChild = x$38._2;
    *         <synthetic> <artifact> private[this] val x$39 = scala.Tuple2(lastChild.range.lim, result._2.reverse): @scala.unchecked match {
    *           case scala.Tuple2((regFilesBNextFreeAddress @ _), (regFilesB @ _)) => scala.Tuple2(regFilesBNextFreeAddress, regFilesB)
    *         };
    *         val regFilesBNextFreeAddress = x$39._1;
    *         val regFilesB = x$39._2;
    *         AddressMap(AddressRange(address, regFilesBNextFreeAddress), regFilesA, regFilesB)
    *       }
    *     }
    *   };
    *   ()
    * }
    * }}}
    *
    * @param range address range covered by this AddressMap
    * @param regFiles list of size 3 of register files
    * @param regFile single register file
    */
  @Initialize
  case class AddressMap(
     range: AddressRange,
     @OfSize(3) regFilesA: List[RegisterFileA],
     // move 64000 bytes forward
     @OfSize(2) @At(0xFA00) @Increment(0x8000) regFilesB: List[RegisterFileB]
  )

  /**
    * '@Initialize macro should generate following code:
    * {{{
    * {
    *   object RegisterFileA extends scala.AnyRef {
    *     def <init>() = {
    *       super.<init>();
    *       ()
    *     };
    *     def apply(address: Address): RegisterFileA = {
    *       <synthetic> <artifact> private[this] val x$46 = {
    *         val child = common.Register(address.$plus(16L.bytes));
    *         scala.Tuple2(child.range.lim, child)
    *       }: @scala.unchecked match {
    *         case scala.Tuple2((regNextFreeAddress @ _), (reg @ _)) => scala.Tuple2(regNextFreeAddress, reg)
    *       };
    *       val regNextFreeAddress = x$46._1;
    *       val reg = x$46._2;
    *       {
    *         val initialAddress: Address = regNextFreeAddress.alignTo(Alignment(128L.bytes));
    *         <synthetic> <artifact> private[this] val x$44 = 1.to(3).foldRight(scala.Tuple2(initialAddress, List.empty[common.Register]))(<empty> match {
    *           case scala.Tuple2(_, scala.Tuple2((previousAddress @ _), (result @ _))) => {
    *             val nextAddress = previousAddress;
    *             val child = common.Register.apply(nextAddress);
    *             scala.Tuple2(child.range.pos.$plus(16L.bytes), {
    *               <synthetic> <artifact> val x$43 = child;
    *               result.$colon$colon(x$43)
    *             })
    *           }
    *         }): @scala.unchecked match {
    *           case (result @ scala.Tuple2(_, $colon$colon((lastChild @ _), _))) => scala.Tuple2(result, lastChild)
    *         };
    *         val result = x$44._1;
    *         val lastChild = x$44._2;
    *         <synthetic> <artifact> private[this] val x$45 = scala.Tuple2(lastChild.range.lim, result._2.reverse): @scala.unchecked match {
    *           case scala.Tuple2((lNextFreeAddress @ _), (l @ _)) => scala.Tuple2(lNextFreeAddress, l)
    *         };
    *         val lNextFreeAddress = x$45._1;
    *         val l = x$45._2;
    *         RegisterFileA(AddressRange(address, lNextFreeAddress), reg, l)
    *       }
    *     }
    *   };
    *   ()
    * }
    * }}}
    *
    * @param range
    * @param reg
    * @param l
    */
  @Initialize
  case class RegisterFileA(range: AddressRange, @At(0x10) reg: common.Register, @OfSize(3) @Modulo(0x80) @Increment(0x10) l: List[common.Register])

  /**
    * '@Initialize macro should generate following code:
    * {{{
    * {
    *   object RegisterFileB extends scala.AnyRef {
    *     def <init>() = {
    *       super.<init>();
    *       ()
    *     };
    *     def apply(address: Address): RegisterFileB = {
    *       val initialAddress: Address = address.$plus(512L.bytes);
    *       <synthetic> <artifact> private[this] val x$49 = 1.to(3).foldRight(scala.Tuple2(initialAddress, List.empty[common.Register]))(<empty> match {
    *         case scala.Tuple2(_, scala.Tuple2((previousAddress @ _), (result @ _))) => {
    *           val nextAddress = previousAddress;
    *           val child = common.Register.apply(nextAddress);
    *           scala.Tuple2(child.range.lim, {
    *             <synthetic> <artifact> val x$48 = child;
    *             result.$colon$colon(x$48)
    *           })
    *         }
    *       }): @scala.unchecked match {
    *         case (result @ scala.Tuple2(_, $colon$colon((lastChild @ _), _))) => scala.Tuple2(result, lastChild)
    *       };
    *       val result = x$49._1;
    *       val lastChild = x$49._2;
    *       <synthetic> <artifact> private[this] val x$50 = scala.Tuple2(lastChild.range.lim, result._2.reverse): @scala.unchecked match {
    *         case scala.Tuple2((regsNextFreeAddress @ _), (regs @ _)) => scala.Tuple2(regsNextFreeAddress, regs)
    *       };
    *       val regsNextFreeAddress = x$50._1;
    *       val regs = x$50._2;
    *       {
    *         <synthetic> <artifact> private[this] val x$47 = {
    *           val child = common.Register(regsNextFreeAddress);
    *           scala.Tuple2(child.range.lim, child)
    *         }: @scala.unchecked match {
    *           case scala.Tuple2((rNextFreeAddress @ _), (r @ _)) => scala.Tuple2(rNextFreeAddress, r)
    *         };
    *         val rNextFreeAddress = x$47._1;
    *         val r = x$47._2;
    *         RegisterFileB(AddressRange(address, rNextFreeAddress), regs, r)
    *       }
    *     }
    *   };
    *   ()
    * }
    * }}}
    *
    * @param range
    * @param regs
    * @param r
    */
  @Initialize
  case class RegisterFileB(range: AddressRange, @At(0x200) @OfSize(3) regs: List[common.Register], r: common.Register)
}

package nonContiguous {
  @Initialize
  case class AddressMap(
    range: AddressRange,
    @OfSize(2) @Increment(0x280)
    incrementRegFilesA: List[RegisterFileA],
    incrementRegFileB: RegisterFileB
  )
  @Initialize
  case class RegisterFileA(
    range: AddressRange,
    reg: common.Register,
    @OfSize(2) @Increment(0x12)
    l: List[common.Register]
  )
  @Initialize
  case class RegisterFileB(
    range: AddressRange,
    @OfSize(2) @Increment(0x24)
    regs: List[common.Register],
    r: common.Register
  )
}
