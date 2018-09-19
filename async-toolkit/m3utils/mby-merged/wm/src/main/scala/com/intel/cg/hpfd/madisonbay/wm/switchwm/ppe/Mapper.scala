package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe

import com.intel.cg.hpfd.csr.generated._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.Mapper.{MACLookupResult, MACMapperResult, RewriteProfileType, RewriteSource}
import com.intel.cg.hpfd.madisonbay.wm.util.ImplicitExtensions.nibbles
import ppe._

class Mapper(val csr : mby_ppe_mapper_map) {
  object MACMapper {
    implicit class MacMapperEntry(val c : map_mac_r) {
      def matches(mac : MACAddress) : Boolean = {
        val mask = (1 << (c.IGNORE_LENGTH() + 1)) - 1
        c.MAC() == (mac.addr & mask)
      }
    }

    def highestMatching(mac : MACAddress) : MACLookupResult = {
      val lookup = (0 until csr.MAP_MAC.length).reverse.collectFirst(
        { case i if csr.MAP_MAC(i) matches mac => MACLookupResult(i, csr.MAP_MAC(i).MAC_ROUTABLE() == 1)})
      lookup match {
        case Some(l) => l
        case None => MACLookupResult(0, false)
      }
    }

    def x(fv : MACMapperImposed) : MACMapperResult = {
      MACMapperResult(highestMatching(fv.OuterDMAC), highestMatching(fv.OuterSMAC), highestMatching(fv.InnerDMAC), highestMatching(fv.OuterSMAC))
    }
  }

  object KeyRewrite {
    val c = csr.MAP_REWRITE

    def rewriteNibble(source : RewriteSource, orig : Int, macResult : MACMapperResult) : Int = {
      source match {
        case 0 => orig // 0 defined to mean NOP
        case 4 => macResult.outerDMAC.index.nib(0)
        case 5 => macResult.outerDMAC.index.nib(1)
        case 6 => macResult.outerSMAC.index.nib(0)
        case 7 => macResult.outerSMAC.index.nib(1)
        case 36 => macResult.innerDMAC.index.nib(0)
        case 37 => macResult.innerDMAC.index.nib(1)
        case 38 => macResult.innerSMAC.index.nib(0)
        case 39 => macResult.innerSMAC.index.nib(1)

        case _ => orig // interpret 'reserved' operations as NOP
      }
    }

    def x[T <: PacketFields](fv : PacketFields, rewriteProfile : RewriteProfileType, macResult : MACMapperResult ) : PacketFields = {
      val rewriteCfg = c(rewriteProfile)
      // 32 'nibbles' available for rewriting
      val key16_13_orig = fv.key16(13).toShort
      val key16_19_orig = fv.key16(19).toShort

      val x0 = fv.key16_u(13, ((rewriteNibble(rewriteCfg(0).SRC_ID().toInt,key16_13_orig.nib(0), macResult)).toInt,
                      (rewriteNibble(rewriteCfg(1).SRC_ID().toInt,key16_13_orig.nib(1), macResult)).toInt,
                      (rewriteNibble(rewriteCfg(2).SRC_ID().toInt,key16_13_orig.nib(2), macResult)).toInt,
                      (rewriteNibble(rewriteCfg(3).SRC_ID().toInt,key16_13_orig.nib(3), macResult)).toInt))
      val x1 = x0.key16_u(19, ((rewriteNibble(rewriteCfg(4).SRC_ID().toInt,key16_19_orig.nib(0), macResult)).toInt,
        (rewriteNibble(rewriteCfg(5).SRC_ID().toInt,key16_19_orig.nib(1), macResult)).toInt,
        (rewriteNibble(rewriteCfg(6).SRC_ID().toInt,key16_19_orig.nib(2), macResult)).toInt,
        (rewriteNibble(rewriteCfg(7).SRC_ID().toInt,key16_19_orig.nib(3), macResult)).toInt))
      val x2 = x1.key16_u(19, ((rewriteNibble(rewriteCfg(4).SRC_ID().toInt,key16_19_orig.nib(0), macResult)).toInt,
        (rewriteNibble(rewriteCfg(5).SRC_ID().toInt,key16_19_orig.nib(1), macResult)).toInt,
        (rewriteNibble(rewriteCfg(6).SRC_ID().toInt,key16_19_orig.nib(2), macResult)).toInt,
        (rewriteNibble(rewriteCfg(7).SRC_ID().toInt,key16_19_orig.nib(3), macResult)).toInt))
      x2
      // etc.
    }
  }
}

object Mapper {
  type RewriteProfileType = Int
  type RewriteSource = Int
  case class MACLookupResult (val index : Int, val routeable : Boolean)
  case class MACMapperResult (val outerDMAC : MACLookupResult,
                         val outerSMAC : MACLookupResult,
                         val innerDMAC : MACLookupResult,
                         val innerSMAC : MACLookupResult)


}
