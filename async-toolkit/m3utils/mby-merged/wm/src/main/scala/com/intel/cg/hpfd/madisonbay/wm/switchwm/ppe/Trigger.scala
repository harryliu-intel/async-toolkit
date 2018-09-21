
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe

import com.intel.cg.hpfd.csr.generated._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline.PipelineStage
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.Trigger.MatchCase
import ppe.{PortIndex, TrafficClass, VID}

import scala.collection.immutable.BitSet


class TriggerUnit extends PipelineStage[FrameState,FrameState] {
  val x: FrameState => FrameState = fs => fs
}

class Triggers(val ppe_cfg: mby_ppe_rx_top_map) extends IndexedSeq[Trigger] {

  val tcfg = new TrigCfg(ppe_cfg.trig_apply, ppe_cfg.trig_apply_misc, ppe_cfg.trig_usage)

  def apply(i: Int): Trigger = {
    new Trigger(tcfg, i)
  }

  def length: Int = tcfg.TRIGGER_ACTION_CFG_1.length // no way to get this more safely (assumes all of the trigger cfg registers are the same length

  def firingTriggers(fs: FrameState): Iterable[Trigger] = {
    val result = List.newBuilder[Trigger]
    var firedThisPrecedenceStage  = false
    (0 until length).map { i =>
      val trig = apply(i)
      if (tcfg.TRIGGER_CONDITION_CFG(i).MATCH_BY_PRECEDENCE() == 0) firedThisPrecedenceStage = false
      if (!firedThisPrecedenceStage && trig.c(fs)) result += trig
    }
    result.result()
  }

}

abstract class TriggerCondition {
  val x: FrameState => Boolean
}

class CgrpCondition(apply_map: mby_ppe_trig_apply_map, index: Int) extends TriggerCondition {

  val tcc = apply_map.TRIGGER_CONDITION_CGRP(index)
  val condition = MatchCase(apply_map.TRIGGER_CONDITION_CFG(index).MATCH_CGRP().toInt)
  val x: FrameState => Boolean = fs => {
    condition match {
      case MatchCase.Unconditional => true
      case MatchCase.Equal => fs.cgrp == (tcc.CGRP_ID(), tcc.CGRP_MASK())
      case MatchCase.NotEqual => !(fs.cgrp == (tcc.CGRP_ID(), tcc.CGRP_MASK()))
    }
  }

}

class SourcePortCondition(apply_map: mby_ppe_trig_apply_map, index: Int) extends TriggerCondition {
  lazy val spMask: BitSet =  BitSet.fromBitMask(Array[Long](apply_map.TRIGGER_CONDITION_RX(index).SRC_PORT_MASK()))
  val x: FrameState => Boolean = fs => {
    spMask.contains(fs.rxPort.p)
  }
}

/**
  * Abstract trigger CSRs into a single logic operation, so as to avoid requirement
  * the author know precisely where a given CSR occurs
  * @param apply_map
  * @param apply_misc_map
  * @param usage_map
  */
class TrigCfg(val apply_map: mby_ppe_trig_apply_map,
              val apply_misc_map: mby_ppe_trig_apply_misc_map,
              val usage_map: mby_ppe_trig_usage_map) {
  // how can I express an apply method such that
  // tcfg(1) returns a type that can be implicitly converted into _all_ of the
  // individual registers at the given index within the several maps?
}

object TrigCfg {
  implicit def tcfgToApplyMap(x: TrigCfg): mby_ppe_trig_apply_map = x.apply_map
  implicit def tcfgToApplyMiscMap(x: TrigCfg): mby_ppe_trig_apply_misc_map = x.apply_misc_map
  implicit def tcfgToUsageMap(x: TrigCfg): mby_ppe_trig_usage_map = x.usage_map
}




class Trigger(val tcfg: TrigCfg, val index: Int) {

  lazy val srcPortCondition = new SourcePortCondition(tcfg, index)
  lazy val cgrpCondition = new CgrpCondition(tcfg, index)
  lazy val stats = tcfg.TRIGGER_STATS(index)

  val c: FrameState => Boolean = fs => {
    val fire = List(cgrpCondition.x, srcPortCondition.x).forall(x => x(fs))
    if (fire) stats.COUNT() = stats.COUNT() + 1
    fire
  }

}


object Trigger {

  object MatchCase extends Enumeration {
    val NotEqual: Value = Value(0, "NotEqual")
    val Equal: Value = Value(1, "Equal")
    val Unconditional: Value = Value(2, "NotEqual")
  }

  /**
    * Simple case of trigger behaviors defined by a when a precedence group consists of a _single_ trigger
   */
  class TrigPipeline(csr: mby_ppe_rx_top_map, trigIdx: Int) extends PipelineStage[FrameState, FrameState] {

    val ta_cfg1 = csr.trig_apply.TRIGGER_ACTION_CFG_1(trigIdx)
    val ta_cfg2 = csr.trig_apply.TRIGGER_ACTION_CFG_2(trigIdx)

    // (obviously needs to actually do filtering!
    def lagFilter(dm: Set[PortIndex]): Set[PortIndex] = dm

    /**
      * A forwarding action is special, in that it should halt further actions in this precedence groupo
      */
    val fwdAction: FrameState => FrameState = { fs =>
      val ta_glort = csr.trig_apply.TRIGGER_ACTION_GLORT(trigIdx)
      ForwardingActionEnum(ta_cfg1.FORWARDING_ACTION().toInt) match {
        case ForwardingActionEnum.Forward =>
          val newDglort = fs.dglort.maskUpdate(ta_glort.NEW_DEST_GLORT().toShort, ta_glort.NEW_DEST_GLORT_MASK().toShort)
          // need to recompute destination mask as well here!
          fs.copy(dglort = newDglort, destPorts = Set.empty)
        case ForwardingActionEnum.Redirect =>
          val newDglort = fs.dglort.maskUpdate(ta_glort.NEW_DEST_GLORT().toShort, ta_glort.NEW_DEST_GLORT_MASK().toShort)
          val filterDestMask = 0 // need to get this from another indirect CSR somehow
          val destPorts = filterDestMask match {
            case 0 => fs.destPorts // need to get from the indirect CSR...
            case 1 => lagFilter(fs.destPorts)
          }
          fs.copy(dglort = newDglort, destPorts = destPorts)
        case ForwardingActionEnum.Drop =>
          fs.copy(destPorts = Set.empty) // really should remove dest ports in mask registers
          // however, the CSR for the 258-bit drop mask is a 'proxy' register, not accessible
          // in our current CSR API
        case _ => fs
      }
    }

    val tcAction: FrameState => FrameState = { fs =>
      TcActionEnum(ta_cfg1.TC_ACTION().toInt) match {
        case TcActionEnum.ReassignTc => fs.copy(tc = new TrafficClass(ta_cfg2.NEW_TC().toInt))
        case _ => fs
      }
    }

    val vlanAction: FrameState => FrameState = { fs =>
      VlanActionEnum(ta_cfg1.VLAN_ACTION().toInt) match {
        case VlanActionEnum.Reassign => fs.copy(egressVidCfg = new VID(ta_cfg2.NEW_EVID().toInt))
        case _ => fs
      }
    }

    val x: FrameState => FrameState = fwdAction andThen tcAction andThen vlanAction
  }


  // below would be much, much better to generate from RDL enumerations!
  object VlanActionEnum extends Enumeration {
    /* No change in VLAN. */
    val AsIs: Value = Value(0, "AsIs")
    /* Override the egress VLAN to the specified value. This only affects L2-switched and L3-unicast frames.
    IP multicast replication, if applicable, still produces frames with the same egress VLANs
    as if this action was not specified. */
    val Reassign: Value = Value(1, "Reassign")
  }

  object ForwardingActionEnum extends Enumeration {
    val AsIs: Value = Value(0, "AsIs")
    val Forward: Value = Value(1, "Forward")
    val Redirect: Value = Value(2, "Redirect")
    val Drop: Value = Value(3, "Drop")
  }

  object TcActionEnum extends Enumeration {
    val AsIs: Value = Value(0, "AsIs")
    val ReassignTc: Value = Value(1, "ReassignTc")
  }

}
