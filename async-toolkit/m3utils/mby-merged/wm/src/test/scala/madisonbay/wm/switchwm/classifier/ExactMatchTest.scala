package madisonbay.wm.switchwm.ppe.classifier

import com.intel.cg.hpfd.madisonbay.Memory._
import monocle.Traversal
import org.scalatest.{ FlatSpec, Matchers }

import madisonbay.csr.all._


import monocle.function.Index._
import monocle.function.Each._
import monocle.state.all._
import scalaz.State

class ExactMatchTest extends FlatSpec with Matchers {
  lazy val top = mby_top_map(Address at 0.bytes)

  lazy val mpp = mby_mpp_map(Address at 0.bytes) //<- alternative

  lazy val shmMap = top.mpp.shm
  lazy val cgrpAMap = top.mpp.mgp(0).rx_ppe.cgrp_a
  lazy val cgrpBMap = top.mpp.mgp(0).rx_ppe.cgrp_b

  lazy val em_hash_lookup_reg = cgrpAMap.A.EM_HASH_LOOKUP
  lazy val cgrp_em_map = cgrpAMap.EM

  private def setEmHashLookupReg: State[mby_top_map, Unit] = {
    val topMapToHashLookupO: Traversal[mby_top_map, em_hash_lookup_r] =
      mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
      mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_a composeLens
      mby_ppe_cgrp_a_map._A composeLens
      mby_ppe_cgrp_a_nested_map._EM_HASH_LOOKUP composeTraversal each

    import madisonbay.csr.all.em_hash_lookup_r._
    for {
      _ <- (topMapToHashLookupO composeLens _PTR).assign(PTR(0))
      _ <- (topMapToHashLookupO composeLens _SELECT_0).assign(SELECT_0(0))
      _ <- (topMapToHashLookupO composeLens _SELECT_1).assign(SELECT_1(0))
      _ <- (topMapToHashLookupO composeLens _SELECT_2).assign(SELECT_2(0))
      _ <- (topMapToHashLookupO composeLens _SELECT_3).assign(SELECT_3(0))
      _ <- (topMapToHashLookupO composeLens _SELECT_4).assign(SELECT_4(0))
      _ <- (topMapToHashLookupO composeLens _MASK).assign(MASK(0))
    } yield ()
  }

  private def setEmHashCamReg: State[mby_top_map, Unit] = {
    val topMapToCamData: Traversal[mby_top_map, em_hash_cam_r.DATA] =
      mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
      mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_a composeLens
      mby_ppe_cgrp_a_map._EM composeLens
      mby_ppe_cgrp_em_map._HASH_CAM composeTraversal each composeLens
      em_hash_cam_rf._EM_HASH_CAM composeTraversal each composeLens em_hash_cam_r._DATA

    topMapToCamData.assign_(em_hash_cam_r.DATA(0))
  }

  private def setEmHashCamEnReg: State[mby_top_map, Unit] = {
    val topMapToEmHashCamEnMask =
      mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
      mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_a composeLens
      mby_ppe_cgrp_a_map._EM composeLens
      mby_ppe_cgrp_em_map._HASH_CAM_EN composeTraversal each composeLens
      em_hash_cam_en_rf._EM_HASH_CAM_EN composeTraversal each composeLens em_hash_cam_en_r._MASK

    topMapToEmHashCamEnMask.assign_(em_hash_cam_en_r.MASK(0))
  }

  private def setEmKeySel0Reg: State[mby_top_map, Unit] = {
    val topMapToEmKeySel0Key8Mask =
      mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
      mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_a composeLens
      mby_ppe_cgrp_a_map._EM composeLens
      mby_ppe_cgrp_em_map._KEY_SEL0 composeTraversal each composeLens
      em_key_sel0_rf._EM_KEY_SEL0 composeTraversal each composeLens em_key_sel0_r._KEY8_MASK

    topMapToEmKeySel0Key8Mask.assign_(em_key_sel0_r.KEY8_MASK(0))
  }

  private def setEmKeySel1Reg: State[mby_top_map, Unit] = {
    val topMapToEmKeySel1 =
      mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
      mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_a composeLens
      mby_ppe_cgrp_a_map._EM composeLens
      mby_ppe_cgrp_em_map._KEY_SEL1 composeTraversal each composeLens
      em_key_sel1_rf._EM_KEY_SEL1 composeTraversal each

    import madisonbay.csr.all.em_key_sel1_r._
    for {
      _ <- (topMapToEmKeySel1 composeLens _KEY_MASK_SEL).assign_(KEY_MASK_SEL(0))
      _ <- (topMapToEmKeySel1 composeLens _KEY32_MASK).assign_(KEY32_MASK(0))
      _ <- (topMapToEmKeySel1 composeLens _KEY16_MASK).assign_(KEY16_MASK(0))
    } yield ()
  }

  private def setEmKeyMaskReg: State[mby_top_map, Unit] = {
    val topMapToEmKeyMask =
      mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
      mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_a composeLens
      mby_ppe_cgrp_a_map._EM composeLens
      mby_ppe_cgrp_em_map._KEY_MASK composeTraversal each composeLens
      em_key_mask_rf._EM_KEY_MASK composeTraversal each composeLens
      em_key_mask_r._MASK

    // TODO: to be fixed with C
    topMapToEmKeyMask.assign_(em_key_mask_r.MASK(0))
  }

  private def setEmHashMissReg: State[mby_top_map, Unit] = {
    val topMapToEmHashMiss =
      mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
      mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_a composeLens
      mby_ppe_cgrp_a_map._EM composeLens
      mby_ppe_cgrp_em_map._HASH_MISS composeTraversal each composeLens
      em_hash_miss_rf._EM_HASH_MISS composeTraversal each

    import madisonbay.csr.all.em_hash_miss_r._
    for {
      _ <- (topMapToEmHashMiss composeLens _ACTION1).assign_(ACTION1(0))
      _ <- (topMapToEmHashMiss composeLens _ACTION0).assign_(ACTION0(0))
    } yield ()
  }

  private def setEmHashCfgReg: State[mby_top_map, Unit] = {
    val topMapToHashCfg =
      mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
      mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_a composeLens
      mby_ppe_cgrp_a_map._EM composeLens
      mby_ppe_cgrp_em_map._HASH_CFG composeTraversal each

    import madisonbay.csr.all.em_hash_cfg_r._
    for {
      _ <- (topMapToHashCfg composeLens _MODE).assign_(MODE(0))
      _ <- (topMapToHashCfg composeLens _BASE_PTR_0).assign_(BASE_PTR_0(0))
      _ <- (topMapToHashCfg composeLens _BASE_PTR_1).assign_(BASE_PTR_1(0))
      _ <- (topMapToHashCfg composeLens _HASH_SIZE_0).assign_(HASH_SIZE_0(0))
      _ <- (topMapToHashCfg composeLens _HASH_SIZE_1).assign_(HASH_SIZE_1(0))
      _ <- (topMapToHashCfg composeLens _ENTRY_SIZE_0).assign_(ENTRY_SIZE_0(0))
      _ <- (topMapToHashCfg composeLens _ENTRY_SIZE_1).assign_(ENTRY_SIZE_1(0))
    } yield ()
  }

  val initRecipt = for {
    _ <- setEmHashLookupReg
    _ <- setEmHashCamReg
    _ <- setEmHashCamEnReg
    _ <- setEmKeySel0Reg
    _ <- setEmKeySel1Reg
    _ <- setEmKeyMaskReg // TODO: in C it selects only part of elems to fill/clear
    _ <- setEmHashMissReg
    _ <- setEmHashCfgReg
  } yield ()

  lazy val _ = (shmMap, cgrpAMap, cgrpBMap, em_hash_lookup_reg, cgrp_em_map, initRecipt)

  it should "pass" in {
    true shouldEqual true
  }
}
