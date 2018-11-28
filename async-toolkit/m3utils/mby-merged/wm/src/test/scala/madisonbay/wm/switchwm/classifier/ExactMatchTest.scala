package madisonbay.wm.switchwm.ppe.classifier

import com.intel.cg.hpfd.madisonbay.Memory._
import madisonbay.csr.all._
import madisonbay.wm.switchwm.ppe.mapper.output._
import madisonbay.wm.switchwm.ppe.mapper.defs.Classifier16BitKeys
import madisonbay.wm.switchwm.ppe.mapper.defs.Classifier32BitKeys

import monocle.{ Optional, Traversal }
import monocle.function.Index._
import monocle.function.Each._
import monocle.state.all._
import scalaz.State

import org.scalatest.{ FlatSpec, Matchers }

class ExactMatchTest extends FlatSpec with Matchers {
  lazy val top = mby_top_map(Address at 0.bytes)

  lazy val mpp = mby_mpp_map(Address at 0.bytes) //<- alternative

  lazy val shmMap = top.mpp.shm
  lazy val cgrpAMap = top.mpp.mgp(0).rx_ppe.cgrp_a
  lazy val cgrpBMap = top.mpp.mgp(0).rx_ppe.cgrp_b

  lazy val em_hash_lookup_reg = cgrpAMap.A.EM_HASH_LOOKUP
  lazy val cgrp_em_map = cgrpAMap.EM

  lazy val _ = (mpp, shmMap, cgrpAMap, cgrpBMap, em_hash_lookup_reg, cgrp_em_map)

  val cgrpAMapEmO: Optional[mby_top_map, mby_ppe_cgrp_em_map] =
    mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
    mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_a composeLens
    mby_ppe_cgrp_a_map._EM

  val cgrpBMapEmO: Optional[mby_top_map, mby_ppe_cgrp_em_map] =
    mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
    mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_b composeLens
    mby_ppe_cgrp_b_map._EM

  def topMapToAEmHashLookupT(bucket: Traversal[List[em_hash_lookup_r], em_hash_lookup_r]): Traversal[mby_top_map, em_hash_lookup_r] =
    mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
    mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_a composeLens
    mby_ppe_cgrp_a_map._A composeLens
    mby_ppe_cgrp_a_nested_map._EM_HASH_LOOKUP composeTraversal bucket

  val topMapToBEmHashLookupT: Traversal[mby_top_map, em_hash_lookup_r] =
    mby_top_map._mpp composeLens mby_mpp_map._mgp composeOptional index(0) composeLens
    mby_mgp_top_map._rx_ppe composeLens mby_ppe_rx_top_map._cgrp_b composeLens
    mby_ppe_cgrp_b_map._B composeLens
    mby_ppe_cgrp_b_nested_map._EM_HASH_LOOKUP composeTraversal each

  private def setEmHashLookupReg(
    topMapToEmHashLookupT: Traversal[mby_top_map, em_hash_lookup_r]
  )(
    ptr: em_hash_lookup_r.PTR,
    select0: em_hash_lookup_r.SELECT_0,
    select1: em_hash_lookup_r.SELECT_1,
    select2: em_hash_lookup_r.SELECT_2,
    select3: em_hash_lookup_r.SELECT_3,
    select4: em_hash_lookup_r.SELECT_4,
    mask: em_hash_lookup_r.MASK
  ): State[mby_top_map, Unit] = {
    import madisonbay.csr.all.em_hash_lookup_r._
    for {
      _ <- (topMapToEmHashLookupT composeLens _PTR).assign(ptr)
      _ <- (topMapToEmHashLookupT composeLens _SELECT_0).assign(select0)
      _ <- (topMapToEmHashLookupT composeLens _SELECT_1).assign(select1)
      _ <- (topMapToEmHashLookupT composeLens _SELECT_2).assign(select2)
      _ <- (topMapToEmHashLookupT composeLens _SELECT_3).assign(select3)
      _ <- (topMapToEmHashLookupT composeLens _SELECT_4).assign(select4)
      _ <- (topMapToEmHashLookupT composeLens _MASK).assign(mask)
    } yield ()
  }

  private def setEmHashCamReg(
    topMapToCgrpEmMapO: Optional[mby_top_map, mby_ppe_cgrp_em_map]
  ): State[mby_top_map, Unit] = {
    val topMapToCamData: Traversal[mby_top_map, em_hash_cam_r.DATA] =
      topMapToCgrpEmMapO composeLens
      mby_ppe_cgrp_em_map._HASH_CAM composeTraversal each composeLens
      em_hash_cam_rf._EM_HASH_CAM composeTraversal each composeLens em_hash_cam_r._DATA

    topMapToCamData.assign_(em_hash_cam_r.DATA(0))
  }

  private def setEmHashCamEnReg(
    topMapToCgrpEmMapO: Optional[mby_top_map, mby_ppe_cgrp_em_map]
  ): State[mby_top_map, Unit] = {
    val topMapToEmHashCamEnMask =
      topMapToCgrpEmMapO composeLens
      mby_ppe_cgrp_em_map._HASH_CAM_EN composeTraversal each composeLens
      em_hash_cam_en_rf._EM_HASH_CAM_EN composeTraversal each composeLens em_hash_cam_en_r._MASK

    topMapToEmHashCamEnMask.assign_(em_hash_cam_en_r.MASK(0))
  }

  private def setEmKeySel0Reg(
    topMapToCgrpEmMapO: Optional[mby_top_map, mby_ppe_cgrp_em_map],
    hash: Traversal[List[em_key_sel0_rf], em_key_sel0_rf],
    packetProfile: Traversal[List[em_key_sel0_r], em_key_sel0_r]
  )(
    value: em_key_sel0_r.KEY8_MASK
  ): State[mby_top_map, Unit] = {  
    val topMapToEmKeySel0Key8Mask =
      topMapToCgrpEmMapO composeLens
      mby_ppe_cgrp_em_map._KEY_SEL0 composeTraversal
      hash composeLens
      em_key_sel0_rf._EM_KEY_SEL0 composeTraversal
      packetProfile composeLens em_key_sel0_r._KEY8_MASK

    topMapToEmKeySel0Key8Mask.assign_(value)
  }

  private def setEmKeySel1Reg(
    topMapToCgrpEmMapO: Optional[mby_top_map, mby_ppe_cgrp_em_map],
    hash: Traversal[List[em_key_sel1_rf], em_key_sel1_rf],
    profile: Traversal[List[em_key_sel1_r], em_key_sel1_r]
  )(
    keyMaskSel: em_key_sel1_r.KEY_MASK_SEL,
    key32Mask: em_key_sel1_r.KEY32_MASK,
    key16Mask: em_key_sel1_r.KEY16_MASK,
  ): State[mby_top_map, Unit] = {
    val topMapToEmKeySel1 =
      topMapToCgrpEmMapO composeLens
      mby_ppe_cgrp_em_map._KEY_SEL1 composeTraversal
      hash composeLens
      em_key_sel1_rf._EM_KEY_SEL1 composeTraversal profile

    import madisonbay.csr.all.em_key_sel1_r._
    for {
      _ <- (topMapToEmKeySel1 composeLens _KEY_MASK_SEL).assign_(keyMaskSel)
      _ <- (topMapToEmKeySel1 composeLens _KEY32_MASK).assign_(key32Mask)
      _ <- (topMapToEmKeySel1 composeLens _KEY16_MASK).assign_(key16Mask)
    } yield ()
  }

  private def setEmKeyMaskReg(
    topMapToCgrpEmMapO: Optional[mby_top_map, mby_ppe_cgrp_em_map]
  ): State[mby_top_map, Unit] = {
    val topMapToEmKeyMask =
      topMapToCgrpEmMapO composeLens
      mby_ppe_cgrp_em_map._KEY_MASK composeTraversal each composeLens
      em_key_mask_rf._EM_KEY_MASK composeTraversal each composeLens
      em_key_mask_r._MASK

    // TODO: to be fixed with C
    topMapToEmKeyMask.assign_(em_key_mask_r.MASK(0))
  }

  private def setEmHashMissReg(
    topMapToCgrpEmMapO: Optional[mby_top_map, mby_ppe_cgrp_em_map]
  ): State[mby_top_map, Unit] = {
    val topMapToEmHashMiss =
      topMapToCgrpEmMapO composeLens
      mby_ppe_cgrp_em_map._HASH_MISS composeTraversal each composeLens
      em_hash_miss_rf._EM_HASH_MISS composeTraversal each

    import madisonbay.csr.all.em_hash_miss_r._
    for {
      _ <- (topMapToEmHashMiss composeLens _ACTION1).assign_(ACTION1(0))
      _ <- (topMapToEmHashMiss composeLens _ACTION0).assign_(ACTION0(0))
    } yield ()
  }

  private def setEmHashCfgReg(
    topMapToCgrpEmMapO: Optional[mby_top_map, mby_ppe_cgrp_em_map],
    profile: Traversal[List[em_hash_cfg_r], em_hash_cfg_r]
  )(
    mode: em_hash_cfg_r.MODE,
    basePtr0: em_hash_cfg_r.BASE_PTR_0,
    basePtr1: em_hash_cfg_r.BASE_PTR_1,
    hashSize0: em_hash_cfg_r.HASH_SIZE_0,
    hashSize1: em_hash_cfg_r.HASH_SIZE_1,
    entrySize0: em_hash_cfg_r.ENTRY_SIZE_0,
    entrySize1: em_hash_cfg_r.ENTRY_SIZE_1
  ): State[mby_top_map, Unit] = {
    val topMapToHashCfg =
      topMapToCgrpEmMapO composeLens
      mby_ppe_cgrp_em_map._HASH_CFG composeTraversal profile

    import madisonbay.csr.all.em_hash_cfg_r._
    for {
      _ <- (topMapToHashCfg composeLens _MODE).assign_(mode)
      _ <- (topMapToHashCfg composeLens _BASE_PTR_0).assign_(basePtr0)
      _ <- (topMapToHashCfg composeLens _BASE_PTR_1).assign_(basePtr1)
      _ <- (topMapToHashCfg composeLens _HASH_SIZE_0).assign_(hashSize0)
      _ <- (topMapToHashCfg composeLens _HASH_SIZE_1).assign_(hashSize1)
      _ <- (topMapToHashCfg composeLens _ENTRY_SIZE_0).assign_(entrySize0)
      _ <- (topMapToHashCfg composeLens _ENTRY_SIZE_1).assign_(entrySize1)
    } yield ()
  }

  private def setFwdTable0Reg(
    block: Traversal[List[fwd_table0_rf], fwd_table0_rf],
    cell: Traversal[List[fwd_table0_r], fwd_table0_r]
  )(data: fwd_table0_r.DATA): State[mby_top_map, Unit] = {
    val topMapToFwdTable0 = mby_top_map._mpp composeLens mby_mpp_map._shm composeLens 
      mby_shm_map._FWD_TABLE0 composeTraversal block composeLens
      fwd_table0_rf._FWD_TABLE0 composeTraversal cell composeLens
      fwd_table0_r._DATA

    topMapToFwdTable0.assign_(data)
  }

  private def setFwdTable1Reg: State[mby_top_map, Unit] = {
    val topMapToFwdTable1 = mby_top_map._mpp composeLens mby_mpp_map._shm composeLens
      mby_shm_map._FWD_TABLE1 composeTraversal each composeLens
      fwd_table1_rf._FWD_TABLE1 composeTraversal each composeLens
    fwd_table1_r._DATA

    topMapToFwdTable1.assign_(fwd_table1_r.DATA(0))
  }

  private def atIndex[A](i: Int) = listIndex[A].index(i).asTraversal

  import em_key_sel0_r.KEY8_MASK
  import em_key_sel1_r.{KEY_MASK_SEL, KEY32_MASK, KEY16_MASK}
  import em_hash_cfg_r.{MODE, BASE_PTR_0, BASE_PTR_1, HASH_SIZE_0, HASH_SIZE_1, ENTRY_SIZE_0, ENTRY_SIZE_1}
  import em_hash_lookup_r.{PTR, SELECT_0, SELECT_1, SELECT_2, SELECT_3, SELECT_4, MASK}
  import fwd_table0_r.DATA

  val initRecipt = for {
    // Classifier A
    _ <- setEmHashLookupReg(topMapToAEmHashLookupT(each))(
      PTR(0), SELECT_0(0), SELECT_1(0), SELECT_2(0), SELECT_3(0), SELECT_4(0), MASK(0)
    )
    _ <- setEmHashCamReg(cgrpAMapEmO)
    _ <- setEmHashCamEnReg(cgrpAMapEmO)
    _ <- setEmKeySel0Reg(cgrpAMapEmO, each, each)(em_key_sel0_r.KEY8_MASK(0))
    _ <- setEmKeySel1Reg(cgrpAMapEmO, each, each)(KEY_MASK_SEL(0), KEY32_MASK(0), KEY16_MASK(0))
    _ <- setEmKeyMaskReg(cgrpAMapEmO) // TODO: in C it selects only part of elems to fill/clear
    _ <- setEmHashMissReg(cgrpAMapEmO)
    _ <- setEmHashCfgReg(cgrpAMapEmO, each)(
      MODE(0),
      BASE_PTR_0(0),
      BASE_PTR_1(0),
      HASH_SIZE_0(0),
      HASH_SIZE_1(0),
      ENTRY_SIZE_0(0),
      ENTRY_SIZE_1(0)
    )
    // Classifier B
    _ <- setEmHashLookupReg(topMapToBEmHashLookupT)(
      PTR(0), SELECT_0(0), SELECT_1(0), SELECT_2(0), SELECT_3(0), SELECT_4(0), MASK(0)
    )
    _ <- setEmHashCamReg(cgrpBMapEmO)
    _ <- setEmHashCamEnReg(cgrpBMapEmO)
    _ <- setEmKeySel0Reg(cgrpBMapEmO, each, each)(KEY8_MASK(0))
    _ <- setEmKeySel1Reg(cgrpBMapEmO, each, each)(KEY_MASK_SEL(0), KEY32_MASK(0), KEY16_MASK(0))
    _ <- setEmKeyMaskReg(cgrpBMapEmO) // TODO: in C it selects only part of elems to fill/clear
    _ <- setEmHashMissReg(cgrpBMapEmO)
    _ <- setEmHashCfgReg(cgrpBMapEmO, each)(
      MODE(0),
      BASE_PTR_0(0),
      BASE_PTR_1(0),
      HASH_SIZE_0(0),
      HASH_SIZE_1(0),
      ENTRY_SIZE_0(0),
      ENTRY_SIZE_1(0)
    )
    // Shared memory
    _ <- setFwdTable0Reg(each, each)(DATA(0))
    _ <- setFwdTable1Reg
  } yield ()

  // setRegs_basic
  lazy val setRegsRecipt = for {
    _ <- setEmKeySel0Reg(cgrpAMapEmO, atIndex(0), atIndex(17))(KEY8_MASK(0x20))
    _ <- setEmKeySel1Reg(cgrpAMapEmO, atIndex(0), atIndex(17))(
      KEY_MASK_SEL(0),
      KEY32_MASK(0x10),
      KEY16_MASK(0),
    )
    _ <- setEmHashCfgReg(cgrpAMapEmO, atIndex(17))(
      MODE(1), // mode (64B == non-split mode)
      BASE_PTR_0(0),
      BASE_PTR_1(0),
      HASH_SIZE_0(0x6),
      HASH_SIZE_1(0),
      ENTRY_SIZE_0(0x3),
      ENTRY_SIZE_1(0)
    )
    _ <- setEmHashLookupReg(topMapToAEmHashLookupT(atIndex(0xC)))(
      PTR(0), SELECT_0(0), SELECT_1(0x1), SELECT_2(0x2), SELECT_3(0x3), SELECT_4(0x4), MASK(0x20000)
    )
    _ <- setFwdTable0Reg(atIndex(0), atIndex(0))(DATA(0x12341234L << 32))
    _ <- setFwdTable0Reg(atIndex(0), atIndex(1))(DATA(0x54210000L << 32))
  } yield ()

  def mockedMapperOutput: MapperOutput = {
    val oneZero = ActionPrecVal(1,0)
    val oneOne = ActionPrecVal(1,1)

    val classifierActions = ClassifierActions(
      act24 = Vector.fill(16)(oneZero),
      act4 = Vector.fill(26)(oneZero)
        .updated(4, oneOne),
      act1 = Vector.fill(24)(oneZero)
        .updated(20, oneOne)
        .updated(22, oneOne)
    )

    /*
    set based on the SQA test

    Frame Header Data:
    00  11  22  33  44  55  00  01  01  01  01  01  81  00  00  02
    08  00  45  00  00  14  00  00  00  00  ff  06  cc  45  b0  0a
    1b  2c  12  34  12  34
     */
    val classifierKeys = ClassifierKeys(
      key32 = Map(
        Classifier32BitKeys.OuterSourceIp -> 0xB00A1B2C,
        Classifier32BitKeys.OuterDestiantionIp -> 0x12341234,
        Classifier32BitKeys.wildcard(4) -> 0x12341234
      ),
      key16 = Map(
        Classifier16BitKeys.OuterDestinationMac0 -> 0x0011,
        Classifier16BitKeys.OuterDestinationMac1 -> 0x2233,
        Classifier16BitKeys.OuterDestinationMac2 -> 0x4455,

        Classifier16BitKeys.OuterSourceMac0 -> 0x0001,
        Classifier16BitKeys.OuterSourceMac1 -> 0x0101,
        Classifier16BitKeys.OuterSourceMac2 -> 0x0101,

        Classifier16BitKeys.OuterEthertype -> 0x0800,
        Classifier16BitKeys.OuterVlan1 -> 0x0001,
        Classifier16BitKeys.OuterVlan2 -> 0x0001,
        Classifier16BitKeys.OuterL4Source -> 0x0001,
        Classifier16BitKeys.OuterL4Destination -> 0x0203,
        Classifier16BitKeys.wildcard(19) -> 0x0203,
        Classifier16BitKeys.wildcard(31) -> 0x0002
      ),
      key8 = Vector.fill(64)(0)
        .updated(3, 0x11)
        .updated(6, 0x40)
        .updated(20, 0xff)
        .updated(21, 0x06)
        .updated(23, 0x14)
        .updated(24, 0x40)
        .updated(28, 0x01)
        .updated(29, 0x02)
        .updated(43, 0x60)
        .updated(45, 0x02)
        .updated(55, 0xA1)
        .updated(56, 0x07)
        .updated(57, 0xF6)
        .updated(58, 0xE5)
        .updated(59, 0xD4)
        .map(_.toByte)
    )

    MapperOutput(
      classifierActions = classifierActions,
      classifierKeys = classifierKeys,
      classifierProfile = 0x11, // 17
      ipOption = Array.empty,
      priorityProfile = 0,
      noPriorityEncoding = false,
      learningMode = SharedVlanLearning,
      l2IngressVlan1Counter = 0
    )
  }


  it should "clear & initialize required registers" in {
    val initialization = for {
      _ <- initRecipt
      _ <- setRegsRecipt
    } yield mockedMapperOutput

    val result = initialization.exec(top)

    result.mpp.mgp(0).rx_ppe.cgrp_a.EM.HASH_CFG(17).HASH_SIZE_0() shouldEqual 0x6
    result.mpp.mgp(0).rx_ppe.cgrp_a.A.EM_HASH_LOOKUP(0xC).SELECT_1() shouldEqual 0x01
    result.mpp.shm.FWD_TABLE0(0).FWD_TABLE0(0).DATA().toHexString shouldEqual "1234123400000000"
  }
}
