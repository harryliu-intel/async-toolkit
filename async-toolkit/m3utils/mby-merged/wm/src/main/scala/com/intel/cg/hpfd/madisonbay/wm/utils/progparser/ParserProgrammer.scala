package com.intel.cg.hpfd.madisonbay.wm.utils.progparser

import com.intel.cg.hpfd.madisonbay.BitVector
import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.{Csr, CsrLenses, ParserLenses}
import com.intel.cg.hpfd.madisonbay.wm.utils.Json.JsonMap
import monocle.function.Index.listIndex
import monocle.state.all._

import scala.annotation.tailrec

//scalastyle:off
object ParserProgrammer {

  val lens_MBY_PARSER_PORT_CFG_1 = mby_ppe_parser_map._PARSER_PORT_CFG
  def lens_MBY_PARSER_PORT_CFG_2(x : Int) = listIndex[parser_port_cfg_r].index(x)
  val lens_MBY_PARSER_PORT_CFG_3 = parser_port_cfg_r._state
  def lens_MBY_PARSER_PORT_CFG(x : Int) = lens_MBY_PARSER_PORT_CFG_1 composeOptional
    lens_MBY_PARSER_PORT_CFG_2(x) composeLens lens_MBY_PARSER_PORT_CFG_3

  val lens_MBY_PARSER_KEY_W_1 = mby_ppe_parser_map._PARSER_KEY_W
  def lens_MBY_PARSER_KEY_W_2(x : Int) = listIndex[parser_key_w_rf].index(x)
  val lens_MBY_PARSER_KEY_W_3 = parser_key_w_rf._PARSER_KEY_W
  def lens_MBY_PARSER_KEY_W_4(y : Int) = listIndex[parser_key_w_r].index(y)
  val lens_MBY_PARSER_KEY_W_5 = parser_key_w_r._state
  def lens_MBY_PARSER_KEY_W(x : Int, y : Int) = lens_MBY_PARSER_KEY_W_1 composeOptional
    lens_MBY_PARSER_KEY_W_2(x) composeLens lens_MBY_PARSER_KEY_W_3 composeOptional lens_MBY_PARSER_KEY_W_4(y) composeLens lens_MBY_PARSER_KEY_W_5

  val lens_MBY_PARSER_KEY_S_1 = mby_ppe_parser_map._PARSER_KEY_S
  def lens_MBY_PARSER_KEY_S_2(x : Int) = listIndex[parser_key_s_rf].index(x)
  val lens_MBY_PARSER_KEY_S_3 = parser_key_s_rf._PARSER_KEY_S
  def lens_MBY_PARSER_KEY_S_4(y : Int) = listIndex[parser_key_s_r].index(y)
  val lens_MBY_PARSER_KEY_S_5 = parser_key_s_r._state
  def lens_MBY_PARSER_KEY_S(x : Int, y : Int) = lens_MBY_PARSER_KEY_S_1 composeOptional
    lens_MBY_PARSER_KEY_S_2(x) composeLens lens_MBY_PARSER_KEY_S_3 composeOptional lens_MBY_PARSER_KEY_S_4(y) composeLens lens_MBY_PARSER_KEY_S_5

  val lens_MBY_PARSER_ANA_W_1 = mby_ppe_parser_map._PARSER_ANA_W
  def lens_MBY_PARSER_ANA_W_2(x : Int) = listIndex[parser_ana_w_rf].index(x)
  val lens_MBY_PARSER_ANA_W_3 = parser_ana_w_rf._PARSER_ANA_W
  def lens_MBY_PARSER_ANA_W_4(y : Int) = listIndex[parser_ana_w_r].index(y)
  val lens_MBY_PARSER_ANA_W_5 = parser_ana_w_r._state
  def lens_MBY_PARSER_ANA_W(x : Int, y : Int) = lens_MBY_PARSER_ANA_W_1 composeOptional
    lens_MBY_PARSER_ANA_W_2(x) composeLens lens_MBY_PARSER_ANA_W_3 composeOptional lens_MBY_PARSER_ANA_W_4(y) composeLens lens_MBY_PARSER_ANA_W_5

  val lens_MBY_PARSER_ANA_S_1 = mby_ppe_parser_map._PARSER_ANA_S
  def lens_MBY_PARSER_ANA_S_2(x : Int) = listIndex[parser_ana_s_rf].index(x)
  val lens_MBY_PARSER_ANA_S_3 = parser_ana_s_rf._PARSER_ANA_S
  def lens_MBY_PARSER_ANA_S_4(y : Int) = listIndex[parser_ana_s_r].index(y)
  val lens_MBY_PARSER_ANA_S_5 = parser_ana_s_r._state
  def lens_MBY_PARSER_ANA_S(x : Int, y : Int) = lens_MBY_PARSER_ANA_S_1 composeOptional
    lens_MBY_PARSER_ANA_S_2(x) composeLens lens_MBY_PARSER_ANA_S_3 composeOptional lens_MBY_PARSER_ANA_S_4(y) composeLens lens_MBY_PARSER_ANA_S_5

  val lens_MBY_PARSER_EXC_1 = mby_ppe_parser_map._PARSER_EXC
  def lens_MBY_PARSER_EXC_2(x : Int) = listIndex[parser_exc_rf].index(x)
  val lens_MBY_PARSER_EXC_3 = parser_exc_rf._PARSER_EXC
  def lens_MBY_PARSER_EXC_4(y : Int) = listIndex[parser_exc_r].index(y)
  val lens_MBY_PARSER_EXC_5 = parser_exc_r._state
  def lens_MBY_PARSER_EXC(x : Int, y : Int) = lens_MBY_PARSER_EXC_1 composeOptional
    lens_MBY_PARSER_EXC_2(x) composeLens lens_MBY_PARSER_EXC_3 composeOptional lens_MBY_PARSER_EXC_4(y) composeLens lens_MBY_PARSER_EXC_5

  val lens_MBY_PARSER_EXT_1 = mby_ppe_parser_map._PARSER_EXT
  def lens_MBY_PARSER_EXT_2(x : Int) = listIndex[parser_ext_rf].index(x)
  val lens_MBY_PARSER_EXT_3 = parser_ext_rf._PARSER_EXT
  def lens_MBY_PARSER_EXT_4(y : Int) = listIndex[parser_ext_r].index(y)
  val lens_MBY_PARSER_EXT_5 = parser_ext_r._state
  def lens_MBY_PARSER_EXT(x : Int, y : Int) = lens_MBY_PARSER_EXT_1 composeOptional
    lens_MBY_PARSER_EXT_2(x) composeLens lens_MBY_PARSER_EXT_3 composeOptional lens_MBY_PARSER_EXT_4(y) composeLens lens_MBY_PARSER_EXT_5


  private def getBitVector(v: Any): BitVector = {
    val (v1, v2) = v.asInstanceOf[String].substring(2).splitAt(8)
    BitVector(Integer.parseUnsignedInt(v1, 16).toLong << 32 |
      Integer.parseUnsignedInt(v2, 16).toLong)
  }
  private def id(a: Any): Int = a.asInstanceOf[Int]

  @tailrec
  private def readParserVer1(lines: List[List[Any]], parser: mby_ppe_parser_map): mby_ppe_parser_map = lines match {
    case Nil => parser
    case line :: tail => line match {
      case "MBY_PARSER_PORT_CFG" :: _ => readParserVer1(tail, lens_MBY_PARSER_PORT_CFG(id(line(1))).set(getBitVector(line(2)))(parser))
      case "MBY_PARSER_KEY_W" :: _ => readParserVer1(tail, lens_MBY_PARSER_KEY_W(id(line(1)), id(line(2))).set(getBitVector(line(3)))(parser))
      case "MBY_PARSER_KEY_S" :: _ => readParserVer1(tail, lens_MBY_PARSER_KEY_S(id(line(1)), id(line(2))).set(getBitVector(line(3)))(parser))
      case "MBY_PARSER_ANA_W" :: _ => readParserVer1(tail, lens_MBY_PARSER_ANA_W(id(line(1)), id(line(2))).set(getBitVector(line(3)))(parser))
      case "MBY_PARSER_ANA_S" :: _ => readParserVer1(tail, lens_MBY_PARSER_ANA_S(id(line(1)), id(line(2))).set(getBitVector(line(3)))(parser))
      case "MBY_PARSER_EXC" :: _ => readParserVer1(tail, lens_MBY_PARSER_EXC(id(line(1)), id(line(2))).set(getBitVector(line(3)))(parser))
      case "MBY_PARSER_EXT" :: _ => readParserVer1(tail, lens_MBY_PARSER_EXT(id(line(1)), id(line(2))).set(getBitVector(line(3)))(parser))
      case _ => assert(false, s"unexpected line $line in readProgrammer"); parser
    }
  }

  def readVer1(fromJson: Map[String, Any], csr: Csr): mby_ppe_parser_map =
    readParserVer1(fromJson.getList[List[Any]]("input"), csr.getRxPpe(0).ppeRxMap.parser)

  @tailrec
  private def readParserCfgVer2(lines: List[Map[String, Any]], parser: mby_ppe_parser_map): mby_ppe_parser_map = lines match {
    case Nil => parser
    case cfg :: tail =>
      val lensCfg = ParserLenses(0).portCfg(cfg.getInt("id"))
      val lensInitialW0 = lensCfg composeLens parser_port_cfg_r._INITIAL_W0_OFFSET composeLens parser_port_cfg_r.INITIAL_W0_OFFSET._value
      val lensInitialW1 = lensCfg composeLens parser_port_cfg_r._INITIAL_W1_OFFSET composeLens parser_port_cfg_r.INITIAL_W1_OFFSET._value
      val lensInitialPtr = lensCfg composeLens parser_port_cfg_r._INITIAL_PTR composeLens parser_port_cfg_r.INITIAL_PTR._value
      val lensInitialState = lensCfg composeLens parser_port_cfg_r._INITIAL_STATE composeLens parser_port_cfg_r.INITIAL_STATE._value
      val lensInitialOpMask = lensCfg composeLens parser_port_cfg_r._INITIAL_OP_MASK composeLens parser_port_cfg_r.INITIAL_OP_MASK._value
      val lensInitialOpRot = lensCfg composeLens parser_port_cfg_r._INITIAL_OP_ROT composeLens parser_port_cfg_r.INITIAL_OP_ROT._value
      val updatedCsr = CsrLenses.execute(parser, for {
        _ <- lensInitialW0.assign_(cfg.getInt("INITIAL_W0_OFFSET"))
        _ <- lensInitialW1.assign_(cfg.getInt("INITIAL_W1_OFFSET"))
        _ <- lensInitialPtr.assign_(cfg.getInt("INITIAL_PTR"))
        _ <- lensInitialState.assign_(cfg.getInt("INITIAL_STATE"))
        _ <- lensInitialOpMask.assign_(cfg.getInt("INITIAL_OP_MASK"))
        _ <- lensInitialOpRot.assign_(cfg.getInt("INITIAL_OP_ROT"))
        } yield ())
      readParserCfgVer2(tail, updatedCsr)
  }

  @tailrec
  private def readParserStagesVer2(stages: List[Map[String, Any]], parser: mby_ppe_parser_map): mby_ppe_parser_map = stages match {
    case Nil => parser
    case cfg :: tail =>
      val idStage = cfg.getInt("stage")
      readParserStagesVer2(tail, readParserRulesVer2(idStage, cfg.getList[Map[String, Any]]("rules"), parser))
  }

  @tailrec
  private def readParserRulesVer2(idStage: Int, rules: List[Map[String, Any]], parser: mby_ppe_parser_map): mby_ppe_parser_map = rules match {
    case Nil => parser
    case cfg :: tail =>
      val pl = ParserLenses(idStage)
      val rule = cfg.getInt("rule")
      val keyW = cfg.getMap("PARSER_KEY_W")
      val lensKeyW1 = pl.keyW(rule) composeLens parser_key_w_r._W1_VALUE composeLens parser_key_w_r.W1_VALUE._value
      val lensKeyW0 = pl.keyW(rule) composeLens parser_key_w_r._W0_VALUE composeLens parser_key_w_r.W0_VALUE._value
      val lensKeyW0Mask = pl.keyW(rule) composeLens parser_key_w_r._W0_MASK composeLens parser_key_w_r.W0_MASK._value
      val lensKeyW1Mask = pl.keyW(rule) composeLens parser_key_w_r._W1_MASK composeLens parser_key_w_r.W1_MASK._value
      val lensKeySMask = pl.keyS(rule) composeLens parser_key_s_r._STATE_MASK composeLens parser_key_s_r.STATE_MASK._value
      val lensKeySValue = pl.keyS(rule) composeLens parser_key_s_r._STATE_VALUE composeLens parser_key_s_r.STATE_VALUE._value
      val lensAnaW0 = pl.anaW(rule) composeLens parser_ana_w_r._NEXT_W0_OFFSET composeLens parser_ana_w_r.NEXT_W0_OFFSET._value
      val lensAnaW1 = pl.anaW(rule) composeLens parser_ana_w_r._NEXT_W1_OFFSET composeLens parser_ana_w_r.NEXT_W1_OFFSET._value
      val lensAnaW2 = pl.anaW(rule) composeLens parser_ana_w_r._NEXT_W2_OFFSET composeLens parser_ana_w_r.NEXT_W2_OFFSET._value
      val lensAnaWSkip = pl.anaW(rule) composeLens parser_ana_w_r._SKIP composeLens parser_ana_w_r.SKIP._value
      val lensAnaSNextState = pl.anaS(rule) composeLens parser_ana_s_r._NEXT_STATE composeLens parser_ana_s_r.NEXT_STATE._value
      val lensAnaSNextStateMask = pl.anaS(rule) composeLens parser_ana_s_r._NEXT_STATE_MASK composeLens parser_ana_s_r.NEXT_STATE_MASK._value
      val lensAnaSNextOp = pl.anaS(rule) composeLens parser_ana_s_r._NEXT_OP composeLens parser_ana_s_r.NEXT_OP._value
      val lensExcOffset = pl.actExc(rule) composeLens parser_exc_r._EX_OFFSET composeLens parser_exc_r.EX_OFFSET._value
      val lensExcParsingDone = pl.actExc(rule) composeLens parser_exc_r._PARSING_DONE composeLens parser_exc_r.PARSING_DONE._value
      val lensExtProtocolId = pl.actExt(rule) composeLens parser_ext_r._PROTOCOL_ID composeLens parser_ext_r.PROTOCOL_ID._value
      val lensExtOffset = pl.actExt(rule) composeLens parser_ext_r._OFFSET composeLens parser_ext_r.OFFSET._value
      val lensExtFlagNum = pl.actExt(rule) composeLens parser_ext_r._FLAG_NUM composeLens parser_ext_r.FLAG_NUM._value
      val lensExtFlagValue = pl.actExt(rule) composeLens parser_ext_r._FLAG_VALUE composeLens parser_ext_r.FLAG_VALUE._value
      val lensExtPtrNum = pl.actExt(rule) composeLens parser_ext_r._PTR_NUM composeLens parser_ext_r.PTR_NUM._value
      val updatedCsr = CsrLenses.execute(parser, for {
        _ <- lensKeyW1.assign_(keyW.getInt("W1_VALUE"))
        _ <- lensKeyW0.assign_(keyW.getInt("W0_VALUE"))
        _ <- lensKeyW0Mask.assign_(keyW.getInt("W0_MASK"))
        _ <- lensKeyW1Mask.assign_(keyW.getInt("W1_MASK"))

        keyS = cfg.getMap("PARSER_KEY_S")
        _ <- lensKeySValue.assign_(keyS.getInt("STATE_VALUE"))
        _ <- lensKeySMask.assign_(keyS.getInt("STATE_MASK"))

        anaW = cfg.getMap("PARSER_ANA_W")
        _ <- lensAnaW0.assign_(anaW.getInt("NEXT_W0_OFFSET"))
        _ <- lensAnaW1.assign_(anaW.getInt("NEXT_W1_OFFSET"))
        _ <- lensAnaW2.assign_(anaW.getInt("NEXT_W2_OFFSET"))
        _ <- lensAnaWSkip.assign_(anaW.getInt("SKIP"))

        anaS = cfg.getMap("PARSER_ANA_S")
        _ <- lensAnaSNextState.assign_(anaS.getInt("NEXT_STATE"))
        _ <- lensAnaSNextOp.assign_(anaS.getInt("NEXT_OP"))
        _ <- lensAnaSNextStateMask.assign_(anaS.getInt("NEXT_STATE_MASK"))

        _ <- lensExcOffset.assign_(cfg.getInt("PARSER_EXC.EX_OFFSET"))
        _ <- lensExcParsingDone.assign_(cfg.getInt("PARSER_EXC.PARSING_DONE"))

        ext = cfg.getMap("PARSER_EXT")
        _ <- lensExtProtocolId.assign_(ext.getInt("PROTOCOL_ID"))
        _ <- lensExtOffset.assign_(ext.getInt("OFFSET"))
        _ <- lensExtFlagNum.assign_(ext.getInt("FLAG_NUM"))
        _ <- lensExtFlagValue.assign_(ext.getInt("FLAG_VALUE"))
        _ <- lensExtPtrNum.assign_(ext.getInt("PTR_NUM"))
      } yield ())
      readParserRulesVer2(idStage, tail, updatedCsr)
  }

  def readVer2(fromJson: Map[String, Any], csr: Csr): mby_ppe_parser_map = {
    val parserAfterCfgRead = readParserCfgVer2(
        fromJson.getList[Map[String,Any]]("input.PARSER_PORT_CFG"),
        csr.getRxPpe(0).ppeRxMap.parser
    )
    readParserStagesVer2(fromJson.getList[Map[String, Any]]("input.stages"), parserAfterCfgRead)
  }

}
