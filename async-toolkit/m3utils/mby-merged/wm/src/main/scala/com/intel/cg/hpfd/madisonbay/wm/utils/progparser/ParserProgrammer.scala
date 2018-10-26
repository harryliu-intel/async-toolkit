package com.intel.cg.hpfd.madisonbay.wm.utils.progparser

import com.intel.cg.hpfd.csr.generated._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.{Csr, CsrLenses, ParserLenses}
import com.intel.cg.hpfd.madisonbay.wm.utils.Json.JsonMap
import monocle.function.Index.listIndex
import monocle.state.all._

import scala.annotation.tailrec

//scalastyle:off
object ParserProgrammer {

  val lens_MBY_PARSER_PORT_CFG_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_PORT_CFG
  def lens_MBY_PARSER_PORT_CFG_2(x : Int) = listIndex[parser_port_cfg_r.parser_port_cfg_r].index(x)
  val lens_MBY_PARSER_PORT_CFG_3 = parser_port_cfg_r.parser_port_cfg_r.state
  def lens_MBY_PARSER_PORT_CFG_state(x : Int) = lens_MBY_PARSER_PORT_CFG_1 composeOptional
    lens_MBY_PARSER_PORT_CFG_2(x) composeLens lens_MBY_PARSER_PORT_CFG_3

  val lens_MBY_PARSER_KEY_W_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_KEY_W
  def lens_MBY_PARSER_KEY_W_2(x : Int) = listIndex[parser_key_w_rf.parser_key_w_rf].index(x)
  val lens_MBY_PARSER_KEY_W_3 = parser_key_w_rf.parser_key_w_rf._PARSER_KEY_W
  def lens_MBY_PARSER_KEY_W_4(y : Int) = listIndex[parser_key_w_r.parser_key_w_r].index(y)
  val lens_MBY_PARSER_KEY_W_5 = parser_key_w_r.parser_key_w_r.state
  def lens_MBY_PARSER_KEY_W_state(x : Int, y : Int) = lens_MBY_PARSER_KEY_W_1 composeOptional
    lens_MBY_PARSER_KEY_W_2(x) composeLens lens_MBY_PARSER_KEY_W_3 composeOptional lens_MBY_PARSER_KEY_W_4(y) composeLens lens_MBY_PARSER_KEY_W_5

  val lens_MBY_PARSER_KEY_S_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_KEY_S
  def lens_MBY_PARSER_KEY_S_2(x : Int) = listIndex[parser_key_s_rf.parser_key_s_rf].index(x)
  val lens_MBY_PARSER_KEY_S_3 = parser_key_s_rf.parser_key_s_rf._PARSER_KEY_S
  def lens_MBY_PARSER_KEY_S_4(y : Int) = listIndex[parser_key_s_r.parser_key_s_r].index(y)
  val lens_MBY_PARSER_KEY_S_5 = parser_key_s_r.parser_key_s_r.state
  def lens_MBY_PARSER_KEY_S_state(x : Int, y : Int) = lens_MBY_PARSER_KEY_S_1 composeOptional
    lens_MBY_PARSER_KEY_S_2(x) composeLens lens_MBY_PARSER_KEY_S_3 composeOptional lens_MBY_PARSER_KEY_S_4(y) composeLens lens_MBY_PARSER_KEY_S_5

  val lens_MBY_PARSER_ANA_W_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_ANA_W
  def lens_MBY_PARSER_ANA_W_2(x : Int) = listIndex[parser_ana_w_rf.parser_ana_w_rf].index(x)
  val lens_MBY_PARSER_ANA_W_3 = parser_ana_w_rf.parser_ana_w_rf._PARSER_ANA_W
  def lens_MBY_PARSER_ANA_W_4(y : Int) = listIndex[parser_ana_w_r.parser_ana_w_r].index(y)
  val lens_MBY_PARSER_ANA_W_5 = parser_ana_w_r.parser_ana_w_r.state
  def lens_MBY_PARSER_ANA_W_state(x : Int, y : Int) = lens_MBY_PARSER_ANA_W_1 composeOptional
    lens_MBY_PARSER_ANA_W_2(x) composeLens lens_MBY_PARSER_ANA_W_3 composeOptional lens_MBY_PARSER_ANA_W_4(y) composeLens lens_MBY_PARSER_ANA_W_5

  val lens_MBY_PARSER_ANA_S_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_ANA_S
  def lens_MBY_PARSER_ANA_S_2(x : Int) = listIndex[parser_ana_s_rf.parser_ana_s_rf].index(x)
  val lens_MBY_PARSER_ANA_S_3 = parser_ana_s_rf.parser_ana_s_rf._PARSER_ANA_S
  def lens_MBY_PARSER_ANA_S_4(y : Int) = listIndex[parser_ana_s_r.parser_ana_s_r].index(y)
  val lens_MBY_PARSER_ANA_S_5 = parser_ana_s_r.parser_ana_s_r.state
  def lens_MBY_PARSER_ANA_S_state(x : Int, y : Int) = lens_MBY_PARSER_ANA_S_1 composeOptional
    lens_MBY_PARSER_ANA_S_2(x) composeLens lens_MBY_PARSER_ANA_S_3 composeOptional lens_MBY_PARSER_ANA_S_4(y) composeLens lens_MBY_PARSER_ANA_S_5

  val lens_MBY_PARSER_EXC_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_EXC
  def lens_MBY_PARSER_EXC_2(x : Int) = listIndex[parser_exc_rf.parser_exc_rf].index(x)
  val lens_MBY_PARSER_EXC_3 = parser_exc_rf.parser_exc_rf._PARSER_EXC
  def lens_MBY_PARSER_EXC_4(y : Int) = listIndex[parser_exc_r.parser_exc_r].index(y)
  val lens_MBY_PARSER_EXC_5 = parser_exc_r.parser_exc_r.state
  def lens_MBY_PARSER_EXC_state(x : Int, y : Int) = lens_MBY_PARSER_EXC_1 composeOptional
    lens_MBY_PARSER_EXC_2(x) composeLens lens_MBY_PARSER_EXC_3 composeOptional lens_MBY_PARSER_EXC_4(y) composeLens lens_MBY_PARSER_EXC_5

  val lens_MBY_PARSER_EXT_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_EXT
  def lens_MBY_PARSER_EXT_2(x : Int) = listIndex[parser_ext_rf.parser_ext_rf].index(x)
  val lens_MBY_PARSER_EXT_3 = parser_ext_rf.parser_ext_rf._PARSER_EXT
  def lens_MBY_PARSER_EXT_4(y : Int) = listIndex[parser_ext_r.parser_ext_r].index(y)
  val lens_MBY_PARSER_EXT_5 = parser_ext_r.parser_ext_r.state
  def lens_MBY_PARSER_EXT_state(x : Int, y : Int) = lens_MBY_PARSER_EXT_1 composeOptional
    lens_MBY_PARSER_EXT_2(x) composeLens lens_MBY_PARSER_EXT_3 composeOptional lens_MBY_PARSER_EXT_4(y) composeLens lens_MBY_PARSER_EXT_5


  private def getLong(v: Any): Long = {
    val (v1, v2) = v.asInstanceOf[String].substring(2).splitAt(8)
    Integer.parseUnsignedInt(v1, 16).toLong << 32 |
      Integer.parseUnsignedInt(v2, 16)
  }
  private def id(a: Any): Int = a.asInstanceOf[Int]

  @tailrec
  private def readParserVer1(lines: List[List[Any]], parser: mby_ppe_parser_map.mby_ppe_parser_map): mby_ppe_parser_map.mby_ppe_parser_map = lines match {
    case Nil => parser
    case line :: tail => line match {
      case "MBY_PARSER_PORT_CFG"  :: _ => readParserVer1(tail, lens_MBY_PARSER_PORT_CFG_state(id(line(1))).set(getLong(line(2)))(parser))
      case "MBY_PARSER_KEY_W"     :: _ => readParserVer1(tail, lens_MBY_PARSER_KEY_W_state(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case "MBY_PARSER_KEY_S"     :: _ => readParserVer1(tail, lens_MBY_PARSER_KEY_S_state(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case "MBY_PARSER_ANA_W"     :: _ => readParserVer1(tail, lens_MBY_PARSER_ANA_W_state(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case "MBY_PARSER_ANA_S"     :: _ => readParserVer1(tail, lens_MBY_PARSER_ANA_S_state(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case "MBY_PARSER_EXC"       :: _ => readParserVer1(tail, lens_MBY_PARSER_EXC_state(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case "MBY_PARSER_EXT"       :: _ => readParserVer1(tail, lens_MBY_PARSER_EXT_state(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case _ => assert(false, s"unexpected line $line in readProgrammer"); parser
    }
  }

  def readVer1(fromJson: Map[String, Any], csr: Csr): mby_ppe_parser_map.mby_ppe_parser_map =
    readParserVer1(fromJson.getList[List[Any]]("input"), csr.getRxPpe(0).csrRxPpe.parser)

  @tailrec
  private def readParserCfgVer2(lines: List[Map[String, Any]], parser: mby_ppe_parser_map.mby_ppe_parser_map): mby_ppe_parser_map.mby_ppe_parser_map = lines match {
    case Nil => parser
    case cfg :: tail =>
      val lensCfg = ParserLenses(0).portCfg(cfg.getInt("id"))
      val updatedCsr = CsrLenses.execute(parser, for {
        _ <- lensCfg.mod_(_.INITIAL_W0_OFFSET.set(cfg.getInt("INITIAL_W0_OFFSET")))
        _ <- lensCfg.mod_(_.INITIAL_W1_OFFSET.set(cfg.getInt("INITIAL_W1_OFFSET")))
        _ <- lensCfg.mod_(_.INITIAL_W2_OFFSET.set(cfg.getInt("INITIAL_W2_OFFSET")))
        _ <- lensCfg.mod_(_.INITIAL_PTR.set(cfg.getInt("INITIAL_PTR")))
        _ <- lensCfg.mod_(_.INITIAL_STATE.set(cfg.getInt("INITIAL_STATE")))
        _ <- lensCfg.mod_(_.INITIAL_OP_MASK.set(cfg.getInt("INITIAL_OP_MASK")))
        _ <- lensCfg.mod_(_.INITIAL_OP_ROT.set(cfg.getInt("INITIAL_OP_ROT")))
        } yield ())
      readParserCfgVer2(tail, updatedCsr)
  }

  @tailrec
  private def readParserStagesVer2(stages: List[Map[String, Any]], parser: mby_ppe_parser_map.mby_ppe_parser_map): mby_ppe_parser_map.mby_ppe_parser_map = stages match {
    case Nil => parser
    case cfg :: tail =>
      val idStage = cfg.getInt("stage")
      readParserStagesVer2(tail, readParserRulesVer2(idStage, cfg.getList[Map[String, Any]]("rules"), parser))
  }

  @tailrec
  private def readParserRulesVer2(idStage: Int, rules: List[Map[String, Any]], parser: mby_ppe_parser_map.mby_ppe_parser_map): mby_ppe_parser_map.mby_ppe_parser_map = rules match {
    case Nil => parser
    case cfg :: tail =>
      val pl = ParserLenses(idStage)
      val rule = cfg.getInt("rule")
      val keyW = cfg.getMap("PARSER_KEY_W")
      val updatedCsr = CsrLenses.execute(parser, for {
        _ <- pl.keyW(rule).mod_(_.W1_VALUE.set(keyW.getInt("W1_VALUE")))
        _ <- pl.keyW(rule).mod_(_.W1_MASK.set(keyW.getInt("W1_MASK")))
        _ <- pl.keyW(rule).mod_(_.W0_VALUE.set(keyW.getInt("W0_VALUE")))
        _ <- pl.keyW(rule).mod_(_.W0_MASK.set(keyW.getInt("W0_MASK")))

        keyS = cfg.getMap("PARSER_KEY_S")
        _ <- pl.keyS(rule).mod_(_.STATE_VALUE.set(keyS.getInt("STATE_VALUE")))
        _ <- pl.keyS(rule).mod_(_.STATE_MASK.set(keyS.getInt("STATE_MASK")))

        anaW = cfg.getMap("PARSER_ANA_W")
        _ <- pl.anaW(rule).mod_(_.NEXT_W0_OFFSET.set(anaW.getInt("NEXT_W0_OFFSET")))
        _ <- pl.anaW(rule).mod_(_.NEXT_W1_OFFSET.set(anaW.getInt("NEXT_W1_OFFSET")))
        _ <- pl.anaW(rule).mod_(_.NEXT_W2_OFFSET.set(anaW.getInt("NEXT_W2_OFFSET")))
        _ <- pl.anaW(rule).mod_(_.SKIP.set(anaW.getInt("SKIP")))

        anaS = cfg.getMap("PARSER_ANA_S")
        _ <- pl.anaS(rule).mod_(_.NEXT_STATE.set(anaS.getInt("NEXT_STATE")))
        _ <- pl.anaS(rule).mod_(_.NEXT_STATE.set(anaS.getInt("NEXT_STATE")))
        _ <- pl.anaS(rule).mod_(_.NEXT_STATE_MASK.set(anaS.getInt("NEXT_STATE_MASK")))

        _ <- pl.actExc(rule).mod_(_.EX_OFFSET.set(cfg.getInt("PARSER_EXC.EX_OFFSET")))
        _ <- pl.actExc(rule).mod_(_.PARSING_DONE.set(cfg.getInt("PARSER_EXC.PARSING_DONE")))

        ext = cfg.getMap("PARSER_EXT")
        _ <- pl.actExt(rule).mod_(_.PROTOCOL_ID.set(ext.getInt("PROTOCOL_ID")))
        _ <- pl.actExt(rule).mod_(_.OFFSET.set(ext.getInt("OFFSET")))
        _ <- pl.actExt(rule).mod_(_.FLAG_NUM.set(ext.getInt("FLAG_NUM")))
        _ <- pl.actExt(rule).mod_(_.FLAG_VALUE.set(ext.getInt("FLAG_VALUE")))
        _ <- pl.actExt(rule).mod_(_.PTR_NUM.set(ext.getInt("PTR_NUM")))
      } yield ())
      readParserRulesVer2(idStage, tail, updatedCsr)
  }

  def readVer2(fromJson: Map[String, Any], csr: Csr): mby_ppe_parser_map.mby_ppe_parser_map = {
    val parserAfterCfgRead = readParserCfgVer2(
        fromJson.getList[Map[String,Any]]("input.PARSER_PORT_CFG"),
        csr.getRxPpe(0).csrRxPpe.parser
    )
    readParserStagesVer2(fromJson.getList[Map[String, Any]]("input.stages"), parserAfterCfgRead)
  }

}
