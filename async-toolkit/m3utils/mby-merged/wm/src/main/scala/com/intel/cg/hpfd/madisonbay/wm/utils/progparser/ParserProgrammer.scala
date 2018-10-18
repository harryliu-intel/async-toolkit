package com.intel.cg.hpfd.madisonbay.wm.utils.progparser

import com.intel.cg.hpfd.csr.generated._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr
import monocle.function.Index.listIndex

import scala.annotation.tailrec

//scalastyle:off
object ParserProgrammer {

  val lens_MBY_PARSER_PORT_CFG_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_PORT_CFG
  def lens_MBY_PARSER_PORT_CFG_2(x : Int) = listIndex[parser_port_cfg_r.parser_port_cfg_r].index(x)
  val lens_MBY_PARSER_PORT_CFG_3 = parser_port_cfg_r.parser_port_cfg_r.state
  def lens_MBY_PARSER_PORT_CFG(x : Int) = lens_MBY_PARSER_PORT_CFG_1 composeOptional
    lens_MBY_PARSER_PORT_CFG_2(x) composeLens lens_MBY_PARSER_PORT_CFG_3

  val lens_MBY_PARSER_KEY_W_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_KEY_W
  def lens_MBY_PARSER_KEY_W_2(x : Int) = listIndex[parser_key_w_rf.parser_key_w_rf].index(x)
  val lens_MBY_PARSER_KEY_W_3 = parser_key_w_rf.parser_key_w_rf._PARSER_KEY_W
  def lens_MBY_PARSER_KEY_W_4(y : Int) = listIndex[parser_key_w_r.parser_key_w_r].index(y)
  val lens_MBY_PARSER_KEY_W_5 = parser_key_w_r.parser_key_w_r.state
  def lens_MBY_PARSER_KEY_W(x : Int, y : Int) = lens_MBY_PARSER_KEY_W_1 composeOptional
    lens_MBY_PARSER_KEY_W_2(x) composeLens lens_MBY_PARSER_KEY_W_3 composeOptional lens_MBY_PARSER_KEY_W_4(y) composeLens lens_MBY_PARSER_KEY_W_5

  val lens_MBY_PARSER_KEY_S_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_KEY_S
  def lens_MBY_PARSER_KEY_S_2(x : Int) = listIndex[parser_key_s_rf.parser_key_s_rf].index(x)
  val lens_MBY_PARSER_KEY_S_3 = parser_key_s_rf.parser_key_s_rf._PARSER_KEY_S
  def lens_MBY_PARSER_KEY_S_4(y : Int) = listIndex[parser_key_s_r.parser_key_s_r].index(y)
  val lens_MBY_PARSER_KEY_S_5 = parser_key_s_r.parser_key_s_r.state
  def lens_MBY_PARSER_KEY_S(x : Int, y : Int) = lens_MBY_PARSER_KEY_S_1 composeOptional
    lens_MBY_PARSER_KEY_S_2(x) composeLens lens_MBY_PARSER_KEY_S_3 composeOptional lens_MBY_PARSER_KEY_S_4(y) composeLens lens_MBY_PARSER_KEY_S_5

  val lens_MBY_PARSER_ANA_W_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_ANA_W
  def lens_MBY_PARSER_ANA_W_2(x : Int) = listIndex[parser_ana_w_rf.parser_ana_w_rf].index(x)
  val lens_MBY_PARSER_ANA_W_3 = parser_ana_w_rf.parser_ana_w_rf._PARSER_ANA_W
  def lens_MBY_PARSER_ANA_W_4(y : Int) = listIndex[parser_ana_w_r.parser_ana_w_r].index(y)
  val lens_MBY_PARSER_ANA_W_5 = parser_ana_w_r.parser_ana_w_r.state
  def lens_MBY_PARSER_ANA_W(x : Int, y : Int) = lens_MBY_PARSER_ANA_W_1 composeOptional
    lens_MBY_PARSER_ANA_W_2(x) composeLens lens_MBY_PARSER_ANA_W_3 composeOptional lens_MBY_PARSER_ANA_W_4(y) composeLens lens_MBY_PARSER_ANA_W_5

  val lens_MBY_PARSER_ANA_S_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_ANA_S
  def lens_MBY_PARSER_ANA_S_2(x : Int) = listIndex[parser_ana_s_rf.parser_ana_s_rf].index(x)
  val lens_MBY_PARSER_ANA_S_3 = parser_ana_s_rf.parser_ana_s_rf._PARSER_ANA_S
  def lens_MBY_PARSER_ANA_S_4(y : Int) = listIndex[parser_ana_s_r.parser_ana_s_r].index(y)
  val lens_MBY_PARSER_ANA_S_5 = parser_ana_s_r.parser_ana_s_r.state
  def lens_MBY_PARSER_ANA_S(x : Int, y : Int) = lens_MBY_PARSER_ANA_S_1 composeOptional
    lens_MBY_PARSER_ANA_S_2(x) composeLens lens_MBY_PARSER_ANA_S_3 composeOptional lens_MBY_PARSER_ANA_S_4(y) composeLens lens_MBY_PARSER_ANA_S_5

  val lens_MBY_PARSER_EXC_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_EXC
  def lens_MBY_PARSER_EXC_2(x : Int) = listIndex[parser_exc_rf.parser_exc_rf].index(x)
  val lens_MBY_PARSER_EXC_3 = parser_exc_rf.parser_exc_rf._PARSER_EXC
  def lens_MBY_PARSER_EXC_4(y : Int) = listIndex[parser_exc_r.parser_exc_r].index(y)
  val lens_MBY_PARSER_EXC_5 = parser_exc_r.parser_exc_r.state
  def lens_MBY_PARSER_EXC(x : Int, y : Int) = lens_MBY_PARSER_EXC_1 composeOptional
    lens_MBY_PARSER_EXC_2(x) composeLens lens_MBY_PARSER_EXC_3 composeOptional lens_MBY_PARSER_EXC_4(y) composeLens lens_MBY_PARSER_EXC_5

  val lens_MBY_PARSER_EXT_1 = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_EXT
  def lens_MBY_PARSER_EXT_2(x : Int) = listIndex[parser_ext_rf.parser_ext_rf].index(x)
  val lens_MBY_PARSER_EXT_3 = parser_ext_rf.parser_ext_rf._PARSER_EXT
  def lens_MBY_PARSER_EXT_4(y : Int) = listIndex[parser_ext_r.parser_ext_r].index(y)
  val lens_MBY_PARSER_EXT_5 = parser_ext_r.parser_ext_r.state
  def lens_MBY_PARSER_EXT(x : Int, y : Int) = lens_MBY_PARSER_EXT_1 composeOptional
    lens_MBY_PARSER_EXT_2(x) composeLens lens_MBY_PARSER_EXT_3 composeOptional lens_MBY_PARSER_EXT_4(y) composeLens lens_MBY_PARSER_EXT_5


  private def getLong(v: Any): Long = {
    val (v1, v2) = v.asInstanceOf[String].substring(2).splitAt(8)
    Integer.parseUnsignedInt(v1, 16).toLong << 32 |
      Integer.parseUnsignedInt(v2, 16)
  }
  private def id(a: Any): Int = a.asInstanceOf[Int]

  @tailrec
  private def readParserProgram(lines: List[List[Any]], parser: mby_ppe_parser_map.mby_ppe_parser_map): mby_ppe_parser_map.mby_ppe_parser_map = lines match {
    case Nil => parser
    case line :: tail => line match {
      case "MBY_PARSER_PORT_CFG" :: _ => readParserProgram(tail, lens_MBY_PARSER_PORT_CFG(id(line(1))).set(getLong(line(2)))(parser))
      case "MBY_PARSER_KEY_W" :: _ => readParserProgram(tail, lens_MBY_PARSER_KEY_W(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case "MBY_PARSER_KEY_S" :: _ => readParserProgram(tail, lens_MBY_PARSER_KEY_S(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case "MBY_PARSER_ANA_W" :: _ => readParserProgram(tail, lens_MBY_PARSER_ANA_W(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case "MBY_PARSER_ANA_S" :: _ => readParserProgram(tail, lens_MBY_PARSER_ANA_S(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case "MBY_PARSER_EXC" :: _ => readParserProgram(tail, lens_MBY_PARSER_EXC(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case "MBY_PARSER_EXT" :: _ => readParserProgram(tail, lens_MBY_PARSER_EXT(id(line(1)), id(line(2))).set(getLong(line(3)))(parser))
      case _ => assert(false, s"unexpected line $line in readProgrammer"); parser
    }
  }

  def apply(fromJson: Map[String, Any], csr: Csr): mby_ppe_parser_map.mby_ppe_parser_map = {
    readParserProgram(fromJson("input").asInstanceOf[List[List[Any]]], csr.getRxPpe(0).csrRxPpe.parser)
  }

}
