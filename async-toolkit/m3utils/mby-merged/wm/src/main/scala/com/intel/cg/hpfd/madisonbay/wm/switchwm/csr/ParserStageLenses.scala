package com.intel.cg.hpfd.madisonbay.wm.switchwm.csr

import madisonbay.csr.all._
import monocle.Optional
import monocle.function.Index.index

object ParserStageLenses {

  def apply(idStage: Int): ParserStageLenses = new ParserStageLenses(idStage)

}

class ParserStageLenses(idStage: Int) {

  // key W[0], W[1]
  def keyW(idRule: Int): Optional[mby_ppe_parser_map, parser_key_w_r] = _keyW composeLens parser_key_w_rf._PARSER_KEY_W composeOptional index(idRule)

  // key State
  def keyS(idRule: Int): Optional[mby_ppe_parser_map, parser_key_s_r] = _keyS composeLens parser_key_s_rf._PARSER_KEY_S composeOptional index(idRule)

  def anaW(idRule: Int): Optional[mby_ppe_parser_map, parser_ana_w_r] = _anaW composeLens parser_ana_w_rf._PARSER_ANA_W composeOptional index(idRule)

  def anaS(idRule: Int): Optional[mby_ppe_parser_map, parser_ana_s_r] = _anaS composeLens parser_ana_s_rf._PARSER_ANA_S composeOptional index(idRule)

  // extract action
  def actExt(idRule: Int): Optional[mby_ppe_parser_map, parser_ext_r] = _actExt composeLens parser_ext_rf._PARSER_EXT composeOptional index(idRule)

  def actExc(idRule: Int): Optional[mby_ppe_parser_map, parser_exc_r] = _actExc composeLens parser_exc_rf._PARSER_EXC composeOptional index(idRule)

  private val _keyW: Optional[mby_ppe_parser_map, parser_key_w_rf] = mby_ppe_parser_map._PARSER_KEY_W composeOptional index(idStage)

  private val _keyS: Optional[mby_ppe_parser_map, parser_key_s_rf] = mby_ppe_parser_map._PARSER_KEY_S composeOptional index(idStage)

  private val _anaW: Optional[mby_ppe_parser_map, parser_ana_w_rf] = mby_ppe_parser_map._PARSER_ANA_W composeOptional index(idStage)

  private val _anaS: Optional[mby_ppe_parser_map, parser_ana_s_rf] = mby_ppe_parser_map._PARSER_ANA_S composeOptional index(idStage)

  private val _actExt: Optional[mby_ppe_parser_map, parser_ext_rf] = mby_ppe_parser_map._PARSER_EXT composeOptional index(idStage)

  private val _actExc: Optional[mby_ppe_parser_map, parser_exc_rf] = mby_ppe_parser_map._PARSER_EXC composeOptional index(idStage)

}
