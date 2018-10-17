package com.intel.cg.hpfd.madisonbay.wm.switchwm.csr

import com.intel.cg.hpfd.csr.generated._
import monocle.Optional
import monocle.function.Index.index

object ParserLenses {

  def apply(idStage: Int): ParserLenses = new ParserLenses(idStage)

}

class ParserLenses(idStage: Int) {

  // key W[0], W[1]
  def keyW(idRule: Int): Optional[mby_ppe_parser_map.mby_ppe_parser_map, parser_key_w_r.parser_key_w_r] =
      _keyW composeLens
        parser_key_w_rf.parser_key_w_rf._PARSER_KEY_W composeOptional
        index(idRule)

  // key State
  def keyS(idRule: Int): Optional[mby_ppe_parser_map.mby_ppe_parser_map, parser_key_s_r.parser_key_s_r] =
      _keyS composeLens
        parser_key_s_rf.parser_key_s_rf._PARSER_KEY_S composeOptional
        index(idRule)

  // extract action
  def actExt(idRule: Int): Optional[mby_ppe_parser_map.mby_ppe_parser_map, parser_ext_r.parser_ext_r] =
      _actExt composeLens
        parser_ext_rf.parser_ext_rf._PARSER_EXT composeOptional
        index(idRule)

  private val _keyW: Optional[mby_ppe_parser_map.mby_ppe_parser_map, parser_key_w_rf.parser_key_w_rf] =
      mby_ppe_parser_map.mby_ppe_parser_map._PARSER_KEY_W composeOptional index(idStage)

  private val _keyS: Optional[mby_ppe_parser_map.mby_ppe_parser_map, parser_key_s_rf.parser_key_s_rf] =
      mby_ppe_parser_map.mby_ppe_parser_map._PARSER_KEY_S composeOptional index(idStage)

  private val _actExt: Optional[mby_ppe_parser_map.mby_ppe_parser_map, parser_ext_rf.parser_ext_rf] =
      mby_ppe_parser_map.mby_ppe_parser_map._PARSER_EXT composeOptional index(idStage)

}
