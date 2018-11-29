package madisonbay.wm.switchwm.csr.lens

import monocle.Optional
import monocle.function.Index.index
import madisonbay.csr.all._

object ParserLenses {

  def portCfg(idPort: Int): Optional[mby_ppe_parser_map, parser_port_cfg_r] = mby_ppe_parser_map._PARSER_PORT_CFG composeOptional index(idPort)

  def extractCfg(extract_profile: Int, word: Int): Optional[mby_ppe_parser_map, parser_extract_cfg_r] = mby_ppe_parser_map._PARSER_EXTRACT_CFG composeOptional
    index(extract_profile) composeLens parser_extract_cfg_rf._PARSER_EXTRACT_CFG composeOptional index(word)

  def ptypeTcam(id: Int, extractionId: Int): Optional[mby_ppe_parser_map, parser_ptype_tcam_r] = mby_ppe_parser_map._PARSER_PTYPE_TCAM composeOptional
    index(id) composeLens parser_ptype_tcam_rf._PARSER_PTYPE_TCAM composeOptional index(extractionId)

  def ptypeRam(id: Int, extractionId: Int): Optional[mby_ppe_parser_map, parser_ptype_ram_r] = mby_ppe_parser_map._PARSER_PTYPE_RAM composeOptional
    index(id) composeLens parser_ptype_ram_rf._PARSER_PTYPE_RAM composeOptional index(extractionId)

}
