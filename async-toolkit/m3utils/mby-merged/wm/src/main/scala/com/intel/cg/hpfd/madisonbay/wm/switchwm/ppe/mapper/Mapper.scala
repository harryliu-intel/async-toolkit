package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrMapper
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.output.MapperOutput
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ParserOutput

object Mapper
{
  def runMapper(csrMapper: CsrMapper, parserOutput: ParserOutput): MapperOutput = {
    val _ = (csrMapper, parserOutput)
    MapperOutput(0)
  }
}
