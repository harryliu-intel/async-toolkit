package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper

/*import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrMapper
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ParserOutput*/

object Mapper {

  case class MacLookupResult(index: Int, routeable: Boolean)
  case class MacMapperResult(outerDMAC: MacLookupResult,
                             outerSMAC: MacLookupResult,
                             innerDMAC: MacLookupResult,
                             innerSMAC: MacLookupResult)


  /*def applyMapping(csr: CsrMapper, parserOutput: ParserOutput): Unit = {

  }*/

}
