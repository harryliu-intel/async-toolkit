package madisonbay.tcp

import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._
import com.intel.cg.hpfd.csr.macros.SizedArray
import eu.timepit.refined.W

case class FmModelSideBandData(Idtag: U32, Tc: U8, Pktmeta: SizedArray[U8, W.`32`.T])

