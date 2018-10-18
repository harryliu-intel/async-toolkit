package madisonbay.tcp

import com.intel.cg.hpfd.csr.macros.SizedArray
import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._
import eu.timepit.refined.W

case class FmModelMsgGetInfo(
  Type: FmModelInfoType.Value,
  Nportssupported: U16,
  Padding: SizedArray[U8, W.`51`.T]
)

