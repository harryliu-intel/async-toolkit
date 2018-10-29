package madisonbay.tcp

import com.intel.cg.hpfd.csr.macros.SizedArray
import eu.timepit.refined.W
import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._

case class FmModelMsgAttr(
  Type: FmModelAttrType.Value,
  Keylength: U16,
  Key: SizedArray[U8,W.`256`.T],
  Value: SizedArray[U8, W.`256`.T]
)

