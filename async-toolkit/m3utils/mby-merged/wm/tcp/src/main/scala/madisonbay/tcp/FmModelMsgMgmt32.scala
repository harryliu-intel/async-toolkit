package madisonbay.tcp

import madisonbay.PrimitiveTypes._

case class FmModelMsgMgmt32(
  Type: FmModelMgmtType.Value,
  Address: U32,
  Value: U64
)

