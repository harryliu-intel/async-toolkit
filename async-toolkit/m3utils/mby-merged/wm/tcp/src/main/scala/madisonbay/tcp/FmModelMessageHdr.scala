package madisonbay.tcp

import madisonbay.PrimitiveTypes._

case class FmModelMessageHdr(
  Msglength: U32,
  Version: U16,
  Type: FmModelMsgType.Value,
  Sw: U16,
  Port: U16
)
