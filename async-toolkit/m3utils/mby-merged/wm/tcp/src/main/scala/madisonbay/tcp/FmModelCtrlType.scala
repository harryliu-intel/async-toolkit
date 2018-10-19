package madisonbay.tcp

// TODO: not used!
object FmModelCtrlType extends Enumeration {
  val Length = 1;

  val ChipResetReq = Value(1, "ChipResetReq")
  val ChipResetRep = Value(2, "ChipResetRep")

  implicit val encoder: ByteArrayEncoder[FmModelCtrlType.Value] =
    value => ByteArrayEncoder.u8bae.encode(value.id.byteValue())
}

