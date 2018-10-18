package madisonbay.tcp

import shapeless.Witness

//scalastyle:off magic.number
case class IosfRegCompNoData(
  dest: BitField[Witness.`8`.T],
  source: BitField[Witness.`8`.T],
  opcode: BitField[Witness.`8`.T] = BitField(0x20),
  tag: BitField[Witness.`3`.T],
  rsp: BitField[Witness.`2`.T],
  rsvd0: BitField[Witness.`2`.T],
  eh0: BitField[Witness.`1`.T] = BitField(0x1),
  exphdr: BitField[Witness.`7`.T] = BitField(0x0),
  eh1: BitField[Witness.`1`.T] = BitField(0x0),
  sai: BitField[Witness.`16`.T],
  rs: BitField[Witness.`8`.T] = BitField(0x0)
)
