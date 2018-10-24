package madisonbay.tcp

import shapeless.Witness

//scalastyle:off magic.number
case class IosfRegWriteReq(
  dest: BitField[Witness.`8`.T],
  source: BitField[Witness.`8`.T],
  opcode: BitField[Witness.`8`.T] = BitField(0x1),
  tag: BitField[Witness.`3`.T],
  bar: BitField[Witness.`3`.T],
  al: BitField[Witness.`1`.T] = BitField(0x1),
  eh0: BitField[Witness.`1`.T],
  exphdr: BitField[Witness.`7`.T] = BitField(0x0),
  eh1: BitField[Witness.`1`.T] = BitField(0x0),
  sai: BitField[Witness.`16`.T],
  rs: BitField[Witness.`3`.T] = BitField(0x0),
  rsvd0: BitField[Witness.`5`.T],
  fbe: BitField[Witness.`4`.T] = BitField(0xf),
  sbe: BitField[Witness.`4`.T],
  fid: BitField[Witness.`8`.T] = BitField(0x0),
  addr0: BitField[Witness.`16`.T],
  addr1: BitField[Witness.`12`.T],
  addr2: BitField[Witness.`20`.T] = BitField(0x0),
  data0: BitField[Witness.`32`.T],
  data1: BitField[Witness.`32`.T]
)
