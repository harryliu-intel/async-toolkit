package madisonbay.tcp

import shapeless.Witness

case class IosfRegBlkAddr(ndw: BitField[Witness.`4`.T], addr: BitField[Witness.`28`.T])
