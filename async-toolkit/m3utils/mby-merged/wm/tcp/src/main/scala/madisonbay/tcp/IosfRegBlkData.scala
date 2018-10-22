package madisonbay.tcp

import shapeless.Witness

case class IosfRegBlkData(data: BitField[Witness.`32`.T])
