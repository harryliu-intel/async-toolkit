package madisonbay.wm.switchwm.epl

import madisonbay.wm.utils.extensions.UIntegers

import scala.util.Try

trait NetworkPacket {

  val bytes: IndexedSeq[Byte]

  def getWord(addr: Int): Short =
    if (addr + 1 >= bytes.length) {
      throw new IndexOutOfBoundsException(s"Address $addr in packet size[${bytes.length}] ${bytes.toList}")
    } else {
      // Packet data is big endian (network order)
      (((bytes(addr).toInt << 8) & UIntegers.MaskUpper8From16) |
        (bytes(addr + 1).toInt & UIntegers.MaskLower8)).toShort
    }

  def getWordSafe(addr: Int): Short = Try(getWord(addr)).getOrElse(0.toShort)

  def length: Int = bytes.length

}
