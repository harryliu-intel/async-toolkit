package madisonbay.wm.utils.extensions

object ExtByte {

  implicit class Implicits(byte: Byte) {

    def hex: String = {
      f"0x$byte%02X"
    }

  }

}
