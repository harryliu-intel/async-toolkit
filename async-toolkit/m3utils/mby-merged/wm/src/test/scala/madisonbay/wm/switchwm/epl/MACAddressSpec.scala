//scalastyle:off
package madisonbay.wm.switchwm.epl

import org.scalatest._


/**
  * Demonstrate some operations MAC Addresses.
  */
  class MACAddressSpec extends FlatSpec with Matchers {

    "Tostring of all zeros " should " be 00:00:00:00:00:00 " in {
      val mac = MACAddress(0)
      mac.toString shouldEqual "00:00:00:00:00:00"
    }

    "Mask-off 8 bits of all one's MAC " should " be FF:FF:FF:FF:FF:00 " in {
      val mac = MACAddress(-1)
      mac.maskLsb(8).toString shouldEqual "FF:FF:FF:FF:FF:00"
    }

  }
