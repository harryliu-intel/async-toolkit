package com.intel.cg.hpfd.csr.testData

import com.intel.cg.hpfd.madisonbay.Memory._

//scalastyle:off magic.number
trait ModuloAddressingExample {
  import common._
  import modulo._

  lazy val moduloAddressingExpectedValue =
    AddressMap(
      AddressRange(Address at 0.bytes, Address at 0x288.bytes),
      List(
        RegisterFileA(
          AddressRange(pos = Address at 0x200.bytes, lim = Address at 0x210.bytes),
          Register(Address at 0x200.bytes,-559038737),
          List(
            Register(Address at 0x208.bytes,-559038737)
          )
        )
      ),
      // alligned to 68 words
      RegisterFileB(
        AddressRange(pos = Address at 0x220.bytes, lim = Address(word = 81, bits = 0 bits)),
        List(
          Register(Address at 0x220.bytes,-559038737),
          Register(Address at 0x228.bytes,-559038737),
          Register(Address at 0x230.bytes,-559038737),
          Register(Address at 0x238.bytes,-559038737),
          Register(Address at 0x240.bytes,-559038737)
        ),
        Register(Address at 0x280.bytes,-559038737)
      )
    )
}

