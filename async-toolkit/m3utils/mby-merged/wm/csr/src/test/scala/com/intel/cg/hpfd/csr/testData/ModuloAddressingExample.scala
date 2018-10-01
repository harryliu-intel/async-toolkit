package com.intel.cg.hpfd.csr.testData

import com.intel.cg.hpfd.madisonbay.Memory._

//scalastyle:off magic.number
trait ModuloAddressingExample {

  lazy val moduloAddressingExpectedValue =
    modulo.AddressMap(
      AddressRange(Address at 0.bytes, Address at 0x288.bytes),
      List(
        modulo.RegisterFileA(
          AddressRange(pos = Address at 0x200.bytes, lim = Address at 0x210.bytes),
          common.Register(Address at 0x200.bytes,-559038737),
          List(
            common.Register(Address at 0x208.bytes,-559038737)
          )
        )
      ),
      // alligned to 68 words
      modulo.RegisterFileB(
        AddressRange(pos = Address at 0x220.bytes, lim = Address(word = 81, bits = 0 bits)),
        List(
          common.Register(Address at 0x220.bytes,-559038737),
          common.Register(Address at 0x228.bytes,-559038737),
          common.Register(Address at 0x230.bytes,-559038737),
          common.Register(Address at 0x238.bytes,-559038737),
          common.Register(Address at 0x240.bytes,-559038737)
        ),
        common.Register(Address at 0x280.bytes,-559038737)
      )
    )
}

