package com.intel.cg.hpfd.csr.testData

import com.intel.cg.hpfd.madisonbay.Memory._

//scalastyle:off magic.number
trait AtAddressingExample {
  import at._
  import common._

  lazy val atAddressingExpectedValue =
    AddressMap(
      AddressRange(Address at 0.bytes, Address at 0x80.bytes),
      List(
        RegisterFileA(
          AddressRange(pos = Address at 0.bytes, lim = Address at 0x30.bytes),
          Register(Address at 0x18.bytes,-559038737),
          List(
            Register(Address at 0x20.bytes,-559038737),
            Register(Address at 0x28.bytes,-559038737)
          )
        ),
        RegisterFileA(
          AddressRange(pos = Address at 0x30.bytes, lim = Address at 0x60.bytes),
          Register(Address at 0x48.bytes,-559038737),
          List(
            Register(Address at 0x50.bytes,-559038737),
            Register(Address at 0x58.bytes,-559038737)
          )
        )
      ),
      RegisterFileB(
        AddressRange(pos = Address at 0x68.bytes, lim = Address at 0x80.bytes),
        List(
          Register(Address at 0x68.bytes,-559038737),
          Register(Address at 0x70.bytes,-559038737)
        ),
        Register(Address at 0x78.bytes,-559038737)
      )
    )
}
