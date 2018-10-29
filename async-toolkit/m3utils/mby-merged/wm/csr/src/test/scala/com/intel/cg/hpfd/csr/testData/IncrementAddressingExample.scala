package com.intel.cg.hpfd.csr.testData

import com.intel.cg.hpfd.madisonbay.Memory._

//scalastyle:off magic.number
trait IncrementAddressingExample {
  import increment._
  import common._

  lazy val incrementAddressingExpectedValue =
    AddressMap(
      AddressRange(Address at 0.bytes, Address at 0x5A0.bytes),
      List(
        RegisterFileA(
          AddressRange(pos = Address at 0.bytes, lim = Address at 0x50.bytes),
          Register(Address at 0.bytes,-559038737),
          List(
            Register(Address at 0x8.bytes,-559038737),
            Register(Address at 0x18.bytes,-559038737),
            Register(Address at 0x28.bytes,-559038737),
            Register(Address at 0x38.bytes,-559038737),
            Register(Address at 0x48.bytes,-559038737)
          )
        ),
        RegisterFileA(
          AddressRange(pos = Address at 0x280.bytes, lim = Address at 0x2D0.bytes),
          Register(Address at 0x280.bytes,-559038737),
          List(
            Register(Address at 0x288.bytes,-559038737),
            Register(Address at 0x298.bytes,-559038737),
            Register(Address at 0x2A8.bytes,-559038737),
            Register(Address at 0x2B8.bytes,-559038737),
            Register(Address at 0x2C8.bytes,-559038737)
          )
        ),
        RegisterFileA(
          AddressRange(pos = Address at 0x500.bytes, lim = Address at 0x550.bytes),
          Register(Address at 0x500.bytes,-559038737),
          List(
            Register(Address at 0x508.bytes,-559038737),
            Register(Address at 0x518.bytes,-559038737),
            Register(Address at 0x528.bytes,-559038737),
            Register(Address at 0x538.bytes,-559038737),
            Register(Address at 0x548.bytes,-559038737)
          )
        )
      ),
      RegisterFileB(
        AddressRange(pos = Address at 0x550.bytes, lim = Address at 0x5A0.bytes),
        List(
          Register(Address at 0x550.bytes,-559038737),
          Register(Address at 0x570.bytes,-559038737),
          Register(Address at 0x590.bytes,-559038737)
        ),
        Register(Address at 0x598.bytes,-559038737)
      )
    )
}

