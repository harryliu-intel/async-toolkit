package com.intel.cg.hpfd.csr.testData

import com.intel.cg.hpfd.madisonbay.Memory._

//scalastyle:off magic.number
trait NonContiguousAddressingExample {

  lazy val nonContiguousAddressingExpectedValue =
    nonContiguous.AddressMap(
      AddressRange(Address at 0.bytes, Address at 0x2E0.bytes),
      List(
        nonContiguous.RegisterFileA(
          AddressRange(pos = Address at 0.bytes, lim = Address at 0x28.bytes),
          common.Register(Address at 0.bytes,-559038737),
          List(
            common.Register(Address at 0x8.bytes,-559038737),
            common.Register(Address at 0x20.bytes,-559038737)
          )
        ),
        nonContiguous.RegisterFileA(
          AddressRange(pos = Address at 0x280.bytes, lim = Address at 0x2A8.bytes),
          common.Register(Address at 0x280.bytes,-559038737),
          List(
            common.Register(Address at 0x288.bytes,-559038737),
            // placeReg alignment changes register pos address!
            common.Register(Address at 0x2A0.bytes,-559038737)
          )
        )
      ),
      nonContiguous.RegisterFileB(
        AddressRange(pos = Address at 0x2A8.bytes, lim = Address at 0x2E0.bytes),
        List(
          common.Register(Address at 0x2A8.bytes,-559038737),
          // placeReg alignment - 2CC aligned to 2D0
          common.Register(Address at 0x2D0.bytes,-559038737)
        ),
        common.Register(Address at 0x2D8.bytes,-559038737)
      )
    )
}

