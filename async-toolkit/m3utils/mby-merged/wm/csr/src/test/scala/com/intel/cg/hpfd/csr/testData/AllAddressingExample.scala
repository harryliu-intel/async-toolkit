package com.intel.cg.hpfd.csr.testData

import com.intel.cg.hpfd.madisonbay.Memory._

//scalastyle:off magic.number
trait AllAddressingExample {

  val allAddressingExpectedValue = all.AddressMap(
    AddressRange(pos = Address at 0.bytes, lim = Address at 0x17C20.bytes),
    List(
      all.RegisterFileA(
        AddressRange(pos = Address at 0.bytes, lim = Address at 0xA8.bytes),
        common.Register(Address at 0x10.bytes,-559038737),
        List(
          common.Register(Address at 0x80.bytes,-559038737),
          common.Register(Address at 0x90.bytes,-559038737),
          common.Register(Address at 0xA0.bytes,-559038737)
        )
      ),
      all.RegisterFileA(
        AddressRange(pos = Address at 0xA8.bytes, lim = Address at 0x128.bytes),
        common.Register(Address at 0xB8.bytes,-559038737),
        List(
          common.Register(Address at 0x100.bytes,-559038737),
          common.Register(Address at 0x110.bytes,-559038737),
          common.Register(Address at 0x120.bytes,-559038737)
        )
      ),
      all.RegisterFileA(
        AddressRange(pos = Address at 0x128.bytes, lim = Address at 0x1A8.bytes),
        common.Register(Address at 0x138.bytes,-559038737),
        List(
          common.Register(Address at 0x180.bytes,-559038737),
          common.Register(Address at 0x190.bytes,-559038737),
          common.Register(Address at 0x1A0.bytes,-559038737)
        )
      )
    ),
    List(
      all.RegisterFileB(
        AddressRange(pos = Address at 0xFA00.bytes, lim = Address at 0xFC20.bytes),
        List(
          common.Register(Address at 0xFC00.bytes,-559038737),
          common.Register(Address at 0xFC08.bytes,-559038737),
          common.Register(Address at 0xFC10.bytes,-559038737)
        ),
        common.Register(Address at 0xFC18.bytes,-559038737)
      ),
      all.RegisterFileB(
        AddressRange(pos = Address at 0x17A00.bytes, lim = Address at 0x17C20.bytes),
        List(
          common.Register(Address at 0x17C00.bytes,-559038737),
          common.Register(Address at 0x17C08.bytes,-559038737),
          common.Register(Address at 0x17C10.bytes,-559038737)
        ),
        common.Register(Address at 0x17C18.bytes,-559038737)
      )
    )
  )
}

