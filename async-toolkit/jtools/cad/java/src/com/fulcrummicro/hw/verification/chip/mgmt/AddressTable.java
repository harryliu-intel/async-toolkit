package com.fulcrummicro.hw.verification.chip.mgmt;

import java.util.Map;

import com.fulcrummicro.hw.verification.chip.mgmt.registers.Register;
import com.fulcrummicro.hw.verification.chip.mgmt.registers.RegisterField;
import com.fulcrummicro.hw.verification.chip.mgmt.registers.UnknownAddressException;

/**
 * AddressTable - defines a structure which contains register addresses
 *
 * @author Naru Sundar
 */

public interface AddressTable {

    /* accessors to lookup addresses */
    int lookupAddress(String regname, int entry2, int entry1, int entry0, int word) 
        throws UnknownAddressException;

    int lookupAddress(String regname, int entry1, int entry0, int word) 
        throws UnknownAddressException;

    int lookupAddress(String regname, int entry, int word) 
        throws UnknownAddressException;

    int lookupAddress(String regname, int entry) 
        throws UnknownAddressException;

    int lookupAddress(String regname) 
        throws UnknownAddressException;

    /* gets a field structure out of a register */
    RegisterField getField(String reg, String field);

    /* method to get the actual register table */
    Map<String,Register> getRegisterMap();
}
