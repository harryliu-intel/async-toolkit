// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.fulcrummicro.hw.verification.chip.mgmt.registers;

/** Register Type definition class **/
public enum RegisterType {
    /** read-write */ RW,
    /** read only */ RO,
    /** shadow/survive read-write */ SRW, 
    /** write only */ WO,
    /** clear on write */ CW,
    /** clear on write 1 */ CW1,
    /** set on write 1 */ SW1,
    /** clear on read */ CR,
    /** clear on access */ CA,
    /** pin */ @Deprecated PIN, 
    /** reserved */ RV,
    /** "not applicable" used for registers with mixed field types */ @Deprecated NA;
    
    public String toSystemRdlType() {
        switch(this) {
            case CW1: return "RW/1C";
            case SW1: return "RW/1S";
            case CR: return "RO/C";
            case CA: return "RO/C";
            case RV: return "RSV";
        }
        return toString();
    }
}

