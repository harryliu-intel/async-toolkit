// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.csp.csp2java.runtime;

import java.math.BigInteger;

public class CspBoolean extends CspInteger implements Packable {
    public CspBoolean(BigInteger val) {
        super(val);
    }
    public int pack(CspInteger packed, int start) {
        packed.assignBits(start, start, booleanValue() ? CspInteger.ONE
                                                       : CspInteger.ZERO);
        return start + 1;
    }
    public int unpack(CspInteger packed, int start) {
        CspInteger parts = packed.extractBits(start, start);
        setValue(parts.equals(CspInteger.ONE) ? CspInteger.TRUE
                                              : CspInteger.FALSE);
        return start + 1;
    }
    public CspCloneableValue duplicate() {
        return new CspBoolean(getValue());
    }
}
