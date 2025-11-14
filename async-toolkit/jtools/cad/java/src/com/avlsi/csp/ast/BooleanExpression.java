// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.csp.ast;

public class BooleanExpression extends IntegerExpression {
    private final boolean val;
    public BooleanExpression(final boolean val) {
        super(val ? "-1" : "0", 10);
        this.val = val;
    }
    public boolean booleanValue() {
        return val;
    }
}
