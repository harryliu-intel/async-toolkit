// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class ContinuousAssign implements VerilogObject {
    private final VerilogObject strength;
    private final VerilogObject delay;
    private final VerilogObject lhs, rhs;
    public ContinuousAssign(final VerilogObject strength,
                            final VerilogObject delay,
                            final VerilogObject lhs,
                            final VerilogObject rhs) {
        this.strength = strength;
        this.delay = delay;
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public void accept(VerilogVisitor v) {
        v.continuousAssign(strength, delay, lhs, rhs);
    }
}
