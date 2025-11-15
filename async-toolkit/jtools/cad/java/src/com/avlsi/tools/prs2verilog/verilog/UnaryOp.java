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

public class UnaryOp implements VerilogObject {
    private final String op;
    private final VerilogObject operand;
    public UnaryOp(final String op, final VerilogObject operand) {
        this.op = op;
        this.operand = operand;
    }
    public void accept(VerilogVisitor v) {
        v.unaryOp(op, operand);
    }
}
