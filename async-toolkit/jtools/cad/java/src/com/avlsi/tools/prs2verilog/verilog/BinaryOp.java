/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class BinaryOp implements VerilogObject {
    private final VerilogObject op1, op2;
    private final String op;
    public BinaryOp(final VerilogObject op1, final String op,
                    final VerilogObject op2) {
        this.op1 = op1;
        this.op = op;
        this.op2 = op2;
    }
    public void accept(VerilogVisitor v) {
        v.binaryOp(op1, op, op2);
    }
}
