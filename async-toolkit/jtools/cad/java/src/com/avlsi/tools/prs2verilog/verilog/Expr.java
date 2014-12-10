/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class Expr implements VerilogObject {
    private final String expr;
    public Expr(final String expr) {
        this.expr = expr;
    }
    public void accept(VerilogVisitor v) {
        v.expr(expr);
    }
}
