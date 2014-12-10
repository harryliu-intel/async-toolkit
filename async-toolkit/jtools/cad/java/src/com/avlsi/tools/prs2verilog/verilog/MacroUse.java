/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class MacroUse implements VerilogObject {
    private final VerilogObject ident;
    private final VerilogObject[] args;
    public MacroUse(final VerilogObject ident, final VerilogObject[] args) {
        this.ident = ident;
        this.args = args;
    }
    public void accept(VerilogVisitor v) {
        v.macroUse(ident, args);
    }
}
