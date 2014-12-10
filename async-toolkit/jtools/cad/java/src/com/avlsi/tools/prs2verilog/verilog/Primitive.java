/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class Primitive implements VerilogObject {
    private final String type;
    private final VerilogObject delay;
    private final VerilogObject ident;
    private final VerilogObject[] terminals;
    public Primitive(final String type,
                     final VerilogObject delay,
                     final VerilogObject ident,
                     final VerilogObject[] terminals) {
        this.type = type;
        this.delay = delay;
        this.ident = ident;
        this.terminals = terminals;
    }
    public void accept(final VerilogVisitor v) {
        v.primitive(type, delay, ident, terminals);
    }
}
