/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class Module implements VerilogObject {
    private final VerilogObject ident;
    private final VerilogObject[] ports;
    private final VerilogObject[] items;
    public Module(final VerilogObject ident,
                  final VerilogObject[] ports,
                  final VerilogObject[] items) {
        this.ident = ident;
        this.ports = ports;
        this.items = items;
    }
    public void accept(final VerilogVisitor v) {
        v.module(ident, ports, items);
    }
}
