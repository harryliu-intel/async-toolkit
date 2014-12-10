/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class ModuleInst implements VerilogObject {
    private final VerilogObject ident;
    private final VerilogObject module;
    private final VerilogObject[] parameters;
    private final VerilogObject[] ports;
    public ModuleInst(final VerilogObject ident,
                      final VerilogObject module,
                      final VerilogObject[] parameters,
                      final VerilogObject[] ports) {
        this.ident = ident;
        this.module = module;
        this.parameters = parameters;
        this.ports = ports;
    }
    public void accept(final VerilogVisitor v) {
        v.moduleInst(ident, module, parameters, ports);
    }
}
