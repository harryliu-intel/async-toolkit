/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class Parameter implements VerilogObject {
    private final VerilogObject type, ident, value;
    public Parameter(final VerilogObject type, final VerilogObject ident,
                     final VerilogObject value) {
        this.type = type;
        this.ident = ident;
        this.value = value;
    }
    public void accept(final VerilogVisitor v) {
        v.parameter(type, ident, value);
    }
}
