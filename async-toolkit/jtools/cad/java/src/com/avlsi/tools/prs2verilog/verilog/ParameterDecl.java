/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class ParameterDecl implements VerilogObject {
    private final VerilogObject ident;
    private final String type;
    public ParameterDecl(final VerilogObject ident, final String type) {
        this.ident = ident;
        this.type = type;
    }
    public void accept(final VerilogVisitor v) {
        v.parameterDecl(ident, type);
    }
}
