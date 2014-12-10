/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class Ident implements VerilogObject {
    protected String ident;
    private final boolean escape;
    public Ident(final String ident) {
        this(ident, true);
    }
    public Ident(final String ident, final boolean escape) {
        this.ident = ident;
        this.escape = escape;
    }
    public void accept(VerilogVisitor v) {
        v.ident(ident, escape);
    }
}
