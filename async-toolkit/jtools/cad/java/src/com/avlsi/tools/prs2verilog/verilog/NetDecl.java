/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class NetDecl implements VerilogObject {
    private final String type;
    private final VerilogObject ident, delay;
    public NetDecl(final String type,
                   final VerilogObject ident,
                   final VerilogObject delay) {
        this.type = type;
        this.ident = ident;
        this.delay = delay;
    }
    public NetDecl(final VerilogObject ident) {
        this("wire", ident, null);
    }
    public NetDecl(final VerilogObject ident, VerilogObject delay) {
        this("wire", ident, delay);
    }
    public NetDecl(final String type, final VerilogObject ident) {
        this(type, ident, null);
    }
    public void accept(VerilogVisitor v) {
        v.netDecl(type, ident, delay);
    }
}
