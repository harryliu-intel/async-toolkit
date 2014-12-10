/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class HierIdent implements VerilogObject {
    protected VerilogObject[] parts;
    public HierIdent(final VerilogObject[] parts) {
        this.parts = parts;
    }
    public void accept(VerilogVisitor v) {
        v.hierIdent(parts);
    }
}
