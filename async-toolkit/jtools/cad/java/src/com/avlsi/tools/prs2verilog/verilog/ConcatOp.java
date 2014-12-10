/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class ConcatOp implements VerilogObject {
    private final VerilogObject[] elements;
    public ConcatOp(final VerilogObject[] elements) {
        this.elements = elements;
    }
    public void accept(VerilogVisitor v) {
        v.concatOp(elements);
    }
}
