/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class LineComment implements VerilogObject {
    private final String comment;
    public LineComment(final String comment) {
        this.comment = comment;
    }
    public void accept(VerilogVisitor v) {
        v.lineComment(comment);
    }
}
