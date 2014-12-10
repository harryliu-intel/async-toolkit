/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class CompilationUnit implements VerilogObject {
    private final VerilogObject[] objects;
    public CompilationUnit(final VerilogObject[] objects) {
        this.objects = objects;
    }
    public void accept(final VerilogVisitor v) {
        v.compilationUnit(objects);
    }
}
