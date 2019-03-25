/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import java.util.Arrays;
import java.util.stream.Collectors;

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
    public String toString() {
        return Arrays.stream(parts)
                     .map(Object::toString)
                     .collect(Collectors.joining("."));
    }
}
