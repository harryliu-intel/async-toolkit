/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class ArrayAccess implements VerilogObject {
    private final VerilogObject array, index;
    public ArrayAccess(final VerilogObject array, final VerilogObject index) {
        this.array = array;
        this.index = index;
    }
    public void accept(VerilogVisitor v) {
        v.arrayAccess(array, index);
    }
}
