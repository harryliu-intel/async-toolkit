/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class NamedPort implements VerilogObject {
    private final VerilogObject portName;
    private final VerilogObject port;
    public NamedPort(final VerilogObject portName, final VerilogObject port) {
        this.portName = portName;
        this.port = port;
    }
    public void accept(VerilogVisitor v) {
        v.namedPort(portName, port);
    }
}
