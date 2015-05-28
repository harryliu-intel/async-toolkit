/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/tools/cast2verilog/Cast2Verilog.java#3 $
 * $DateTime: 2015/02/20 05:49:00 $
 * $Author: rliu68 $
 */

package com.avlsi.tools.cast2verilog;

import java.io.Writer;
import java.util.Set;

import com.avlsi.tools.prs2verilog.Prs2Verilog;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

/**
 * Do not write out a Verilog for a single cell more than once.
 **/
class NoRepeatVisitor implements Prs2Verilog.VisitorFactory {
    private Set<String> done;
    private Prs2Verilog.SingleWriterVisitor single;
    public NoRepeatVisitor(final Writer writer, final Set<String> done) {
        this(writer, done, true);
    }
    public NoRepeatVisitor(final Writer writer, final Set<String> done,
                           final boolean ifdef) {
        this.done = done;
        this.single = new Prs2Verilog.SingleWriterVisitor(writer, ifdef);
    }
    public VerilogVisitor getVisitor(final String cellName) {
        return done.add(cellName) ? single.getVisitor(cellName) : null;
    }
    public void doneVisitor(final VerilogVisitor visitor) {
        single.doneVisitor(visitor);
    }
}
