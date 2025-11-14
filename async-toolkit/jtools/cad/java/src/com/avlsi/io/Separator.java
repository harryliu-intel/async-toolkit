// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/tools/cast2verilog/Cast2RTL.java#17 $
 * $DateTime: 2015/09/11 21:45:54 $
 * $Author: rliu68 $
 */

package com.avlsi.io;

import java.io.PrintWriter;

public class Separator {
    private final PrintWriter out;
    private final String sep;
    private boolean first;
    public Separator(final PrintWriter out, final String sep) {
        this.out = out;
        this.sep = sep;
        this.first = true;
    }
    public Separator(final PrintWriter out) {
        this(out, ",\n");
    }
    public void print(final String s) {
        if (first) {
            first = false;
        } else {
            out.print(sep);
        }
        out.print(s);
    }
    public void ifdef(final String s) {
        out.println("\n`ifdef " + s);
    }
    public void endif() {
        out.println("\n`endif");
    }
}
