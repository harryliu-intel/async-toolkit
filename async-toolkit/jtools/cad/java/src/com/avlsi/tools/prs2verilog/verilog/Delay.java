// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;

public class Delay implements VerilogObject {
    private final VerilogObject rise, fall, off;

    public Delay(final VerilogObject rise) {
        this(rise, null);
    }
    public Delay(final VerilogObject rise, final VerilogObject fall) {
        this(rise, fall, null);
    }
    public Delay(final VerilogObject rise, final VerilogObject fall,
                 final VerilogObject off) {
        this.rise = rise;
        this.fall = fall;
        this.off = off;
    }
    public void accept(VerilogVisitor v) {
        v.delay(rise, fall, off);
    }
}
