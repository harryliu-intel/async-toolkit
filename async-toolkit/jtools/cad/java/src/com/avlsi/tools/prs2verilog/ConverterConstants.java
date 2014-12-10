/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */         

package com.avlsi.tools.prs2verilog;

import com.avlsi.file.common.HierName;

public class ConverterConstants {
    public static String getDelayMacroString() {
        return "PRS2VERILOG_DELAY";
    }

    public static String getDelayBiasString() {
        return "PRS2VERILOG_DELAYBIAS";
    }

    public static String getExtraDelayString(final HierName node,
                                             final boolean up) {
        return "PRS2VERILOG_EXTRADELAY_" + (up ? "UP" : "DOWN") + "_" +
               node.getAsString('.').replace(',','_');
    }
}
