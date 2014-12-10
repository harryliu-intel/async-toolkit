/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.lvs;

/**
 * Class to represent warnings and errors in lvs.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class LVSProblem {
    public static final int LVS_ERROR = 0;
    public static final int LVS_WARNING = 1;

    final String message;
    final int type;

    public LVSProblem(final String message, final int type) {
        this.message = message;
        this.type = type;
    }

    public LVSProblem(final String message) {
        this(message, LVS_ERROR);
    }

    public String getMessage() {
        return message;
    }

    public int getType() {
        return type;
    }

    public String toString() {
        return "LVS " + (type == LVS_ERROR ? "Error" : "Problem") + ": " + message;
    }
}
