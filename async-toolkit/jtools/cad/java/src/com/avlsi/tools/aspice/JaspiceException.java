/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.aspice;

/*****************************************************************
 * Class to represent exceptions in Jaspice.
 *****************************************************************/
public final class JaspiceException
        extends java.lang.Exception {
    public JaspiceException(final String message) {
        super(message);
    }
    public JaspiceException(final String message, Throwable cause) {
        super(message, cause);
    }
    public JaspiceException(Throwable cause) {
        super(cause);
    }
}
