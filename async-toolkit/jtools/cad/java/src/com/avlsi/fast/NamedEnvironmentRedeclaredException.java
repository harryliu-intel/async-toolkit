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

package com.avlsi.fast;

/**
 * Exception thrown by {@link EnvBlock} for duplicated named environments.
 **/
public final class NamedEnvironmentRedeclaredException extends Exception {
    public NamedEnvironmentRedeclaredException(final String detail) {
        super(detail);
    }

    public String toString() {
        return "NamedEnvironmentRedeclaredException: "
            + getMessage();
    }
}
