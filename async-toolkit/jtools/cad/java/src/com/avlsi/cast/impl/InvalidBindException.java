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

package com.avlsi.cast.impl;

/**
 * Exception thrown by {@link TopLevelEnvironment.UnbindableEnvironment#bind}
 * when a bind attempt is made.
 *
 * @see TopLevelEnvironment.UnbindableEnvironment
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class InvalidBindException extends RuntimeException {
    public InvalidBindException() {
        super();
    }

    public InvalidBindException(final String detail) {
        super(detail);
    }
}
