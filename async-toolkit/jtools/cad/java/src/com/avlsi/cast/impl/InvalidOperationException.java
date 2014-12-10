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
 * Exception thrown when an illegal operation occurs.  Ie 
 * <code>bool + bool<code>, * or <code>int / 0<code>.
 *
 * @see Value
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class InvalidOperationException extends Exception {
    public InvalidOperationException() {
        super();
    }

    public InvalidOperationException(final String detail) {
        super(detail);
    }

    public InvalidOperationException(final String detail, Throwable cause) {
        super(detail, cause);
    }

    public InvalidOperationException(Throwable cause) {
        super(cause);
    }
}
