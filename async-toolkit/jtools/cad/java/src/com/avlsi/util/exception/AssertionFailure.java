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

package com.avlsi.util.exception;

/**
 * Error thrown in case of assertion failure.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class AssertionFailure extends Error {
    /**
     * Class constructor.
     **/
    public AssertionFailure() { }

    /**
     * Class constructor.
     * @param detail  detail message
     **/
    public AssertionFailure(final String detail) {
        super(detail);
    }

    public AssertionFailure(final Throwable cause) {
        super(cause);
    }
}
