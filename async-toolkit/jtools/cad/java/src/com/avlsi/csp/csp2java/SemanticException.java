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

package com.avlsi.csp.csp2java;

/**
 * Exception thrown in case of semantic error with CSP.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class SemanticException extends Exception {
    /**
     * Class constructor.
     **/
    public SemanticException(final String message) {
        super(message);
    }

    public SemanticException(final String message, Throwable cause) {
        super(message, cause);
    }

    public SemanticException(Throwable cause) {
        super(cause);
    }
}
