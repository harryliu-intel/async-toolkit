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

package com.avlsi.io;

/**
 * Thrown to indicate that an attempt to parse an int token failed.
 *
 * @see StreamLexer
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class TokenNotIntException extends TokenException {
    public TokenNotIntException() {
        super();
    }

    public TokenNotIntException(final String detail) {
        super(detail);
    }

    public TokenNotIntException(final Throwable cause) {
        super(cause);
    }

    public TokenNotIntException(
            final String message,
            final Throwable cause) {
        super(message, cause);
    }
}
