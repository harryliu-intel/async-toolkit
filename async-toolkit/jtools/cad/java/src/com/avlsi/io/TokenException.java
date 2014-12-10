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
 * Abstract base class for all classes thrown to indicate that an attempt to
 * parse a token.
 *
 * @see StreamLexer
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public abstract class TokenException extends RuntimeException {
    public TokenException() {
        super();
    }

    public TokenException(final String detail) {
        super(detail);
    }

    public TokenException(final String detail, final Throwable cause) {
        super(detail, cause);
    }

    public TokenException(final Throwable cause) {
        super(cause);
    }
}
