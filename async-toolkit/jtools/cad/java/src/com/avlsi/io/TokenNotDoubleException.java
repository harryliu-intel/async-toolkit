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
 * Thrown to indicate that an attempt to parse a double token failed.
 *
 * @see StreamLexer
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class TokenNotDoubleException extends TokenException {
    public TokenNotDoubleException() {
        super();
    }

    public TokenNotDoubleException(final String detail) {
        super(detail);
    }

    public TokenNotDoubleException(final Throwable cause) {
        super(cause);
    }
}
