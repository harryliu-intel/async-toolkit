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
 * Thrown to indicate that an attempt to parse a symbol token failed.
 *
 * @see StreamLexer
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class TokenNotSymbolException extends TokenException {
    public TokenNotSymbolException() {
        super();
    }

    public TokenNotSymbolException(final String detail) {
        super(detail);
    }

    public TokenNotSymbolException(final String detail,
                                   final Throwable cause) {
        super(detail, cause);
    }

    public TokenNotSymbolException(final Throwable cause) {
        super(cause);
    }
}
