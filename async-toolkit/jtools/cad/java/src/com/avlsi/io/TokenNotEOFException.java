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
 * Thrown to indicate that an attempt to parse a EOF token failed.
 *
 * @see StreamLexer
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
class TokenNotEOFException extends TokenException {
    public TokenNotEOFException() {
        super();
    }

    public TokenNotEOFException(final String detail) {
        super(detail);
    }
}
