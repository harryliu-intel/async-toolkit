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
 * Thrown to indicate that an attempt to parse a literal token failed.
 *
 * @see StreamLexer
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
class TokenNotLiteralException extends TokenException {
    public TokenNotLiteralException() {
        super();
    }

    public TokenNotLiteralException(final String detail) {
        super(detail);
    }
}
