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
 * Exception thrown by {@link ImportEnvironment#lookup} when a symbol has
 * been declared in multiple environments.
 *
 * @see ImportEnvironment
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class AmbiguousLookupException extends Exception {
    private final Symbol sym;

    public AmbiguousLookupException() {
        super();

        this.sym = null;
    }

    public AmbiguousLookupException(final Symbol sym,
                                    final String env1,
                                    final String env2) {
        super();

        this.sym = sym;
    }
}
