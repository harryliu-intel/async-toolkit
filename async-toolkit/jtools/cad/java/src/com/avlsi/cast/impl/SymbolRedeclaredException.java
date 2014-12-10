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
 * Exception thrown by {@link Environment#bind} when a symbol has already
 * been declared.
 *
 * @see Environment
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class SymbolRedeclaredException extends Exception {
    private final Symbol sym;

    public SymbolRedeclaredException() {
        super();

        this.sym = null;
    }

    public SymbolRedeclaredException(final Symbol sym) {
        super("Symbol \"" + sym.getString() +
              "\" redeclared.  Did you array cells with different metaparameters?");

        this.sym = sym;
    }
}
