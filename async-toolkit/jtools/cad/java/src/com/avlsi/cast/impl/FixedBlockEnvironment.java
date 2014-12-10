/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.cast.impl;

/**
 * Like BlockEnvironment, but doesn't allow definitions in the parent
 * environment to be overriden by definitions in the child
 * environment.  "Fixed" in this case means fixed in place, static.
 **/

public class FixedBlockEnvironment extends BlockEnvironment {

    public FixedBlockEnvironment(final Environment parentEnv) {
        super(parentEnv);
    }

    public FixedBlockEnvironment(final Environment parentEnv,
                                 final LocalEnvironment localEnv) {
        super(parentEnv, localEnv);
    }

    /**
     * Binds the value to the symbol in the local environment.  Does
     * not allow binding of a symbol already bound in the parent
     * environment.
     **/
    public void bind(final Symbol sym, final Value val)
        throws SymbolRedeclaredException {
        if (parentEnv.contains(sym)) {
            throw new SymbolRedeclaredException(sym);
        }
        localEnv.bind(sym, val);
    }
}
