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

import com.avlsi.cast.impl.EnvironmentIterator;

/**
 * This class represents environments.  You can bind a value to a symbol,
 * and lookup the value of a symbol.  The value contains the type
 * information, and is responsible for allowing and disallowing operations.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public interface Environment {
    /**
     * @throws InvalidBindException
     **/
    void bind(final Symbol sym, final Value val)
        throws SymbolRedeclaredException;

    boolean contains(final Symbol sym);

    Value lookup(final Symbol sym) throws AmbiguousLookupException;

    EnvironmentIterator iterator();

    /**
     * Returns an iterator over all key/value pairs in the environment.
     **/
    EnvironmentEntryIterator entryIterator();
}
