/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.cast.impl;

import java.util.NoSuchElementException;

import com.avlsi.cast.impl.Value;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.EnvironmentIterator;
import com.avlsi.cast.impl.ChainedEnvironmentIterator;

/**
 * This class represents environments with a parent and a local
 * environment.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
abstract class AbstractSplicingEnvironment implements Environment {
    protected Environment parentEnv;

    protected AbstractSplicingEnvironment(final Environment parentEnv) {
        if (parentEnv == null)
            throw new IllegalArgumentException("parentEnv was null");

        this.parentEnv = parentEnv;
    }

    protected abstract Environment getLocalEnvironment();

    /**
     * Looks up the value in the local environment.  If 
     * it is not found there, lookup in the parent environment.
     * This necessitates using a NullEnvironment at the top level.
     **/
    public Value lookup(final Symbol sym)
        throws AmbiguousLookupException {
        final Value v = getLocalEnvironment().lookup(sym);
        if (v != null)
            return v;
        return parentEnv.lookup(sym);
    }

    /** Checks the local environment first, then the parent. **/
    public boolean contains(final Symbol sym) {
        return(getLocalEnvironment().contains(sym) ||
               parentEnv.contains(sym));
    }
    
    public EnvironmentIterator iterator() {
        return new ChainedEnvironmentIterator( getLocalEnvironment(),
                                               parentEnv );
    }

    public EnvironmentEntryIterator entryIterator() {
        return new ChainedEnvironmentEntryIterator(getLocalEnvironment(),
                                                   parentEnv);
    }
}
