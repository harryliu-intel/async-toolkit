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
 * This class represents environments for block scoping.  Declarations
 * are made within the environment, and will disappear at the end of the
 * block.  Lookups that fail locally are forwarded to the parent
 * environment.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class ChainEnvironment implements Environment {
    private Environment parentEnv;
    private Environment childEnv;

    public ChainEnvironment(final Environment parentEnv,
                            final Environment childEnv) {
        if (parentEnv == null)
            throw new IllegalArgumentException("parentEnv was null");
        if (childEnv == null)
            throw new IllegalArgumentException("childEnv was null");

        this.parentEnv = parentEnv;
        this.childEnv = childEnv;
    }

    /**
     * Binds the value to the symbol in the child environment.
     **/
    public void bind(final Symbol sym, final Value val)
        throws SymbolRedeclaredException {
        childEnv.bind(sym, val);
    }

    /**
     * Looks up the value in the child environenment.  If 
     * it is not found there, lookup in the parent environment.
     * This nececcisates using a NullEnvironment at the top level.
     **/
    public Value lookup(final Symbol sym)
        throws AmbiguousLookupException {
        final Value v = childEnv.lookup(sym);
        if (v != null)
            return v;
        return parentEnv.lookup(sym);
    }

    /** Checks the child environment first, then the parent. **/
    public boolean contains(final Symbol sym) {
        return(childEnv.contains(sym) || parentEnv.contains(sym));
    }

    public EnvironmentIterator iterator() {
        return new ChainedEnvironmentIterator( childEnv, parentEnv );
    }

    public EnvironmentEntryIterator entryIterator() {
        return new ChainedEnvironmentEntryIterator( childEnv, parentEnv );
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();

        sb.append("ChainEnvironment(\n");
        sb.append("parent:");
        sb.append(parentEnv.toString());
        sb.append(",me:\n");
        sb.append(childEnv.toString());
        sb.append(")\n");

        return sb.toString();
    }
}
