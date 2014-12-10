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
public class BlockEnvironment extends AbstractSplicingEnvironment {
    protected LocalEnvironment localEnv;

    public BlockEnvironment(final Environment parentEnv) {
        super(parentEnv);

        this.localEnv = new LocalEnvironment();
    }

    public BlockEnvironment(final Environment parentEnv,
                            final LocalEnvironment localEnv) {
        super(parentEnv);

        if (localEnv == null)
            throw new IllegalArgumentException("localEnv was null");

        this.localEnv = localEnv;
    }

    protected Environment getLocalEnvironment() {
        return localEnv;
    }

    /**
     * Binds the value to the symbol in the local environment.  Allows
     * binding of a symbol already bound in the parent environment.
     **/
    public void bind(final Symbol sym, final Value val)
        throws SymbolRedeclaredException {
        localEnv.bind(sym, val);
    }

    /**
     * Returns a new environment with the same parent,
     * but a separate LocalEnvironment, which is a copy this
     * LocalEnvironment.
     **/
    public BlockEnvironment getSiblingEnvironment() {
        return new BlockEnvironment(parentEnv, localEnv.duplicate());
    }

    public String getLocalEnvironmentString() {
        return localEnv.toString();
    }

    /**
     * Return an environment with the same parent, and same values, 
     * but arrays are shallow copied.
     **/
    public BlockEnvironment protectedArrayEnvironment() {
        return new BlockEnvironment(parentEnv,
                localEnv.protectedArrayEnvironment());
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();

        sb.append("BlockEnvironment(\n");
        sb.append("parent:");
        sb.append(parentEnv.toString());
        sb.append(",me:\n");
        sb.append(localEnv.toString());
        sb.append(")\n");

        return sb.toString();
    }

    /**
     * Add all of the key-value pairs in env to this environment, with
     * appropriate duplication warnings.
     **/
    public void absorbEnv(LocalEnvironment env)
        throws SymbolRedeclaredException {
        localEnv.absorbEnv(env);
    }
    
    public EnvironmentIterator iterator() {
        return new ChainedEnvironmentIterator( localEnv, parentEnv );
    }

    public EnvironmentEntryIterator entryIterator() {
        return new ChainedEnvironmentEntryIterator( localEnv, parentEnv );
    }
}
