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
 * This class merges two environments in a tree.  Binds are not allowed,
 * only lookups.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class SplicingEnvironment extends AbstractSplicingEnvironment {
    protected Environment localEnv;

    public SplicingEnvironment(final Environment parentEnv,
                               final Environment localEnv) {
        super(parentEnv);

        if (localEnv == null)
            throw new IllegalArgumentException("localEnv was null");

        this.localEnv = localEnv;
    }

    protected Environment getLocalEnvironment() {
        return localEnv;
    }

    /**
     * Bind not allowed; throws InvalidBindException.
     * 
     * @throws InvalidBindException  Always.
     **/
    public void bind(final Symbol sym, final Value val) {
        throw new InvalidBindException("SplicingEnvironment " +
                "doesn't allow bind().");
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();

        sb.append("SplicingEnvironment(\n");
        sb.append("parent:");
        sb.append(parentEnv.toString());
        sb.append(",me:\n");
        sb.append(localEnv.toString());
        sb.append(")\n");

        return sb.toString();
    }
}
