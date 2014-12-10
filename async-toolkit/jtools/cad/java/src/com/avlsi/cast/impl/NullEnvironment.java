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

/**
 * Null environment class.  Cannot binds and lookups fail.  Used at the top
 * of the environment chain.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class NullEnvironment implements Environment {
    private static NullEnvironment singleton = null;

    private NullEnvironment() { }

    /** @throws InvalidBindException  Always. **/
    public void bind(final Symbol sym, final Value val) {
        throw new InvalidBindException("can't bind in null env");
    }

    public Value lookup(final Symbol sym) {
        return null;
    }

    public boolean contains(final Symbol sym) {
        return false;
    }


    public EnvironmentIterator iterator() {
        return new EnvironmentIterator () {
                
                public final boolean hasNext() {
                    return false;
                }

                public final EnvironmentEntry next() {
                    throw new NoSuchElementException();
                }
            };
    }

    public EnvironmentEntryIterator entryIterator() {
        return new EnvironmentEntryIterator () {
                
                public final boolean hasNext() {
                    return false;
                }

                public final EnvironmentEntry next() {
                    throw new NoSuchElementException();
                }
            };
    }

    public static NullEnvironment getInstance() {
        if (singleton == null) singleton = new NullEnvironment();
        return singleton;
    }
}
