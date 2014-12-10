/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.cast.impl;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;

import com.avlsi.cast.impl.Value;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.EnvironmentIterator;


import com.avlsi.util.exception.AssertionFailure;

/**
 * Local environment, has no parent capabilities,
 * implements a mapping from Symbol to Value.
 * It's just a HashMap with some type safety.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class LocalEnvironment implements Environment {
    private final Map map;

    public LocalEnvironment() {
        this(new HashMap());
    }

    public LocalEnvironment(final Map map) {
        this.map = map;
    }

    /**
     * If the Symbol is global, binds both the global
     * and the local forms to the value.
     * Global signals end with a '!'.
     **/
    public void bind(final Symbol sym, final Value val)
        throws SymbolRedeclaredException {

        internalBind(sym, val);
        if (sym.isGlobal())
            internalBind(sym.stripBang(), val);
    }
    /**
     * Like bind, but without the duplication of globals.  For
     * internal use only.
     **/
    private void internalBind(final Symbol sym, final Value val)
        throws SymbolRedeclaredException {
        if (map.containsKey(sym)) {
            // If two values have the same address, then they must really be
            // the same value, and only appears to be redeclared due to
            // multiple inheritence; we don't count this as an error.  This
            // means that we must never reuse the same value object to bind to
            // different symbols.
            if (map.get(sym) != val)
                throw new SymbolRedeclaredException(sym);
        } else {
            map.put(sym, val);
        }
    }

    public Value lookup(final Symbol sym) {
        return (Value) map.get(sym);
    }

    public boolean contains(final Symbol sym) {
        return map.containsKey(sym);
    }

    /**
     * Returns a deep copy of the environment.
     **/
    public LocalEnvironment duplicate() {
        final LocalEnvironment env = new LocalEnvironment();

        for (final Iterator i = map.entrySet().iterator(); i.hasNext(); ) {
            final Entry entry = (Entry) i.next();
            final Symbol sym = (Symbol) entry.getKey();
            final Value val = (Value) entry.getValue();

            try {
                env.internalBind(sym, val.duplicate());
            } catch (SymbolRedeclaredException e) {
                throw (AssertionFailure)
                    new AssertionFailure(sym + " = " + val + " redeclared!")
                        .initCause(e);
            }
        }

        return env;
    }

    /**
     * Return an environment with the same values, but arrays are shallow
     * copied.
     **/
    public LocalEnvironment protectedArrayEnvironment() {
        final LocalEnvironment env = new LocalEnvironment();

        for (final Iterator i = map.entrySet().iterator(); i.hasNext(); ) {
            final Entry entry = (Entry) i.next();
            final Symbol sym = (Symbol) entry.getKey();
            final Value val = (Value) entry.getValue();

            if (val instanceof ArrayValue)
                env.map.put(sym, ((ArrayValue) val).shallowCopy());
            else
                env.map.put(sym, val);

        }

        return env;
    }

    public String toString() {
        return "LocalEnvironment(" + map.toString() + ")";
    }

    /**
     * Add all of the key-value pairs in env to this environment, with
     * appropriate duplication warnings.
     **/
    public void absorbEnv(Environment env)
        throws SymbolRedeclaredException {
        if (env == null) return;
        EnvironmentEntryIterator entries = env.entryIterator();
        while (entries.hasNext()) {
            EnvironmentEntry entry = entries.next();
            internalBind(entry.getName(), entry.getValue());
        }
    }

    public EnvironmentIterator iterator() {
        return new EnvironmentIterator () {
                
                private final Iterator m_Iter = map.entrySet().iterator();

                public final boolean hasNext() {
                    return m_Iter.hasNext();
                }

                public final EnvironmentEntry next() {
                    final Entry entry = ( Entry ) m_Iter.next();
                    return new EnvironmentEntry((Symbol) entry.getKey(),
                                                (Value) entry.getValue());
                }
            };
    }

    public EnvironmentEntryIterator entryIterator() {
        return new EnvironmentEntryIterator () {
                
                private final Iterator m_Iter = map.entrySet().iterator();

                public final boolean hasNext() {
                    return m_Iter.hasNext();
                }

                public final EnvironmentEntry next() {
                    final Entry entry = ( Entry ) m_Iter.next();
                    return new EnvironmentEntry((Symbol) entry.getKey(),
                                                (Value) entry.getValue());
                }
            };
    }
}
