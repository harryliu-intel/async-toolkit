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
 * A LoopEnvironment, binds loop index. All lookups are first checked
 * against the loop index, then passed to the parent environment.  
 * All declarations are made in the outer environment.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class LoopEnvironment implements Environment {
    private final Environment env;
    private final Symbol loopSym;
    private final Value val;

    public LoopEnvironment(final Environment env, 
                           final Symbol loopSym,
                           final Value val) {
        if (env == null)
            throw new IllegalArgumentException("parent env was null");

        this.env = env;
        this.loopSym = loopSym;
        this.val = val;

    }

    public void bind(final Symbol sym, final Value val)
        throws SymbolRedeclaredException {
        // all bindings are put off into the outer environment
        // except for redeclaring the loop var -- that's an error
        if (sym.equals(loopSym))
            throw new SymbolRedeclaredException(sym);

        env.bind(sym, val);
    }

    public Value lookup(final Symbol sym)
        throws AmbiguousLookupException {
        if (sym.equals(loopSym))
            return val;
        else
            return env.lookup(sym);
    }

    public boolean contains(final Symbol sym) {
        return (sym.equals(loopSym) || env.contains(sym));
    }

    public String toString() {
        return "LoopEnvironment(\n" + env + "\n)";
    }

    public EnvironmentIterator iterator() {
        return new EnvironmentIterator () {
                
                private boolean m_didValue = false;
                private final Value m_Val = val;
                private final Symbol m_Name = loopSym;
                private final EnvironmentIterator m_EnvIter = env.iterator();

                public final boolean hasNext() {
                    return ( ( ! m_didValue ) || ( m_EnvIter.hasNext() ) ); 
                }

                public final EnvironmentEntry next() {
                    if ( ! m_didValue ) { 
                        m_didValue = true;
                        return new EnvironmentEntry(m_Name, m_Val);
                    }
                    else {
                        return m_EnvIter.next();
                    }
                }
            };
    }

    public EnvironmentEntryIterator entryIterator() {
        return new EnvironmentEntryIterator () {
                
                private boolean m_didEntry = false;
                private final EnvironmentEntry m_Entry =
                    new EnvironmentEntry(loopSym, val);
                private final EnvironmentEntryIterator m_EnvEntryIter =
                    env.entryIterator();

                public final boolean hasNext() {
                    return ( ( ! m_didEntry ) ||
                             ( m_EnvEntryIter.hasNext() ) ); 
                }

                public final EnvironmentEntry next() {
                    if ( ! m_didEntry ) { 
                        m_didEntry = true;
                        return m_Entry;
                    }
                    else {
                        return m_EnvEntryIter.next();
                    }
                }
            };
    }
}
