/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */



package com.avlsi.cast.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.NoSuchElementException;

import com.avlsi.cast.impl.Value;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.EnvironmentIterator;


/**
 * Looks up the symbol in all of the imported environments;
 * an exception will be thrown unless it is found in exactly one
 * environment.  Binding to imported environments is not allowed,
 * that has already been done.
 * <p>
 * @todo jmr This will need modification when you can "import foo as bar".
 **/
public final class ImportEnvironment implements Environment {
    private final ArrayList envs;

    public ImportEnvironment() {
        envs = new ArrayList();
    }

    public ImportEnvironment(final ImportEnvironment defaultImportEnv) {
        this();
        envs.addAll(defaultImportEnv.envs);
    }

    /**
     * Looks up the symbol in all of the imported environments;
     * an exception will be thrown unless it is found in exactly one
     * environment.
     **/
    public Value lookup(final Symbol sym)
        throws AmbiguousLookupException
    {
        final String env1 = "<unknown>";
        String env2 = "<unknown>";

        Value val = null;

        for (int iEnv = 0; iEnv < envs.size(); ++iEnv) {
            final Value v = ((Environment) envs.get(iEnv)).lookup(sym);

            if (v == null)
                continue;

            // if we have already found a value, it is an error
            if (val != null)
                throw new AmbiguousLookupException(sym, env1, env2);
            else
                val = v;
        }

        return val;
    }

    /**
     * Looks up the symbol in all of the imported environments;
     **/
    public boolean contains(final Symbol sym) {
        for (int iEnv = 0; iEnv < envs.size(); ++iEnv) {
            if (((Environment) envs.get(iEnv)).contains(sym))
                return true;
        }
        return false;
    }

    /** @throws InvalidBindException  Always.  **/
    public void bind(final Symbol sym, final Value val) {
        throw new InvalidBindException("can't bind to import env");
    }

    /**
     * Adds the environment to the list of imported environments,
     * if it is not already there (this prevents spurious
     * AmbiguousLookupExceptions, and allows multiple importation
     * in the same file).
     **/
    public void addEnvironment(final Environment env) {
        if (!envs.contains(env))
            envs.add(env);
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("ImportEnvironment(\n");
        for (Iterator envIterator = envs.iterator();
             envIterator.hasNext();
             ) {
            sb.append(envIterator.next().toString());
            sb.append("\n");
        }
        sb.append(")");
        return sb.toString();
    }

    public EnvironmentIterator iterator() {
        return new EnvironmentIterator () {
                
                private final Iterator m_EnvsIter = envs.iterator();
                private EnvironmentIterator m_CurrEnvIter = null;
                private boolean noNext = false;
                /**
                   Updates m_CurrEnvIter to be an iterator into the next non-empty
                   environment in the collection iterated by m_EnvsIter.
                 */
                private final void updateCurrIter() {
                    if ( m_CurrEnvIter == null ) {
                        final Environment nextEnv = ( Environment ) m_EnvsIter.next();
                        m_CurrEnvIter = nextEnv.iterator();
                    }

                    while ( ! ( m_CurrEnvIter.hasNext() ) ) {
                        final Environment nextEnv = ( Environment ) m_EnvsIter.next();
                        m_CurrEnvIter = nextEnv.iterator();
                    }
                }

                public final boolean hasNext() {
                    if ( ! noNext ) {
                        try {
                            updateCurrIter();
                            return true;
                        }
                        catch( NoSuchElementException e ) {
                            noNext = true;
                            return false;
                        }
                    }
                    else {
                        return false;
                    }
                }

                public final EnvironmentEntry next() {
                    updateCurrIter();
                    return m_CurrEnvIter.next();
                }
            };
    }

    public EnvironmentEntryIterator entryIterator() {
        return new EnvironmentEntryIterator () {
                
                private final Iterator m_EnvsIter = envs.iterator();
                private EnvironmentEntryIterator m_CurrEnvEntryIter = null;
                private boolean noNext = false;
                /**
                   Updates m_CurrEnvEntryIter to be an iterator into the next non-empty
                   environment in the collection iterated by m_EnvsIter.
                 */
                private final void updateCurrIter() {
                    if ( m_CurrEnvEntryIter == null ) {
                        final Environment nextEnv = ( Environment ) m_EnvsIter.next();
                        m_CurrEnvEntryIter = nextEnv.entryIterator();
                    }

                    while ( ! ( m_CurrEnvEntryIter.hasNext() ) ) {
                        final Environment nextEnv = ( Environment ) m_EnvsIter.next();
                        m_CurrEnvEntryIter = nextEnv.entryIterator();
                    }
                }

                public final boolean hasNext() {
                    if ( ! noNext ) {
                        try {
                            updateCurrIter();
                            return true;
                        }
                        catch( NoSuchElementException e ) {
                            noNext = true;
                            return false;
                        }
                    }
                    else {
                        return false;
                    }
                }

                public final EnvironmentEntry next() {
                    updateCurrIter();
                    return m_CurrEnvEntryIter.next();
                }
            };
    }
}

