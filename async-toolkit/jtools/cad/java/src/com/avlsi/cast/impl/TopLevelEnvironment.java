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
 * Environment for top level of file.  This consists of a local environment,
 * and the environments imported from elsewhere.  Lookups first occur
 * in the local environment, then are passed on to the imported
 * environments.  Declarations happen in the local environment.
 * <p>
 * In the future, there will be a distinction private symbols that are
 * not exported.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class TopLevelEnvironment implements Environment {
    private final ImportEnvironment importEnv;
    private final BlockEnvironment  topEnv;

    public TopLevelEnvironment(final ImportEnvironment importEnv) {
        this.importEnv = importEnv;
        // Disallow "import foo.bar.FOO;  define FOO()() {...}"
        this.topEnv = new FixedBlockEnvironment(NullEnvironment.getInstance());
    }


    /**
     * Prevents rebinding of a symbol which has been imported.
     **/
    public void bind(final Symbol sym, final Value val)
        throws SymbolRedeclaredException {
        if (importEnv.contains(sym))
            throw new SymbolRedeclaredException(sym);
        topEnv.bind(sym, val);
    }

    public Value lookup(final Symbol sym)
        throws AmbiguousLookupException {
        final Value v = topEnv.lookup(sym);
        if (v != null)
            return v;
        return importEnv.lookup(sym);
    }

    public boolean contains(final Symbol sym) {
        return (topEnv.contains(sym) || importEnv.contains(sym));
    }

    public Environment getExportedEnvironment() {
        return new UnbindableEnvironment(topEnv);
    }

    /**
     * Environment that does not allow binds to occur.
     **/
    private static final class UnbindableEnvironment implements Environment {
        private final Environment env;

        public UnbindableEnvironment(final Environment env) {
            if (env == null)
                throw new IllegalArgumentException("env was null");

            this.env = env;
        }

        /** @throws InvalidBindException  Always. **/
        public void bind(final Symbol sym, final Value val) {
            throw new InvalidBindException("can't bind to this env");
        }

        public Value lookup(final Symbol sym)
            throws AmbiguousLookupException {
            return env.lookup(sym);
        }

        public boolean contains(final Symbol sym) {
            return env.contains(sym);
        }

        public String toString() {
            return "UnbindableEnvironment(" + env.toString() + ")";
        }

        public EnvironmentIterator iterator() {
            return env.iterator();
        }

        public EnvironmentEntryIterator entryIterator() {
            return env.entryIterator();
        }

    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();

        sb.append("TopLevelEnvironment(\n");
        sb.append(topEnv.toString());
        sb.append("\nImportEnv:\n");
        sb.append(importEnv.toString());
        sb.append(")\n");

        return sb.toString();
    }

    
    public EnvironmentIterator iterator() {
        return new ChainedEnvironmentIterator( topEnv, importEnv );
    }

    public EnvironmentEntryIterator entryIterator() {
        return new ChainedEnvironmentEntryIterator( topEnv, importEnv );
    }
}
