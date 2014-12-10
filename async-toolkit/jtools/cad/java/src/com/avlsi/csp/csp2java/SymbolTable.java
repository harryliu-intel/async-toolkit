/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.csp2java;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import com.avlsi.csp.ast.Type;
import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.UnaryFunction;

/**
 * Symbol table for csp to Java translator.  
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
class SymbolTable {
    /** The current environment. **/
    private Environment env = new Environment();

    /**
     * Adds symbol to the current scope's environment, throwing
     * <code>SymbolRedefinedException</code> if the symbol already
     * exists in this scope.
     **/
    public void insert(final String name,
            final Type type, final String targetName)
        throws SymbolRedefinedException {
        env.insert(name, new Attribute(type, targetName));
    }

    /**
     * Adds symbol to the outermost scope's environment, throwing
     * <code>SymbolRedefinedException</code> if the symbol already
     * exists in any scope.
     **/
    public void insertInRootEnvironment(final String name,
            final Type type, final String targetName)
        throws SymbolRedefinedException {
        getRootEnvironment().insert(name, new Attribute(type, targetName));
    }

    /**
     * Looks up <code>name</code> in the symbol table.
     **/
    public Attribute lookup(final String name)
        throws SymbolUndefinedException {
        return env.lookup(name);
    }

    /**
     * Begin a scope, pushing a new environment linked to the old one.
     **/
    public void beginScope() {
        env = new Environment(env);
    }

    /**
     * Pop outermost ennvironment.
     **/
    public void endScope() {
        env = env.getParent();
    }

    /**
     * Returns an iterator of Strings of names in the current scope.
     **/
    public Iterator/*<String>*/ getRootNames() {
        return getRootEnvironment().getLocalNames();
    }

    /**
     * Returns an iterator of Strings of names in the current scope.
     **/
    public Iterator/*<String>*/ getRootAttributes() {
        return getRootEnvironment().getLocalAttributes();
    }

    /**
     * Returns the root environment.
     **/
    private Environment getRootEnvironment() {
        Environment e = env;
        while (e.getParent() != null)
            e = e.getParent();
        return e;
    }

    /**
     * Attributes of a variable:  the type and its name in the
     * target language.
     **/
    public final class Attribute {
        /** CSP type of variable. **/
        private final Type type;
        /** Name for variable in target language. **/
        private final String targetName;

        public Attribute(final Type type, final String targetName) {
            this.type = type;
            this.targetName = targetName;
        }

        /** Returns CSP type of variable. **/
        public Type getType() {
            return type;
        }

        /** Returns name for variable in target language. **/
        public String getTargetName() {
            return targetName;
        }
    }

    /**
     * Chained environments.
     **/
    private final class Environment {
        private final Environment parent;
        private final Map/*<String,Attribute>*/ syms;

        public Environment() {
            this(null);
        }

        public Environment(final Environment parent) {
            this.parent = parent;
            this.syms = new HashMap/*<String,Attribute>*/();
        }

        public Environment getParent() {
            return parent;
        }

        /**
         * Add <code>name</code> to this environment with the
         * specified attributes.
         **/
        public void insert(final String name, final Attribute attr)
            throws SymbolRedefinedException {
            if (syms.get(name) != null)
                throw new SymbolRedefinedException("Symbol redefined: "
                        + name);

            syms.put(name, attr);
        }

        /**
         * Looks up <code>name</code> in the chained environments.
         **/
        public Attribute lookup(final String name)
            throws SymbolUndefinedException {
            final Attribute attr = (Attribute) syms.get(name);

            if (attr != null)
                return attr;
            else if (parent == null)
                throw new SymbolUndefinedException("Symbol undefined: "
                        + name);
            else
                return parent.lookup(name);
        }

        /**
         * Returns an unmodifiable iterator of Strings of names in this scope
         * only.
         **/
        public Iterator/*<String>*/ getLocalNames() {
            return Collections.unmodifiableMap(syms).keySet().iterator();
        }

        /**
         * Returns an unmodifiable iterator of Pairs of Strings of names 
         * from this scope and their attributes.
         **/
        public Iterator/*<Pair<String,Attribute>>*/ getLocalAttributes() {
            return new
                MappingIterator/*<Map.Entry<String,Attribute>,
                                  Pair<String,Attribute>>*/
                    (Collections.unmodifiableMap(syms)
                        .entrySet().iterator(),
                    new UnaryFunction/*<Map.Entry<String,Attribute>>*/() {
                        public Object execute(final Object o) {
                            return Pair.fromMapEntry((Entry) o);
                        }
                    });
        }
    }
}
