/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cast.impl;

/**
 * Symbol class, just a string, but could, in the future, use 
 * a WeakHashSet of these guys.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Symbol {
    private final String s;

    private Symbol(final String s) {
        this.s = s;
    }

    /** Return a non-global form of this Symbol.  Doesn't change original. **/
    public Symbol stripBang() {
        if (s.endsWith("!"))
            return new Symbol(s.substring(0, s.length()-1));
        else
            return this;
    }

    /** Does this Symbol represent a global name (ends in !)? **/
    public boolean isGlobal() {
        return s.endsWith("!");
    }

    public static Symbol create(final String s) {
        return new Symbol(s);
    }

    public String getString() {
        return s;
    }

    /**
     * Is the module name part of the symbol, or just the type name?
     **/
    public boolean fullyQualified() {
        return (s.indexOf('.') != -1);
    }

    public int hashCode() {
        return s.hashCode();
    }

    public boolean equals(Object o) {
        if (o instanceof Symbol)
            return equals((Symbol) o);
        else
            return false;
    }

    public boolean equals(Symbol sym) {
        return s.equals(sym.s);
    }

    public String toString() {
        return "Symbol(" + s + ")";
    }
}
