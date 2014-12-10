/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.ast;

/**
 * Abstract base class for CSP types
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public abstract class Type 
    extends AbstractASTNode implements Cloneable {

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public abstract void accept(VisitorInterface v) throws VisitorException;

    /**
     * Indicates dimension of the type.  Scalars have dimension 0, arrays
     * N, where N > 0.
     **/
    public abstract int dimension();

    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    public static Type clone(final Type t) {
        try {
            return (Type) t.clone();
        } catch (CloneNotSupportedException e) {
            throw new AssertionError("Cannot clone " + t);
        }
    }
}
