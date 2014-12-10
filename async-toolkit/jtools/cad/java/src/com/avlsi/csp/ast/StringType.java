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
 * Class for CSP string type
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class StringType extends Type {
    boolean is_const = false;

    public StringType () {
    }

    public StringType (boolean is_const) {
        this.is_const = is_const;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitStringType(this);
    }

    /**
     * Indicates dimension of the type.  Scalars have dimension 0, arrays
     * N, where N > 0.
     **/
    public int dimension() {
        // A string is a scalar CSP type, so its dimension is 0.
        return 0;
    }

    /**
     * Indicates whether this string type is a constant integer type.
     **/
    public boolean isConst() {
        return is_const;
    }

    public String toString() {
        return "string";
    }
}
