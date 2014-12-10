/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/csp/ast/BooleanType.java#7 $
 */

package com.avlsi.csp.ast;

/**
 * Class for CSP Boolean type
 *
 * @author David Hilvert
 * @version $Revision: #7 $ $Date: 2002/01/29 $
 **/
public class BooleanType extends Type {
    private final boolean is_const;
    public BooleanType () {
        this(false);
    }

    public BooleanType (boolean is_const) {
        this.is_const = is_const;
    }

    /**
     * Indicates dimension of the type.  Scalars have dimension 0, arrays
     * N, where N > 0.
     **/
    public int dimension() {
        // a boolean is a scalar CSP type, so its dimension is 0.
        return 0;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitBooleanType(this);
    }

    public boolean isConst() {
        return is_const;
    }

    public String toString() {
        return "bool";
    }
}
