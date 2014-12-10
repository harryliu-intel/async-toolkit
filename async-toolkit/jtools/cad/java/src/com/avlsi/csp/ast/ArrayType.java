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
 * Class for CSP Array types
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class ArrayType extends Type{
    private final Range range;
    private Type elementType;

    public ArrayType (final Range range, final Type elementType) {
        this.range = range;
        this.elementType = elementType;
    }

    public int dimension() {
        return elementType.dimension() + 1;
    }

    public Range getRange () {
        return range;
    }

    public Type getElementType () {
        return elementType;
    }

    public void setElementType (final Type elementType) {
        this.elementType = elementType;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitArrayType(this);
    }

    public String toString() {
        final String r;
        final ExpressionInterface min = range.getMinExpression();
        final ExpressionInterface max = range.getMaxExpression();
        if (min instanceof IntegerExpression &&
            max instanceof IntegerExpression) {
            r = min.toString() + ".." + max.toString();
        } else {
            r = "";
        }
        return elementType.toString() + "[" + r + "]";
    }
}
