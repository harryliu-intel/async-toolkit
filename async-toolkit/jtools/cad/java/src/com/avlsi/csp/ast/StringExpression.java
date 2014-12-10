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
 * String literals.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class StringExpression
    extends AbstractASTNode
    implements ExpressionInterface {

    /** The string.  Not null. **/
    private final String val;

    /**
     * Class constructor.  
     *
     * @param val The string.  May not be null.
     **/
    public StringExpression(final String val) {
        this.val = val;
    }

    /**
     * Returns the string.
     *
     * @return string, not null
     **/
    public String getValue() {
        return val;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitStringExpression(this);
    }

    public String toString() {
        return val;
    }
}
