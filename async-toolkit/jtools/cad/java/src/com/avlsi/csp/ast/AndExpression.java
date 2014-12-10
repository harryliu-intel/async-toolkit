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
 * Bitwise and expression, represents <code> a &amp; b </code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class AndExpression extends AbstractBinaryExpression {

    /**
     * Class constructor.
     *
     * @param left left child expression, not null
     * @param right right child expression, not null
     **/
    public AndExpression(final ExpressionInterface left,
            final ExpressionInterface right) {
        super(left, right);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitAndExpression(this);
    }
    public String getOperator() { return "&"; }

}
