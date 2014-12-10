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
 * Multiply expression, represents <code> a * b </code>
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class MultiplyExpression extends AbstractBinaryExpression {

    /**
     * Class constructor.
     *
     * @param left left child expression, not null
     * @param right right child expression, not null
     **/
    public MultiplyExpression(final ExpressionInterface left,
            final ExpressionInterface right) {
        super(left, right);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitMultiplyExpression(this);
    }

    public String getOperator() { return "*"; }
}
