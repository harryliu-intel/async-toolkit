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
 * Divide expression, represents <code> a / b </code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class DivideExpression extends AbstractBinaryExpression {

    /**
     * Class constructor.
     *
     * @param left left child expression, not null
     * @param right right child expression, not null
     **/
    public DivideExpression(final ExpressionInterface left,
            final ExpressionInterface right) {
        super(left, right);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitDivideExpression(this);
    }

    public String getOperator() { return "/"; }
}
