/*
 * Copyright 2006 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

/**
 * Conditional OR expression, represents <code> a || b </code>.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class ConditionalOrExpression extends AbstractBinaryExpression {

    /**
     * Class constructor.
     *
     * @param left left child expression, not null
     * @param right right child expression, not null
     **/
    public ConditionalOrExpression(final ExpressionInterface left,
                                   final ExpressionInterface right) {
        super(left, right);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitConditionalOrExpression(this);
    }

    public String getOperator() { return "||"; }
}
