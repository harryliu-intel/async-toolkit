/*
 * Copyright 2011 Intel Corporation. All Rights Reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

/**
 * Bitwise XOR expression, represents <code> a ^ b </code>.
 *
 * @author Kevin Duncklee
 * @version $Revision$ $Date$
 **/
public class XorExpression extends AbstractBinaryExpression {

    /**
     * Class constructor.
     *
     * @param left left child expression, not null
     * @param right right child expression, not null
     **/
    public XorExpression(final ExpressionInterface left,
            final ExpressionInterface right) {
        super(left, right);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitXorExpression(this);
    }

    public String getOperator() { return "^"; }
}
