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
 * Abstract base class for binary expressions.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public abstract class AbstractBinaryExpression 
    extends AbstractASTNode implements ExpressionInterface {
    /** Left child expression.  May not be null. **/
    private final ExpressionInterface left;
    /** Right child expression.  May not be null. **/
    private final ExpressionInterface right;

    /**
     * Class constructor.
     *
     * @param left  Left child expression, not null.
     * @param right Rigth child expression, not null.
     **/
    public AbstractBinaryExpression(final ExpressionInterface left,
            final ExpressionInterface right) {
        this.left = left;
        this.right = right;
    }

    /**
     * Returns the left child expression.
     *
     * @return left child expression, may not be null.
     **/
    public ExpressionInterface getLeft() {
        return left;
    }

    /**
     * Returns the left child expression.
     *
     * @return right child expression, may not be null.
     **/
    public ExpressionInterface getRight() {
        return right;
    }

    /**
     * Returns the operator.
     *
     * @return the operator as it would have been written in CSP.
     **/
    public String getOperator() { return "binary operator"; }
}
