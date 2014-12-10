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
 * Abstract base class for unary expressions.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public abstract class AbstractUnaryExpression
    extends AbstractASTNode
    implements ExpressionInterface {
    private final ExpressionInterface expr;

    /**
     * Class constructor.
     *
     * @param expr child expression, may not be null
     **/
    public AbstractUnaryExpression(final ExpressionInterface expr) {
        this.expr = expr;
    }

    /**
     * Returns the child expression.
     *
     * @return child expression, may not be null
     **/
    public ExpressionInterface getExpression() {
        return expr;
    }
}
