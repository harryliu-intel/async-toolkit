/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

/**
 * A class that turns an expression into a statement.  It is useful for
 * evaluating an expression for side effect only.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class ExpressionStatement
    extends AbstractASTNode
    implements StatementInterface {

    /** 
     * The expression to evaluate. May not be null.
     **/
    protected final ExpressionInterface expr;

    /**
     * Class constructor.
     *
     * @param expr  expression to evaluate for side effect, may not be null
     **/
    public ExpressionStatement(final ExpressionInterface expr) {
        this.expr = expr;
    }

    /**
     * Returns expression to be evaluated for side effect
     *
     * @return expression to be evaluated, may not be null.
     **/
    public ExpressionInterface getExpression() {
        return expr;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitExpressionStatement(this);
    }
}
