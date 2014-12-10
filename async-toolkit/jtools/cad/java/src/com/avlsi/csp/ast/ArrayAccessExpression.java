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
 * Array expression, represents <code> a[i] </code>.
 * Todo: add support slicing <code>a[x..y]</code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class ArrayAccessExpression
    extends AbstractASTNode
    implements ExpressionInterface {

    /** Expression to be accessed.  May not be null. **/
    private final ExpressionInterface arrayExpr;

    /** Index expression.  May not be null. **/
    private final ExpressionInterface indexExpr;

    /**
     * Class constructor.
     *
     * @param arrayExpr  expression to be access, not null
     * @param indexExpr  index of arrayExpression to return, not null
     **/
    public ArrayAccessExpression(final ExpressionInterface arrayExpr,
            final ExpressionInterface indexExpr) {
        this.arrayExpr = arrayExpr;
        this.indexExpr = indexExpr;
    }

    /**
     * Returns the array expression.
     *
     * @return array expression to be accessed, not null
     **/
    public ExpressionInterface getArrayExpression() {
        return arrayExpr;
    }

    /**
     * Returns the index expression.
     *
     * @return index of array expression to access, not null
     **/
    public ExpressionInterface getIndexExpression() {
        return indexExpr;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitArrayAccessExpression(this);
    }
}
