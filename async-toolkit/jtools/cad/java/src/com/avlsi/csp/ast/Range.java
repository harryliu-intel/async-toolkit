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
 * Range class, represents <code> A..B </code>.  The range represents
 * the sequence of integers <code>A &lt;= i &amp;&amp; i &lt;= B</code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class Range extends AbstractASTNode {

    /** Expression for minimum value of range.  May not be null.  **/
    private final ExpressionInterface minExpr;

    /** Expression for maximum value of range.  May not be null.  **/
    private final ExpressionInterface maxExpr;

    /**
     * Class constructor.
     *
     * @param minExpr  expression for minimum value of range
     * @param maxExpr  expression for maximum value of range
     **/
    public Range(final ExpressionInterface minExpr,
            final ExpressionInterface maxExpr) {
        this.minExpr = minExpr;
        this.maxExpr = maxExpr;
    }

    /**
     * Returns expression for minimum value of range.
     *
     * @return expression for minimum value of range, not null
     **/
    public ExpressionInterface getMinExpression() {
        return minExpr;
    }

    /**
     * Returns expression for maximum value of range.
     *
     * @return expression for maximum value of range, not null
     **/
    public ExpressionInterface getMaxExpression() {
        return maxExpr;
    }
}
