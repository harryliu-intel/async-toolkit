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
 * Bit range expression, represents <code> a[n:m] </code>.  The range represents
 * the bits <code>m &lt;= i &amp;&amp; i &lt;= n</code>.  Modified from
 * class Range, and modeled after ArrayAccessExpression.
 *
 * @author David Hilvert 
 * @version $Revision$ $Date$
 **/
public class BitRangeExpression
    extends AbstractASTNode
    implements ExpressionInterface {

    /** Expression that we are extracting from.  May not be null.  **/
    private final ExpressionInterface bitsExpr;

    /** Expression for minimum value of range.  May be null. **/
    private final ExpressionInterface minExpr;

    /** Expression for maximum value of range.  May not be null.  **/
    private final ExpressionInterface maxExpr;

    /**
     * Class constructor.
     *
     * @param bitsExpr expression to be extracted from, not null
     * @param minExpr  expression for minimum value of range, may be null
     * @param maxExpr  expression for maximum value of range, not null
     **/
    public BitRangeExpression(final ExpressionInterface bitsExpr,
            final ExpressionInterface minExpr,
            final ExpressionInterface maxExpr) {
        this.bitsExpr = bitsExpr;
        this.minExpr = minExpr;
        this.maxExpr = maxExpr;
    }

    /**
     * Returns the expression to be extracted from.
     *
     * @return expression to be extracted from, not null
     **/
    public ExpressionInterface getBitsExpression() {
        return bitsExpr;
    }

    /**
     * Returns expression for minimum value of range.
     *
     * @return expression for minimum value of range, may be null
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

    /**
     * Returns an expression representing the current value of the specified 
     * bits.  This can only be used as an rvalue.
     * 
     * @return expression for the current value of the bits, not null.
     * @throws IllegalStateException if the returned expression would evaluate
     * the index expression more than once
     **/
    public ExpressionInterface getValueExpression() {
        // Translate:
        //  x[n:m] --> ((x & ((1 << (n + 1)) - 1)) >> m)

        if (minExpr == null) throw new IllegalStateException();

        ExpressionInterface one = new IntegerExpression ("1", 10);
        ExpressionInterface result = 
            (ExpressionInterface) new RightShiftExpression (
                (ExpressionInterface) new AndExpression (
                    bitsExpr, 
                    (ExpressionInterface) new SubtractExpression (
                        (ExpressionInterface) new LeftShiftExpression (
                            one,
                            (ExpressionInterface) new AddExpression (
                                maxExpr, one)),
                        one)),
                minExpr);

        return result;
    }

    /**
     * Returns an expression representing the ultimate assigned value in the
     * statement <code>x[n:m] := expr</code>.  This can only be used as an
     * rvalue.
     *
     * @param expr  right-hand-side of assignment under consideration
     * @return expression representing the assigned value
     * @throws IllegalStateException if the returned expression would evaluate
     * the index expression more than once
     **/
    public ExpressionInterface getAssignedExpression(ExpressionInterface expr){
        // Translate:
        //  x[n:m] := expr 
        //      --> ((x & ~((1 << (n + 1)) - (1 << m))) 
        //           | ((expr << m) & ((1 << (n + 1)) - (1 << m)))) 
        if (minExpr == null) throw new IllegalStateException();

        ExpressionInterface one = new IntegerExpression ("1", 10);
        ExpressionInterface mask = new SubtractExpression (
            (ExpressionInterface) new LeftShiftExpression (
                one,
                (ExpressionInterface) new AddExpression (
                    maxExpr, one)),
            (ExpressionInterface) new LeftShiftExpression (
                one, minExpr));
        ExpressionInterface result = 
            (ExpressionInterface) new OrExpression (
                (ExpressionInterface) new AndExpression (
                    bitsExpr,
                    (ExpressionInterface) new NotExpression (mask)),
                (ExpressionInterface) new AndExpression (
                    (ExpressionInterface) new LeftShiftExpression (
                        expr,
                        minExpr),
                    mask));
        return result;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitBitRangeExpression(this);
    }
}
