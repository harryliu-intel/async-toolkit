/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

/**
 * Abstract base class for channel related operations.
 * <code> X!x, X?x </code>
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public abstract class AbstractChannelStatement
    extends AbstractASTNode
    implements StatementInterface {

    /** 
     * The expression for the channel to send or receive on.
     * May not be null.
     **/
    protected final ExpressionInterface channelExpr;

    /**
     * The expression for the right hand side of the operation:
     *  the value to send, or the variable to receive into.
     *  May be null for receive expressions.
     **/
    protected final ExpressionInterface rhsExpr;

    /**
     * Class constructor.
     *
     * @param channelExpr  expression for channel on which to operate,
     *     not null
     * @param rhsExpr  expression for right hand side of operation,
     *     may be null for recieve statements
     **/
    public AbstractChannelStatement(final ExpressionInterface channelExpr,
            final ExpressionInterface rhsExpr) {
        this.channelExpr = channelExpr;
        this.rhsExpr = rhsExpr;
    }

    /**
     * Returns channel expression for send or recieve.
     *
     * @return Channel expression to send or recieve on, may not be null.
     **/
    public ExpressionInterface getChannelExpression() {
        return channelExpr;
    }

    /**
     * Returns right hand side expression: value to send, or variable
     * to receive into.  May be null for receive statements.
     *
     * @return  Expression from right hand side, ie <code>x</code>
     * from <code>X?x</code> or <code>X!x</code>.
     **/
    public ExpressionInterface getRightHandSide() {
        return rhsExpr;
    }
}
