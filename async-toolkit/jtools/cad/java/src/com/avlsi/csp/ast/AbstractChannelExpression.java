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
 * Abstract base class for channel expressions.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public abstract class AbstractChannelExpression 
    extends AbstractASTNode implements ExpressionInterface {
    /** Channel expression.  May not be null. **/
    protected final ExpressionInterface channelExpr;

    /**
     * Class constructor.
     *
     * @param channelExpr channel expression, not null.
     **/
    public AbstractChannelExpression(final ExpressionInterface channelExpr) {
        this.channelExpr = channelExpr;
    }

    /**
     * Returns channel to be operated on.
     *
     * @return  expression for channel to be operated on, not null
     **/
    public ExpressionInterface getChannelExpression() {
        return channelExpr;
    }
}
