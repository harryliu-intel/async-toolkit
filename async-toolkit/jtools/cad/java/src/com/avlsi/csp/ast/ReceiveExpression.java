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
 * Receive expression, represents <code> X? </code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class ReceiveExpression extends AbstractChannelExpression {
    /**
     * Class constructor.
     *
     * @param channelExpr  expression for channel to receive on, not null
     **/
    public ReceiveExpression(final ExpressionInterface channelExpr) {
        super(channelExpr);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitReceiveExpression(this);
    }
}
