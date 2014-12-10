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
 * Recieve statement, <code> X ? x </code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class ReceiveStatement extends AbstractChannelStatement {

    /**
     * Class constructor.
     *
     * @param channelExpr  expression for channel to receive from,
     *     not null
     * @param rhs expression for variable to receive into.  
     *     May be null.
     **/
    public ReceiveStatement(final ExpressionInterface channelExpr,
            final ExpressionInterface rhs) {
        super(channelExpr, rhs);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitReceiveStatement(this);
    }
}
