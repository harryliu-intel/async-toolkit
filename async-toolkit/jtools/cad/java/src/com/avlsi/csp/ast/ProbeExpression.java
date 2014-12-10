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
 * Probe expression, represents <code> #X </code>.
 * Todo: add support for <code>#X, Y, Z: expr</code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class ProbeExpression extends AbstractChannelExpression {
    /**
     * Class constructor.
     *
     * @param channelExpr  expression for channel to probe, not null
     **/
    public ProbeExpression(final ExpressionInterface channelExpr) {
        super(channelExpr);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitProbeExpression(this);
    }
}
