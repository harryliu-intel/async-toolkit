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
 * Not expression, represents <code> ~x </code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class NotExpression extends AbstractUnaryExpression {

    /**
     * Class constructor.
     *
     * @param left left child expression, not null
     * @param right right child expression, not null
     **/
    public NotExpression(final ExpressionInterface expr) {
        super(expr);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitNotExpression(this);
    }
}
