// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
 * Peek expression, represents <code> #X? </code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class PeekExpression extends AbstractChannelExpression {
    /**
     * Class constructor.
     *
     * @param channelExpr  expression for channel to peek, not null
     **/
    public PeekExpression(final ExpressionInterface channelExpr) {
        super(channelExpr);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitPeekExpression(this);
    }
}
