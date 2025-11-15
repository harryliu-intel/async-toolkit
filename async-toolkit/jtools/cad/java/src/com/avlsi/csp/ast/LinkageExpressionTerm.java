// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

/**
 *
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class LinkageExpressionTerm 
    extends AbstractASTNode
    implements LinkageTermInterface {

    private final LinkageExpressionInterface expr;

    private final boolean isInverted;

    public LinkageExpressionTerm(final LinkageExpressionInterface expr,
            final boolean isInverted) {
        this.expr = expr;
        this.isInverted = isInverted;
    }

    public LinkageExpressionInterface getExpression() {
        return expr;
    }

    public boolean isInverted() {
        return isInverted;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitLinkageExpressionTerm(this);
    }
}
