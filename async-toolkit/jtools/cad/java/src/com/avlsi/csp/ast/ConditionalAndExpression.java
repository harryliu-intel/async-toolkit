// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2006 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

/**
 * Conditional and expression, represents <code> a &amp;&amp; b </code>.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class ConditionalAndExpression extends AbstractBinaryExpression {

    /**
     * Class constructor.
     *
     * @param left left child expression, not null
     * @param right right child expression, not null
     **/
    public ConditionalAndExpression(final ExpressionInterface left,
                                    final ExpressionInterface right) {
        super(left, right);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitConditionalAndExpression(this);
    }

    public String getOperator() { return "&&"; }
}
