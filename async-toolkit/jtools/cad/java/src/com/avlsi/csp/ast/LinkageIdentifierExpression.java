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
public class LinkageIdentifierExpression
    extends AbstractASTNode
    implements LinkageExpressionInterface {

    private final String ident;

    public LinkageIdentifierExpression(final String ident) {
        this.ident = ident;
    }

    public String getIdentifier() {
        return ident;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitLinkageIdentifierExpression(this);
    }
}
