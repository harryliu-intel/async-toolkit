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
public class LinkageStructureAccessExpression
    extends AbstractASTNode
    implements LinkageExpressionInterface {

    private final LinkageExpressionInterface structExpr;

    private final String fieldName;

    public LinkageStructureAccessExpression(
            final LinkageExpressionInterface structExpr,
            final String fieldName) {
        this.structExpr = structExpr;
        this.fieldName = fieldName;
    }

    public LinkageExpressionInterface getStructureExpression() {
        return structExpr;
    }

    public String getFieldName() {
        return fieldName;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitLinkageStructureAccessExpression(this);
    }
}
