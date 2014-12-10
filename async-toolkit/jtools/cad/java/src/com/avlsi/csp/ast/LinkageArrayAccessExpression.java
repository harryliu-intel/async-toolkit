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
public class LinkageArrayAccessExpression
    extends AbstractASTNode
    implements LinkageExpressionInterface {

    private final LinkageExpressionInterface arrayExpr;

    private final ExpressionInterface indexExpr;

    public LinkageArrayAccessExpression(
            final LinkageExpressionInterface arrayExpr,
            final ExpressionInterface indexExpr) {
        this.arrayExpr = arrayExpr;
        this.indexExpr = indexExpr;
    }

    public LinkageExpressionInterface getArrayExpression() {
        return arrayExpr;
    }

    public ExpressionInterface getIndexExpression() {
        return indexExpr;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitLinkageArrayAccessExpression(this);
    }
}
