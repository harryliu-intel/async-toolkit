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
 * Structure expression, represents <code> a.f </code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class StructureAccessExpression 
    extends AbstractASTNode
    implements ExpressionInterface {

    /** Expression whose field is to be accessed, not null. **/
    private final ExpressionInterface structExpr;

    /** Name of field to be accessed, not null.  **/
    private final String fieldName;

    /**
     * Class constructor.
     *
     * @param structExpr  Expression whose field is to be accessed, not null.
     * @param fieldName   Name of field to be accessed, not null.
     **/
    public StructureAccessExpression(final ExpressionInterface structExpr,
            final String fieldName) {
        this.structExpr = structExpr;
        this.fieldName = fieldName;
    }

    /**
     * Returns expression whose field is to be accessed.
     *
     * @return  expression whose field is to be accessed, not null
     **/
    public ExpressionInterface getStructureExpression() {
        return structExpr;
    }

    /**
     * Returns name of field to be accessed.
     *
     * @return   name of field to be accessed, not null
     **/
    public String getFieldName() {
        return fieldName;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitStructureAccessExpression(this);
    }
}
