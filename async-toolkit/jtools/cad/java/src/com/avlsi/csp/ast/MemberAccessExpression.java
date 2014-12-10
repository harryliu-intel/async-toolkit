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
 * Member access expression, represents <code> a::f </code>.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class MemberAccessExpression 
    extends AbstractASTNode
    implements ExpressionInterface {

    /** Expression whose field is to be accessed, not null. **/
    private final ExpressionInterface structExpr;

    /** Name of member to be accessed, not null.  **/
    private final String memberName;

    /**
     * Class constructor.
     *
     * @param structExpr  Expression whose field is to be accessed, not null.
     * @param memberName  Name of member to be accessed, not null.
     **/
    public MemberAccessExpression(final ExpressionInterface structExpr,
                                  final String memberName) {
        this.structExpr = structExpr;
        this.memberName = memberName;
    }

    /**
     * Returns expression whose member is to be accessed.
     *
     * @return  expression whose member is to be accessed, not null
     **/
    public ExpressionInterface getStructureExpression() {
        return structExpr;
    }

    /**
     * Returns name of member to be accessed.
     *
     * @return   name of member to be accessed, not null
     **/
    public String getMemberName() {
        return memberName;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitMemberAccessExpression(this);
    }
}
