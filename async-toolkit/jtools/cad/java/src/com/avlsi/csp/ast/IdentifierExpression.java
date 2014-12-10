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
 * Identifier expression, represents <code> ident </code>.
 * <p> Todo: maybe a string pool for the idents? </p>
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class IdentifierExpression 
    extends AbstractASTNode
    implements ExpressionInterface {

    /** Identifier name, not null **/
    private final String ident;

    /**
     * Class constructor.
     *
     * @param ident  Name of identifier, not null
     **/
    public IdentifierExpression(final String ident) {
        this.ident = ident;
    }

    /**
     * Returns identifier name
     *
     * @return identifier name, not null
     **/
    public String getIdentifier() {
        return ident;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitIdentifierExpression(this);
    }
}
