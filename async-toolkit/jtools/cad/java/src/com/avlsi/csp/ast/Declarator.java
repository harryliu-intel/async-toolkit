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
 * Declarator, modified from IdentifierExpression
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class Declarator extends AbstractASTNode {
    /** No direction specified **/
    public static final int NONE  = 0;

    /** Input direction, i.e., "-" **/
    public static final int IN    = 1;

    /** Output direction, i.e., "+" **/
    public static final int OUT   = 2;

    /** Bidirectional, i.e., "+-" or "-+" **/
    public static final int INOUT = 3;

    /** Declarator name, not null **/
    private final IdentifierExpression ident;

    /** Declarator type fragment **/
    private Type typeFragment;

    /** Initializer **/
    private ExpressionInterface init;

    /**
     * Directionality of an argument.  Undefined for declarators not in a
     * formal parameter list.
     **/
    private final int direction;

    public Declarator(final IdentifierExpression ident, final Type typeFragment,
                      final ExpressionInterface init) {
        this(ident, typeFragment, init, NONE);
    }

    /**
     * Class constructor.
     *
     * @param ident  Identifier of declarator
     * @param typeFragment Type fragment associated with declarator
     **/
    public Declarator(final IdentifierExpression ident, final Type typeFragment,
                      final ExpressionInterface init, final int direction){
        this.ident = ident;
        this.typeFragment = typeFragment;
        this.init = init;
        this.direction = direction;
    }

    /**
     * Returns identifier
     *
     * @return identifier, not null
     **/
    public IdentifierExpression getIdentifier() {
        return ident;
    }

    /**
     * Returns type fragment
     *
     * @return type fragment, may be null
     **/
    public Type getTypeFragment() {
        return typeFragment;
    }

    /**
     * Sets type fragment
     *
     * @param typeFragment Type fragment to associate with declarator
     **/
    public void setTypeFragment(final Type typeFragment) {
        this.typeFragment = typeFragment;
    }

    /** 
     * Returns initializer
     * 
     * @return initializer, may be null
     **/
    public ExpressionInterface getInitializer() {
        return init;
    }

    /**
     * Sets initializer
     *
     * @param init Initializer expression
     **/
    public void setInitializer(ExpressionInterface init) {
        this.init = init;
    }

    /**
     * Returns directionality
     *
     * @return directionality
     **/
    public int getDirection() {
        return direction;
    }
}
