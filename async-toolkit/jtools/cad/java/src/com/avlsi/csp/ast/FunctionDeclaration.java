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
 * Class describing a function declaration. Ie,
 * <code>function sum(x, y: int):int = sum := x + y;</code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class FunctionDeclaration extends AbstractASTNode {

    /** Name of the function.  May not be null. **/
    private final IdentifierExpression funcName;

    /** List of types and names of formal parameters. May not be null. **/
    private final DeclarationList formals;

    /**
     * Return type of the function, or null if the function doesn't
     * return a value.
     **/
    private final Type returnType;

    /** Function body.  May not be null.  **/
    private final StatementInterface body;

    /**
     * Class constructor.
     *
     * @param funcName  name of declared function, not null
     * @param formals  list of formal parameters, not null
     * @param returnType  return type of function, or null if no return type
     * @param body  body of function, not null
     **/
    public FunctionDeclaration(final String funcName,
            final DeclarationList formals,
            final Type returnType,
            final StatementInterface body) {
        this.funcName = new IdentifierExpression(funcName);
        this.formals = formals;
        this.returnType = returnType;
        this.body = body;
    }

    /**
     * Returns the name of the declared function.
     *
     * @return name of declared function, not null
     **/
    public String getName() {
        return funcName.getIdentifier();
    }

    public IdentifierExpression getNameIdentifier() {
        return funcName;
    }

    /**
     * Returns list of formal parameters.
     *
     * @return formal parameter list, not null
     **/
    public DeclarationList getFormals() {
        return formals;
    }

    /**
     * Returns return type of function.
     *
     * @return return type of function, or null for none
     **/
    public Type getReturnType() {
        return returnType;
    }

    /**
     * Returns body of function.
     *
     * @return body statement of function, not null
     **/
    public StatementInterface getBodyStatement() {
        return body;
    }
}
