/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.recalc;

import java.util.HashSet;

/**
 * This class defines what unary operators are.
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public abstract class AbstractUnaryOp implements ExpressionInterface {
    /** Expression. **/
    protected final ExpressionInterface expression;

    /** Operator symbol. **/
    private final String symbol;

    /**
     * Construct Binary Operation
     **/
    public AbstractUnaryOp(ExpressionInterface expression,
                           String symbol)
    {
        this.expression = expression;
        this.symbol = symbol;
    }

    /**
     * Evaluate the expression
     * @return result of expression
     **/
    public abstract double eval();

    /**
     * Get list of varaibles this expression depends on
     * @return list of references to variables used in this expression
     **/
    public HashSet getDependant() {
        return expression.getDependant();
    }

    /**
     * Object to String
     * @return String describing expression
     **/
    public String toString() {
        return symbol + expression.toString();
    }
}
