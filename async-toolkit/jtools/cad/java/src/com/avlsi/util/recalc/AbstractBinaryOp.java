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
 * This class defines what binary operators are.
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public abstract class AbstractBinaryOp implements ExpressionInterface {
    /** Left side value. **/
    protected final ExpressionInterface left;

    /** Right side value. **/
    protected final ExpressionInterface right;

    /** Operator symbol. **/
    private final String symbol;

    /**
     * Construct Binary Operation
     **/
    public AbstractBinaryOp(ExpressionInterface left,
                            ExpressionInterface right,
                            String symbol)
    {
        this.left = left;
        this.right = right;
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
        HashSet dependants = left.getDependant();
        dependants.addAll(right.getDependant());

        return dependants;
    }

    /**
     * Object to String
     * @return String describing expression
     **/
    public String toString() {
        return "(" + left.toString() + symbol + right.toString() + ")";
    }
}
