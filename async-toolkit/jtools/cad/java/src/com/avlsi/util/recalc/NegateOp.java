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

/**
 * This class defines the negation operator.
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class NegateOp extends AbstractUnaryOp {
    /**
     * Construct Binary Operation
     **/
    public NegateOp(ExpressionInterface expression) {
        super(expression, "-");
    }

    /**
     * Evaluate the expression
     * @return result of expression
     **/
    public double eval() {
        return -expression.eval();
    }
}
