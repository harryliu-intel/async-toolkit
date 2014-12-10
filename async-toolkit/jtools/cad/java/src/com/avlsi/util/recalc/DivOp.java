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
 * This class defines the division operator.
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class DivOp extends AbstractBinaryOp {
    /**
     * Construct Binary Operation
     **/
    public DivOp(ExpressionInterface left,
                 ExpressionInterface right)
    {
        super(left, right, "/");
    }

    /**
     * Evaluate the expression
     * @return result of expression
     **/
    public double eval() {
        return left.eval() / right.eval();
    }
}
