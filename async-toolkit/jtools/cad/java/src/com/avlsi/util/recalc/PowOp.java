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
 * This class defines the power operator.
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class PowOp extends AbstractBinaryOp {
    /**
     * Construct Binary Operation
     **/
    public PowOp(ExpressionInterface left,
                 ExpressionInterface right)
    {
        super(left, right, "**");
    }

    /**
     * Evaluate the expression
     * @return result of expression
     **/
    public double eval() {
        return Math.pow(left.eval(), right.eval());
    }
}
