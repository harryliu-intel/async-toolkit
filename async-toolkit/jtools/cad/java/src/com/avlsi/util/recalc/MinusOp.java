// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
 * This class defines the minus operator.
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class MinusOp extends AbstractBinaryOp {
    /**
     * Construct Binary Operation
     **/
    public MinusOp(ExpressionInterface left,
                  ExpressionInterface right)
    {
        super(left, right, "-");
    }

    /**
     * Evaluate the expression
     * @return result of expression
     **/
    public double eval() {
        return left.eval() - right.eval();
    }
}
