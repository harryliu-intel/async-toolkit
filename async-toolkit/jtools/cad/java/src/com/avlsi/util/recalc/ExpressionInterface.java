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
 * This interface defines what an expression is.
 * Variables, constants, and functions implement this
 * interface.  Functions (and often variables) tend
 * to recursively rely on other expressions.
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public interface ExpressionInterface {
    /**
     * Evaluate the expression
     * @return result of expression
     **/
    double eval();

    /**
     * Get list of varaibles this expression depends on
     * @return list of references to variables used in this expression
     **/
    HashSet getDependant();

    /**
     * Object to String
     * @return String describing expression
     **/
    String toString();
}
