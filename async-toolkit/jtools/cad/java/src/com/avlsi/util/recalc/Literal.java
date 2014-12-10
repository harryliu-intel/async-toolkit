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
 * This class defines literal values for the recalc engine
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class Literal implements ExpressionInterface {
    /** Value of literal. **/
    private double value;

    /** Construct literal. **/
    public Literal(double value) {
        this.value = value;
    }

    /**
     * Get list of varaibles this expression depends on.
     * @return Empty collection
     **/
    public HashSet getDependant() {
        return new HashSet();
    }

    /**
     * Evaluate the variable's value
     * @return value
     **/
    public double eval() {
        return value;
    }

    /**
     * Synonym for eval
     * @return double
     **/
    public double get() {
        return eval();
    }

    /**
     * Convert variable to string.
     **/
    public String toString() {
        return Double.toString(value);
    }
}
