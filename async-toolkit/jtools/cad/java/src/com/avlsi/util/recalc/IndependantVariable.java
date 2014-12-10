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
 * This class defines independant variables for the recalc engine
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class IndependantVariable extends AbstractVariable {
    /**
     * Construct variable
     * @param name of variable
     * @param value of variable
     **/
    public IndependantVariable(String name, double value) {
        super(name);
        set(value);
    }

    /**
     * Construct anonymous variable
     * @param value of variable
     **/
    public IndependantVariable(double value) {
        this("", value);
    }

    /**
     * Evaluate the variable's value
     * @return value 
     **/
    public double eval() {
        return value;
    }

    /**
     * Set variable's value.
     * @param value to set
     **/
    public void set(double value) {
        invalidate();
        this.value = value;
    }
}
