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
import java.util.Iterator;

/**
 * This class defines dependant variables for the recalc engine
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class DependantVariable extends AbstractVariable {
    /** Expression describing variable calculation. **/
    private final ExpressionInterface expression;

    /** True if the variable is "up to date". **/
    private boolean valid;

    /**
     * Construct variable
     * @param name of variable
     * @param expression of how to evaluate variable
     **/
    public DependantVariable(String name, ExpressionInterface expression) {
        super(name);

        this.expression = expression;
        valid = false;

        // Inform all variables "this" relies on that "this" accesses them
        HashSet dependants = expression.getDependant();
        for (final Iterator i = dependants.iterator(); i.hasNext(); )
            ((AbstractVariable) i.next()).accessor(this);
    }

    /**
     * Construct anonymous variable
     * @param expression of how to evaluate variable
     **/
    public DependantVariable(ExpressionInterface expression) {
        this("", expression);
    }

    /**
     * Evaluate the variable's value, computing it if necessary.
     * @return value
     **/
    public double eval() {
        if (!valid) {
            value = expression.eval();
            valid = true;
        }

        return value;
    }

    /** Declare this variable and its accessors invalid. **/
    protected void invalidate() {
        valid = false;
        super.invalidate();
    }

    /**
     * Convert variable to string.
     **/
    public String debugString() {
        return toString() + " = " + expression.toString();
    }
}
