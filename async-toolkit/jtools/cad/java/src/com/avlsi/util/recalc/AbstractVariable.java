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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;

/**
 * This class defines variables for the recalc engine
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public abstract class AbstractVariable implements ExpressionInterface {
    /** Variable's name. **/
    private final String name;

    /** Current value of variable. **/
    protected double value;

    /** List of variables which access this variable. **/
    protected ArrayList accessors = new ArrayList();

    /**
     * Construct variable.
     * @param name of variable
     **/
    public AbstractVariable(String name) {
        this.name = name;
    }

    /**
     * Construct anonymous variable.
     **/
    public AbstractVariable() {
        this.name = "";
    }

    /**
     * Get Name.
     * @return name of variable
     **/
    public String getName() {
        return name;
    }

    /**
     * Get list of varaibles this expression depends on.
     * Note that we don't recursively tell other variables what
     * variables "this" relies on.  "this" can handle that by itself.
     * @return list of references to variables used in this expression
     **/
    public HashSet getDependant() {
        HashSet dependants = new HashSet();
        dependants.add(this);

        return dependants;
    }

    /**
     * Evaluate the variable's value
     * @return value
     **/
    public abstract double eval();

    /**
     * Synonym for eval
     * @return value
     **/
    public double get() {
        return eval();
    }

    /**
     * Declare this variable and its accessors invalid.
     **/
    protected void invalidate() {
        for (final Iterator i = accessors.iterator(); i.hasNext(); )
            ((DependantVariable) i.next()).invalidate();
    }

    /**
     * Tell "this" that "var" accesses "this".
     * @param var Variable whose expression accesses "this".
     **/
    public void accessor(DependantVariable var) {
        accessors.add(var);
    }

    /**
     * Convert variable to string.
     **/
    public String toString() {
        return (name.equals("") ? Double.toString(eval()) : name);
    }
}
