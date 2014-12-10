/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

/**
 * Thrown to indicate that an instance exception was requested in
 * an <code>InstSpecList</code> but the subcell did not exist.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class NoSuchInstanceException extends Exception {
    /**
     * Instance in which the named subcell could not be found.
     * May not be null.
     **/
    private final String instanceName;

    /**
     * Type of <code>instanceName</code>.  May not be null.
     **/
    private final String cellType;

    /**
     * The name of the subcell that could not be found.  May not be null.
     **/
    private final String subcellName;

    public NoSuchInstanceException(
            final String instanceName,
            final String cellType,
            final String subcellName) {
        super("No subcell " + subcellName + " found in " +
                instanceName + "(type " + cellType + ")");
        this.instanceName = instanceName;
        this.cellType = cellType;
        this.subcellName = subcellName;
    }

    /**
     * Returns the instance in which the named subcell could not be found.
     * @return the instance in which the named subcell could not be found,
     *   not null.
     **/
    public String getInstanceName() {
        return instanceName;
    }

    /**
     * Returns the type of <code>instanceName</code>.
     * @return the type of <code>instanceName</code>, not null.
     **/
    public String getCellType() {
        return cellType;
    }

    /**
     * Returns the name of the subcell that could not be found.
     * @return the name of the subcell that could not be found, not null
     **/
    public String getSubcellName() {
        return subcellName;
    }
}
