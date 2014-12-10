/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cell;

/**
 * Thrown to indicate that the specified environment could not be found
 * for cell instantiation.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class NoSuchEnvironmentException extends Exception {

    private final String cellName;
    private final String envName;

    /**
     * Constructs a <code>NoSuchEnvironmentException</code> indicating
     * that the specified environment could not be created.
     **/
    public NoSuchEnvironmentException(final String cellName,
                                      final String envName) {
        this.cellName = cellName;
        this.envName = envName;
    }

    /**
     * Constructs a <code>NoSuchEnvironmentException</code> indicating
     * that the specified environment could not be created.
     **/
    public NoSuchEnvironmentException(final String cellName,
                                      final String envName,
                                      final Throwable cause) {
        super(cause);
        this.cellName = cellName;
        this.envName = envName;
    }

    /**
     * Returns the name of the environment that couldn't be found.
     **/
    public String getEnvironmentName() {
        return envName;
    }

    /**
     * Returns the name of the cell where the environment couldn't be found.
     **/
    public String getCellName() {
        return cellName;
    }
}
