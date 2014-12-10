/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

/**
 * Thrown to indicate that an instance exception was specified twice in
 * an <code>InstSpecList</code>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class DuplicateInstanceSpecException extends Exception {
    /**
     * Instance in which the cosim exception is multiply specified.
     * May not be null.
     **/
    private final String instanceName;

    /**
     * The cosim exception that is multiply specified.  May not be null.
     **/
    private final String subInstanceName;

    /**
     * Class constructor.
     **/
    public DuplicateInstanceSpecException(
            final String instanceName,
            final String subInstanceName) {
        super("Exception " + subInstanceName + " multiply specified in " +
              instanceName);
        this.instanceName = instanceName;
        this.subInstanceName = subInstanceName;
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
     * Returns the name of the subinstance that could not be found.
     * @return the name of the subinstance that could not be found, not null
     **/
    public String getSubInstanceName() {
        return subInstanceName;
    }
}
