/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

/**
 * Thrown to indicate that a cosimulation could not be carried out because
 * a depth greater than the depth of the hierarchy was requested.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class HierarchyDepthException extends Exception {

    /**
     * Instance name whose depth was exceeded.  May not be null.
     **/
    final String instanceName;

    /**
     * Amount by which depth was exceeded.  Must be positive.
     **/
    final int excessDepth;

    /**
     * Class constructor.
     *
     * @param instanceName Instance name whose depth was exceeded.  
     *   May not be null.
     * @param excessDepth  Amount by which depth was exceeded.  
     *   Must be positive.
     **/
    public HierarchyDepthException(final String instanceName,
            final int excessDepth) {
        this.instanceName = instanceName;
        this.excessDepth = excessDepth;
    }

    /**
     * Returns the instance name whose depth was exceeded.
     * @return the instance name whose depth was exceeded, not null.
     **/
    public String getInstanceName() {
        return instanceName;
    }

    /**
     * Returns the amount by which the depth was exceeded.
     * @return the amount by which the depth was exceeded, positive.
     **/
    public int getExcessDepth() {
        return excessDepth;
    }

    public String toString() {
        return "Depth of " + instanceName + " exceeded by " + excessDepth;
    }
}
