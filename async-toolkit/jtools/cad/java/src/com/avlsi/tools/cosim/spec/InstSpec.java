/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

/**
 * Represents an instance and cosimulation method for an instance-by-instance
 * exception.
 * <p>
 * Represents the rule <code>instspec</code>
 * in the
 * <a href="http://internal.avlsi.com/tree/sw/cad/doc/specs/cast/cosim.html">Cosimulation UI Specification</a>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class InstSpec implements AcceptorInterface {
    /**
     * Instance name.  May not be null.
     **/
    private final String instanceName;

    /**
     * Cosimulation method.  May not be null.
     **/
    private final CoSimSpecList coSimSpecList;

    /**
     * Class constructor.
     *
     * @param instanceName  instance name, may not be null
     * @param coSimSpecList  cosimulation method, may not be null
     **/
    public InstSpec(
            final String instanceName,
            final CoSimSpecList coSimSpecList) {
        this.instanceName = instanceName;
        this.coSimSpecList = coSimSpecList;
    }

    /**
     * Returns the instance name.
     * @return the instance name, not null
     **/
    public String getInstanceName() {
        return instanceName;
    }

    /**
     * Returns the cosimulation method.
     * @return the cosimulation method, not null
     **/
    public CoSimSpecList getCoSimSpecList() {
        return coSimSpecList;
    }

    public String toString() {
        return " - " + instanceName + coSimSpecList;
    }

    public void accept(VisitorInterface v) {
        v.visitInstSpec(this);
    }
}
