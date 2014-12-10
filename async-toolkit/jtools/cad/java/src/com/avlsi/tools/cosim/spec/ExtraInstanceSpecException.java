/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

import com.avlsi.tools.cosim.spec.InstSpecList;

/**
 * Thrown by <code>DSim.cosimulate</code> to indicate that a non-empty
 * <code>InstSpecList</code> was provided where it should have been empty.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ExtraInstanceSpecException extends Exception {
    /**
     * Instance for which the non-empty <code>InstSpecList</code>
     * was specified.  May not be null.
     **/
    private final String instanceName;

    /**
     * The non-empty <code>InstSpecList</code>.  May not be null.
     **/
    private final InstSpecList instSpecList;

    /**
     * Class constructor.
     **/
    public ExtraInstanceSpecException(
            final String instanceName,
            final InstSpecList instSpecList) {
        super("Erroneously non-empty instspec_list " + instSpecList +
                " for " + instanceName);
        this.instanceName = instanceName;
        this.instSpecList = instSpecList;
    }

    /**
     * Returns the instance for which the non-empty <code>InstSpecList</code>
     *   was specified.
     * @return the instance for which the non-empty <code>InstSpecList</code>
     *   was specified.  Not null.
     **/
    public String getInstanceName() {
        return instanceName;
    }

    /**
     * Returns the non-empty <code>InstSpecList</code>.
     * @return the non-empty <code>InstSpecList</code>, not null.
     **/
    public InstSpecList getInstSpecList() {
        return instSpecList;
    }
}
