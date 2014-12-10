/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;

/**
 * Represents the instance-by-instance exceptions.
 * <p>
 * Represents the rule <code>instspec_list</code>
 * in the
 * <a href="http://internal.avlsi.com/tree/sw/cad/doc/specs/cast/cosim.html">Cosimulation UI Specification</a>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class InstSpecList implements AcceptorInterface {
    /**
     * Instance-by-instance exceptions.  May not be null.
     **/
    private final InstSpec[] instSpecs;

    /**
     * Class constructor.
     *
     * @param instSpecs  Instance-by-instance exceptions.  May not be null.
     **/
    public InstSpecList(final InstSpec[] instSpecs) {
        this.instSpecs = instSpecs;
    }

    /**
     * Returns an <code>Iterator&lt;InstSpec&gt;</code> of the
     *   instance-by-instance exceptions
     * @return an <code>Iterator&lt;InstSpec&gt;</code> of the
     *   instance-by-instance exceptions, not null
     **/
    public Iterator<InstSpec> iterator() {
        return Collections.unmodifiableList(Arrays.asList(instSpecs))
                          .iterator();
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();

        for (int i = 0; i < instSpecs.length; ++i)
            sb.append(instSpecs[i].toString());

        return sb.toString();
    }

    public void accept(VisitorInterface v) {
        v.visitInstSpecList(this);
    }
}
