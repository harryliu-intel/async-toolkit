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
 * Represents the methods in which a cell should be cosimulated.
 * <p>
 * Represents the rule <code>cosimspec_list</code>
 * in the
 * <a href="http://internal.avlsi.com/tree/sw/cad/doc/specs/cast/cosim.html">Cosimulation UI Specification</a>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class CoSimSpecList implements AcceptorInterface {
    /**
     * Methods in which a cell should be cosimulated.  May not be null.
     **/
    private final CoSimSpec[] coSimSpecs;

    /**
     * Class constructor.
     *
     * @param coSimSpecs  methods in which a cell should be cosimulated.  
     *   May not be null.
     **/
    public CoSimSpecList(final CoSimSpec[] coSimSpecs) {
        this.coSimSpecs = coSimSpecs;
    }

    /**
     * Returns an <code>Iterator&lt;CoSimSpec&gt;</code> of the
     *   methods to be cosimulated.
     * @return an <code>Iterator&lt;CoSimSpec&gt;</code> of the
     *   methods to be cosimulated, not null.
     **/
    public Iterator<CoSimSpec> iterator() {
        return Collections.unmodifiableList(Arrays.asList(coSimSpecs))
                          .iterator();
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer("{");
        boolean firstP = true;

        for (int i = 0; i < coSimSpecs.length; ++i) {
            if (firstP)
                firstP = false;
            else
                sb.append(" | ");

            sb.append(coSimSpecs[i].toString());
        }

        sb.append('}');

        return sb.toString();
    }

    public void accept(VisitorInterface v) {
        v.visitCoSimSpecList(this);
    }
}
