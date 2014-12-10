/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

public class ParameterizedNodeName implements NodeName {
    private final String str;

    public ParameterizedNodeName(String str) {
        this.str = str;
    }

    public boolean isParameterized() {
        return true;
    }

    public String toStringWithIndex(int index) {
        return str + "." + index;
    }

    public String toStringWithVariable(String var) {
        return str + ".d[" + var + "]";
    }

    public NodeName unparameterize(int index) {
        return new UnparameterizedNodeName(toStringWithIndex(index));
    }

    public boolean equals(Object o) {
        return (o instanceof ParameterizedNodeName &&
                str.equals(((ParameterizedNodeName)o).str));
    }

    public int hashCode() {
        return str.hashCode();
    }
}
