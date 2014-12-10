/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

public class UnparameterizedNodeName implements NodeName {
    private final String str;

    public UnparameterizedNodeName(String str) {
        this.str = str;
    }

    public boolean isParameterized() {
        return false;
    }

    public String toStringWithIndex(int index) {
        return str;
    }

    public String toStringWithVariable(String var) {
        return str;
    }

    public NodeName unparameterize(int index) {
        return this;
    }

    public boolean equals(Object o) {
        return (o instanceof UnparameterizedNodeName &&
                str.equals(((UnparameterizedNodeName)o).str));
    }

    public int hashCode() {
        return str.hashCode();
    }

    public String toString() {
        return str;
    }
}
