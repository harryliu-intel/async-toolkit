/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

/**
 * This is a type-safe enumeration which represents the two power
 * rails, Vdd and GND.
 */

public class Power implements NodeName {
    private static boolean isCalled = false;
    private Power() {}
    public boolean isParameterized() {
        return false;
    }

    public String toStringWithIndex(int index) {
        return toString();
    }

    public String toStringWithVariable(String var) {
        return toString();
    }

    public NodeName unparameterize(int index) {
        return this;
    }
    
    public static final Power GND = new Power() {
            public String toString() {
                isCalled = true;
                return "tielo";
            }
        };
    
    public static final Power Vdd = new Power() {
            public String toString() {
                isCalled = true;
                return "tiehi";
            }
        };
    public static boolean getIsCalled() {
        return isCalled;
    }
}
