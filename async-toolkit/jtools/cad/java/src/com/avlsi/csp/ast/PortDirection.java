/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

import com.avlsi.fast.ports.PortDefinition;

/**
 * Class for CSP port directions.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class PortDirection {
    public static final PortDirection IN = new PortDirection("input");
    public static final PortDirection OUT = new PortDirection("output");
    public static final PortDirection INOUT = new PortDirection("inout");

    private final String message;

    private PortDirection(final String s) {
        message = s;
    }

    public String toString() {
        return message;
    }

    /**
     * Maps directions encoded as ints (used by <code>PortDefinition</code>
     * constants) to <code>PortDirection</code>s.
     **/
    public static /*@ non_null @*/ PortDirection mapDirection(
            final int direction) {
        switch (direction) {
            case PortDefinition.IN:
                return PortDirection.IN;
            case PortDefinition.OUT:
                return PortDirection.OUT;
            case PortDefinition.INOUT:
            case PortDefinition.NONE:
                return PortDirection.INOUT;
        }

        throw new AssertionError("Unknown port direction");
    }
}
