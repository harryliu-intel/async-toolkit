/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

import java.math.BigInteger;

/**
 * Class for CSP channel type.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class ChannelType extends Type {

    /**
     * The number of different values the channel can carry,
     * from <code>0</code> to <code>numValues - 1</code>.
     **/
    private final /*@ non_null @*/ BigInteger numValues;

    /**
     * The direction of the channel.
     **/
    private final /*@ non_null @*/ PortDirection direction;

    /**
     * CAST type name for the channel.
     **/
    private final /*@ non_null @*/ String typeName; 

    /**
     * Class constructor
     *
     * @param numValues
     *        The number of different values the channel can carry,
     *        from <code>0</code> to <code>numValues - 1</code>.
     **/
    public ChannelType(final /*@ non_null @*/ BigInteger numValues,
                       final /*@ non_null @*/ PortDirection direction,
                       final /*@ non_null @*/ String typeName) {
        this.numValues = numValues;
        this.direction = direction;
        this.typeName = typeName;
    }

    /**
     * Returns the number of values respresentable by the channel,
     * or -1 for unbounded.
     **/
    public /*@ non_null @*/ BigInteger getNumValues() {
        return numValues;
    }

    /**
     * Returns the direction of the channel, input or output.
     **/
    public /*@ non_null @*/ PortDirection getDirection() {
        return direction;
    }

    /**
     * Returns the CAST type name of the channel.
     **/
    public /*@ non_null @*/ String getTypeName() {
        return typeName;
    }

    public void accept(VisitorInterface v) throws VisitorException {
        v.visitChannelType(this);
    }

    public int dimension() {
        // A channel is a scalar CSP type, so its dimension is 0.
        return 0;
    }

    public String toString() {
        return getTypeName();
    }
}
