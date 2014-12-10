/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast.impl;

import com.avlsi.cell.CellImpl;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.debug.Debug;

/**
 * This class represents javablock-internal channels (and arrays of
 * same) to the cast parser.  It provides the fields "in" and "out" so
 * that the devices in the javablock can specify whether they're
 * sending or receiving.  Immutable.
 **/

public class JavaChannelContainerValue
    extends Value
    implements FieldedValueInterface {

    private static final Type TYPE =
        new ClassType("<java channel container>");

    private final HierName instanceName;

    /**
     * Should be either a JavaChannelValue or an ArrayValue containing them.
     **/
    private final Value in;

    /**
     * Should be either a JavaChannelValue or an ArrayValue containing them.
     **/
    private final Value out;

    /**
     * Creates the JavaChannelContainerValue for a single channel
     * @param name name of the channel and this Value
     **/
    public JavaChannelContainerValue(final HierName name) {
        super(true);
        instanceName = name;

        final HierName inName, outName;
        try {
            inName = HierName.makeHierName(name, "in");
            outName = HierName.makeHierName(name, "out");
        } catch (InvalidHierNameException e) {
            throw new RuntimeException("neither 'in' nor 'out' contains a period, so this should never happen: " + e.getMessage(), e);
        }

        in = new JavaChannelValue(inName, true);
        out = new JavaChannelValue(outName, false);
    }

    /**
     * Creates the JavaChannelContainerValue for an array of channels
     * @param name base name for the array and the name of this Value
     * @param spec specification of the array
     **/
    public JavaChannelContainerValue(final HierName name,
                                     final SubscriptSpecInterface spec) {
        super(true);
        instanceName = name;

        final HierName inName, outName;
        try {
            inName = HierName.makeHierName(name, "in");
            outName = HierName.makeHierName(name, "out");
        } catch (InvalidHierNameException e) {
            throw new RuntimeException("neither 'in' nor 'out' contains a period, so this should never happen: " + e.getMessage(), e);
        }

        in = ArrayValue.makeArray(new JavaChannelValue(inName, true), spec);
        out = ArrayValue.makeArray(new JavaChannelValue(outName, false), spec);
    }

    public String toString() {
        return "javablock channel(" + instanceName + ")";
    }

    public Value accessField(final Symbol sym, final int permission)
        throws InvalidOperationException {
        if (sym.equals(Symbol.create("in"))) {
            return in;
        } else if (sym.equals(Symbol.create("out"))) {
            return out;
        } else {
            throw new InvalidOperationException("java channels can only be accessed by .in and .out, not ." + sym.getString());
        }
    }

    //
    // functions implementing Value
    //

    public Value assign(final Value v, final CellImpl cell) {
        Debug.assertTrue(false, "should never assign JavaChannelContainerValues");
        return null;            // make compiler happy
    }

    /**
     * Will be needed if array statements are allowed in the javablock
     * in the future.  However, since JavaChannelContainerValues are
     * immutable, this function is trivial.
     **/
    public Value duplicate () {
        return this;
    }

    public HierName getInstanceName() {
        return this.instanceName;
    }

    public Type getType() {
        return TYPE;
    }

    public Value newInstanceName(final HierName newInstanceName) {
        Debug.assertTrue(false, "no reason why you should ever do this, so it's not implemented yet");
        return null;            // make compiler happy
    }
}
