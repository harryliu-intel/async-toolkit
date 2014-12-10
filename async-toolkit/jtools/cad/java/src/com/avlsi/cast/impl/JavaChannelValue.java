/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast.impl;

import com.avlsi.cell.CellImpl;
import com.avlsi.file.common.HierName;
import com.avlsi.util.debug.Debug;

/**
 * This class represents one direction (either sending or receiving)
 * of a javablock-internal channel to the cast parser.  Immutable.
 **/
public class JavaChannelValue extends Value {
    private final HierName instanceName;
    /** Input or output channel? **/
    private final boolean inputChannelP;

    private static final Type TYPE =
        new ClassType("<java channel>");

    public JavaChannelValue(final HierName instanceName,
                            final boolean inputChannelP) {
        super(true);
        this.instanceName = instanceName;
        this.inputChannelP = inputChannelP;
    }

    //
    // functions implementing Value
    //

    public Value assign(final Value v, final CellImpl cell) {
        Debug.assertTrue(false, "should never assign JavaChannelValues");
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
        return new JavaChannelValue(newInstanceName, inputChannelP);
    }
}
