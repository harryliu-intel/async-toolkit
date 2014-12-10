/*
 * Copyright 2003-2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto;

public interface TruthTable {
    /** Returns an array of inputs which this output depends on. */
    ChannelName[] getInputs();

    /** Returns the name of this output. */
    ChannelName getOutput();

    /** An array of output values.  The size of the array is
     * getInput()[0].get1of() * getInput()[1].get1of() * ...
     * The output values are packed into the array, with the
     * first input being the least-significant part of the
     * array index, and the last input being the most-significant
     * part.  An output value of -1 means no output. */
    byte[] getTable();

    /**
     * Returns an array of booleans (where the indices have the same
     * meaning as in the array returned by getTable()) which indicates
     * whether the input is acknowledged.
     * @param  input  which input you want to know about (same as indices
     *                into the array returned by getInput())
     */
    boolean[] getInputAcknowledge(int input);
}
