/*
 * Copyright 2003-2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto;

public interface ChannelName {
    /** Returns the type of the channel.  (For example, returns 2
     * for an e1of2.) */
    int get1of();

    /** Returns the base name of the channel.  This is just
     * alphanumeric (plus underscores.) */
    String getName();

    /** Returns the array index of the channel.  If the channel
     * is a scalar channel, the returned array is of length zero.
     * If the channel is a one-dimensional array, the returned
     * array is of length 1, with the one element being the array
     * index.  For multi-dimensional channels, the returned array
     * has one element for each dimension, specifying the index
     * for that dimension, reading left-to-right. */
    int[] getArrayIndices();

    /** Returns the base name of the channel, followed by a comma-separated
     * list of array indices in square brackets, if any. */
    String getNameWithIndices();
}
