/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cast.impl;

import com.avlsi.tools.cosim.CoSimChannelNames;
import com.avlsi.util.debug.Debug;

/**
 * This class represents subscripting specification for arrays,
 * ie <code>[K..L,M..N]</code>
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class DenseSubscriptSpec implements SubscriptSpecInterface {
    private final Range[] ranges;

    /**
     * Class constructor.
     * 
     * @throws IllegalArgumentException  If the min exceeds the max for
     *   any of the ranges.
     **/
    public DenseSubscriptSpec(final Range[] ranges) {
        this.ranges = new Range[ranges.length];

        for (int i = 0; i < ranges.length; ++i) {
            if (ranges[i].getMin() > ranges[i].getMax())
                throw new IllegalArgumentException("range min > max in spec: "
                                                   + ranges[i].getMin() + ">"
                                                   + ranges[i].getMax());
            this.ranges[i] = ranges[i];
        }
    }

    public int getNumElements() {
        int s = 1;
        for (int i = 0; i < ranges.length; ++i)
            s *= getSizeForDimension(i);
        return s;
    }

    public int getNumDimensions() {
        return ranges.length;
    }

    public int getSizeForDimension(int nDim) {
        return ranges[nDim].size();
    }

    public int positionOf(final int[] idx) {
        if (idx.length != getNumDimensions())
            throw new IllegalArgumentException("dims don't agree: "
                                               + idx.length + "!="
                                               + getNumDimensions());

        int pos = 0;
        for (int i = 0; i < idx.length; ++i) {
            if (!ranges[i].contains(idx[i]))
                throw new IndexOutOfBoundsException("subscript " + idx[i]
                                                    + " out of range "
                                                    + ranges[i]);

            pos = pos * getSizeForDimension(i)
                + (idx[i] - ranges[i].getMin());
        }

        return pos;
    }

    public int[] indexOf(int pos) {
        if (pos < 0 || pos >= getNumElements())
            throw new IllegalArgumentException("This array doesn't have a "
                                               + pos + "th element");

        final int[] a = new int[getNumDimensions()];
        int off = getNumElements();

        for (int i = 0; i < a.length; ++i) {
            off /= getSizeForDimension(i);
            final int zeroIdxPos = pos / off;
            pos -= zeroIdxPos * off;
            a[i] = zeroIdxPos + ranges[i].getMin();
        }

        Debug.assertTrue(off == 1);

        return a;
    }

    /**
     * Returns the range for dimension idim.  Safe because Range is 
     * immutable.
     **/
    public Range getRange(int idim) {
        return ranges[idim];
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();

        sb.append("[");

        for (int i = 0; i < ranges.length; ++i) {
            if (i > 0)
                sb.append(",");

            sb.append(ranges[i].toString());
        }

        sb.append("]");

        return sb.toString();
    }

    public static String idxToString(final int[] idx) {
        final StringBuffer sb = new StringBuffer();

        sb.append("[");

        for (int i = 0; i < idx.length; ++i) {
            if (i > 0)
                sb.append(",");

            sb.append(idx[i]);
        }

        sb.append("]");

        return sb.toString();
    }

    /**
     * Returns true if the two subscript specs have exactly the
     * same number of dimensions, and the same indexes exist in
     * each dimension.
     **/
    public boolean equals(final Object o) {
        return o instanceof DenseSubscriptSpec
            && equals((DenseSubscriptSpec) o);
    }

    /**
     * Returns true if the two subscript specs have exactly the
     * same number of dimensions, and the same indexes exist in
     * each dimension.
     **/
    public boolean equals(final DenseSubscriptSpec s) {
        if (ranges.length != s.ranges.length)
            return false;
        else {
            for (int i = 0; i < ranges.length; ++i)
                if (!ranges[i].equals(s.ranges[i]))
                    return false;

            return true;
        }
    }

    /**
     * Constructs the spec for CoSimChannelNames, based on this spec.
     **/
    private DenseSubscriptSpec getCoSimSpec(boolean ignoreLastDimension) {
        final int numDimensions;
        if (ignoreLastDimension)
            numDimensions = this.getNumDimensions() - 1;
        else
            numDimensions = this.getNumDimensions();

        final Range[] ranges = new Range[numDimensions];
        // Bug 917: Previously, new ranges were being created
        // [0 .. old_max - old_min].  I think that this was a mistake,
        // so now we will use the same range --jmr
        System.arraycopy(this.ranges, 0, ranges, 0, ranges.length);

        return new DenseSubscriptSpec(ranges);
    }

    /**
     * Returns an array of names.  Under the cosim framework, each of
     * these names corresponds to a NodeChannel.
     *
     * @param baseName the name of the overall array
     * @param ignoreLastDimension Arrays of wide channels use the last
     * dimension as channelwidth, so cosim names for that array should
     * ignore that dimension
     **/
    public CoSimChannelNames getCoSimChannelNames(String baseName,
                                                  boolean ignoreLastDimension) {
        final DenseSubscriptSpec newSpec = getCoSimSpec(ignoreLastDimension);
        final String[] vals = new String[newSpec.getNumElements()];

        for (int i=0; i<newSpec.getNumElements(); i++) {
            final int[] index = newSpec.indexOf(i);
            if (index.length < 1) // Prevent "L[]" as a created name
                vals[i] = baseName;
            else
                vals[i] = baseName + idxToString(index);
        }

        return new CoSimChannelNames(newSpec, vals, baseName);
    }
}
