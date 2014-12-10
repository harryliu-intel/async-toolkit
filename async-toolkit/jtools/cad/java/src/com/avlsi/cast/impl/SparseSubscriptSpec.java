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
import com.avlsi.util.exception.AssertionFailure;

import java.util.Arrays;
import java.util.Comparator;

/**
 * This class represents subscripting specification for sparse arrays,
 * ie arrays with indices <code>[0,0], [1,2], [3,1]</code> defined.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class SparseSubscriptSpec implements SubscriptSpecInterface {
    /**
     * Comparator to lexicographically compare <code>int[]</code> used as
     * indices.
     **/
    private static final Comparator/*<int[]>*/ indexComparator =
        new Comparator/*<int[]>*/() {
            public int compare(final /*@ non_null @*/ Object o1,
                               final /*@ non_null @*/ Object o2) {
                final int[] a1 = (int[]) o1;
                final int[] a2 = (int[]) o2;

                assert a1.length == a2.length;
                for (int i = 0; i < a1.length; ++i) {
                    // do subtraction as long to avoid overflow
                    final long v1 = a1[i];
                    final long v2 = a2[i];
                    final long c = v1 - v2;

                    if (c != 0)
                        return c < 0 ? -1 : 1;
                }

                return 0;
            }
        };

    /**
     * A list of the indices that exist in the subscript spec.
     * If <code>[0,0], [1,2], [3,1]</code> existed, then
     * <code> indices = {{0, 0}, {1,2}, {3,1}} </code>.
     * Must always be sorted.
     **/
    private final int[][] indices;

    /**
     * The number of dimensions of the array spec.
     **/
    private final int numDimensions;

    /**
     * Constructs a sparse spec from a dense one.
     **/
    public SparseSubscriptSpec(final DenseSubscriptSpec spec) {
        numDimensions = spec.getNumDimensions();
        indices = new int[spec.getNumElements()][];

        for (int i = 0; i < indices.length; ++i)
            indices[i] = spec.indexOf(i);
    }

    /**
     * Constructor used by <code>augment()</code> to construct a
     * spec from existing indices.  Requires that the indices be
     * sorted in lexicographic order.
     **/
    private SparseSubscriptSpec(final /*@ non_null @*/ int[][] indices,
                                final int numDimensions) {
        this.indices = indices;
        this.numDimensions = numDimensions;
    }

    public int getNumElements() {
        return indices.length;
    }

    public int getNumDimensions() {
        return numDimensions;
    }

    public int positionOf(final int[] idx) {
        if (idx.length != numDimensions)
            throw new IllegalArgumentException("dims don't agree: "
                                               + idx.length + "!="
                                               + numDimensions);

        final int index = Arrays.binarySearch(indices, idx, indexComparator);

        if (index < 0)
            throw new IndexOutOfBoundsException("subscript out of range");
        else
            return index;
    }

    public int[] indexOf(int pos) {
        final int[] a = new int[numDimensions];

        for (int i = 0; i < numDimensions; ++i)
            a[i] = indices[pos][i];

        return a;
    }

    /**
     * Returns a new SparseSubscriptSpec containing all the indices from
     * <code>oldSpec</code> and <code>augmentingSpec</code>.
     * 
     * @throws InvalidOperationException
     *         If <code>oldSpec</code> is not disjoint with
     *         </code>augmentingSpec</code> or if the
     *         dimensions do not agree.
     *
     * <pre><jml>
     *   normal_behavior
     *     requires newValues.length ==
     *                oldValues.length + augmentingValues.length;
     *     requires oldSpec.getNumElements() == oldValues.length;
     *     requires augmentingSpec.getNumElements() == augmentingValues.length;
     * </jml></pre>
     **/
    static /*@ non_null @*/ SparseSubscriptSpec augment
        (/*@ non_null @*/ SubscriptSpecInterface oldSpec,
         /*@ non_null @*/ Value[] oldValues,
         /*@ non_null @*/ SubscriptSpecInterface augmentingSpec,
         /*@ non_null @*/ Value[] augmentingValues,
         /*@ non_null @*/ Value[] newValues)
        throws InvalidOperationException {

        final int numDimensions = oldSpec.getNumDimensions();
        if (numDimensions != augmentingSpec.getNumDimensions())
            throw new InvalidOperationException("dims don't agree: "
                                               + oldSpec.getNumDimensions() + "!="
                                               + augmentingSpec.getNumDimensions());

        int[][] indices = new int[newValues.length][];

        // merge indices and values into new arrays
        int iOld = 0;
        int iAugmenting = 0;
        int iNew = 0;
        for ( ; iOld < oldValues.length &&
                iAugmenting < augmentingValues.length; ++iNew) {
            final int[] oldIndex = oldSpec.indexOf(iOld);
            final int[] augmentingIndex =
                augmentingSpec.indexOf(iAugmenting);
            final int c = indexComparator.compare(oldIndex, augmentingIndex);
            if (c == 0) {
                throw new InvalidOperationException("Cannot augment. " +
                        "Both old and augmenting indices contain " +
                        DenseSubscriptSpec.idxToString(oldIndex));
            } else if (c < 0) {
                indices[iNew] = oldIndex;
                newValues[iNew] = oldValues[iOld];
                iOld++;
            } else {
                indices[iNew] = augmentingIndex;
                newValues[iNew] = augmentingValues[iAugmenting];
                iAugmenting++;
            }
        }

        // copy over remainder
        for ( ; iOld < oldValues.length; ++iNew, ++iOld) {
            indices[iNew] = oldSpec.indexOf(iOld);
            newValues[iNew] = oldValues[iOld];
        }

        for ( ; iAugmenting < augmentingValues.length;
                ++iNew, ++iAugmenting) {
            indices[iNew] = augmentingSpec.indexOf(iAugmenting);
            newValues[iNew] = augmentingValues[iAugmenting];
        }

        return new SparseSubscriptSpec(indices, numDimensions);
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();

        sb.append("[");

        for (int i = 0; i < indices.length; ++i) {
            if (i > 0)
                sb.append(", ");
            sb.append(DenseSubscriptSpec.idxToString(indices[i]));
        }

        sb.append("]");

        return sb.toString();
    }

    public CoSimChannelNames getCoSimChannelNames(String baseName,
                                                  boolean ignoreLastDimension) {
        throw new AssertionFailure("can't handle sparse arrays as javablock device parameters yet.  " + baseName + " is a sparse array");
    }
}
