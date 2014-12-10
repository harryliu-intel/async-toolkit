/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim;

import com.avlsi.cast.impl.DenseSubscriptSpec;
import com.avlsi.cast.impl.InvalidOperationException;
import com.avlsi.cast.impl.Range;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;

/**
 * Class for the cosim framework to use to pass an array of outputs to
 * a javablock device.  Works along the same principle as ArrayValue,
 * but simpler.
 **/
public class CoSimChannelOutputArray implements CoSimChannelArrayInterface {
    private final DenseSubscriptSpec spec; // Java arrays are rectangular
    private final ChannelOutput[] chans;
    private final String instanceName;

    public CoSimChannelOutputArray(final DenseSubscriptSpec spec,
                                   final ChannelOutput[] chans,
                                   final String instanceName) {
        this.spec = spec;
        this.chans = chans;
        this.instanceName = instanceName;
    }

    public DenseSubscriptSpec getSpec() {
        return spec;
    }

    /**
     * Returns all the channels included in this array as a
     * one-dimensional array.  The order of the channels is not
     * guaranteed.  The array should not be modified.
     **/
    public ChannelOutput[] getChannels() {
        return chans;
    }

    /**
     * Returns the channel resulting from subscripting.  Can return
     * slices.
     * <p>
     * In order for the access to be valid, the specifications must
     * both have the same number of dimensions, and all the indices
     * of the accessSpec must present in the array's spec.
     **/
    public CoSimChannelOutputArray
        accessArray(final DenseSubscriptSpec accessSpec)
        throws InvalidOperationException {

        if (spec.getNumDimensions() != accessSpec.getNumDimensions())
            throw new InvalidOperationException("dims don't agree.  array has " + spec.getNumDimensions() + " dimensions but accessor has " + accessSpec.getNumDimensions());

        final ChannelOutput[] newChans =
            new ChannelOutput[accessSpec.getNumElements()];

        for (int i = 0; i < newChans.length; ++i) {
            // spec.positionOf(idx) will throw IndexOutOfBoundsException
            // if idx is not in the spec
            final int[] idx = accessSpec.indexOf(i);
            final int position;

            try {
                position = spec.positionOf(accessSpec.indexOf(0));
            } catch (IndexOutOfBoundsException e) {
                throw new InvalidOperationException("bad array access of "
                        + instanceName + ": index "
                        + DenseSubscriptSpec.idxToString(accessSpec.indexOf(0))
                        + " not in spec " + spec, e);
            }
            newChans[i] = chans[position];
        }

        return new CoSimChannelOutputArray(accessSpec, newChans, instanceName);
    }

    /**
     * Access a single index.
     **/
    public ChannelOutput accessArray(final int[] idx) {
        try {
            final Range[] rs = new Range[idx.length];

            for (int i = 0; i < rs.length; ++i)
                rs[i] = new Range(idx[i], idx[i]);

            final CoSimChannelOutputArray wrapper =
                accessArray(new DenseSubscriptSpec(rs));
            Debug.assertTrue(wrapper.chans.length == 1);

            return wrapper.chans[0];
        } catch (InvalidOperationException e) {
            throw new AssertionFailure(e);
        }
    }

    /**
     * Access a range of indices.
     **/
    public ChannelOutput [] accessArrayRange(final int from, final int to) {
        Debug.assertTrue(to>=from);
        int N = to-from+1;
        ChannelOutput range[] = new ChannelOutput[N];
        for (int i=0; i<N; i++) {
            range[i] = accessArray(new int[]{from+i});
        }
        return range;
    }

    public String toString() {
        return "CoSimChannelOutputArray " + instanceName;
    }
}
