/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim;

import com.avlsi.cast.impl.DenseSubscriptSpec;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.util.debug.Debug;

/**
 * XXX BUG FIXME TODO!!!! This class has horrible documentation.
 * Figure out what it does and document it. --frederik
 **/

/**
 * A wrapper, in the spirit of ArrayValue, for an array spec and the
 * values it corresponds to.  In this case the values are Strings.
 *
 * Assumed to contain at least one value.
 **/
public class CoSimChannelNames {
    private final DenseSubscriptSpec spec; // Java arrays are rectangular
    private final String[] vals;
    private final String instanceName;

    public CoSimChannelNames(final DenseSubscriptSpec spec,
                             final String[] vals,
                             final String instanceName) {
        this.spec = spec;
        this.vals = vals;
        this.instanceName = instanceName;
        Debug.assertTrue(vals.length > 0);
    }

    public boolean hasOneElement() {
        return (vals.length == 1);
    }

    public String getFirstElement() {
        return vals[0];
    }

    public String[] getAllElements() {
        return vals;
    }

    /**
     * Does all the channel lookups.
     * @exception DeviceConstructionException
     *     Thrown if it can't find one of the channels or some
     *     channels are inputs and some are outputs
     **/
    public CoSimChannelArrayInterface
        convertToChannels(final ChannelDictionary cdict)
        throws DeviceConstructionException {

        // Figure out whether this is an input or an output array by
        // looking at the first channel
        final ChannelInput chanIn = cdict.getInputChan(vals[0]);
        if (chanIn == null)
            return convertToChannelOutputs(cdict);
        else
            return convertToChannelInputs(cdict);
    }

    private CoSimChannelArrayInterface
        convertToChannelInputs(final ChannelDictionary cdict)
        throws DeviceConstructionException {
        final ChannelInput[] chans = new ChannelInput[vals.length];

        for (int i=0; i<vals.length; i++) {
            final ChannelInput chan = cdict.getInputChan(vals[i]);
            if (chan == null)
                throw new DeviceConstructionException("Couldn't find input channel bound to " + vals[i], this.getClass());
            chans[i] = chan;
        }

        return new CoSimChannelInputArray(spec, chans, instanceName);
    }

    private CoSimChannelArrayInterface
        convertToChannelOutputs(final ChannelDictionary cdict)
        throws DeviceConstructionException {
        final ChannelOutput[] chans = new ChannelOutput[vals.length];

        for (int i=0; i<vals.length; i++) {
            final ChannelOutput chan = cdict.getOutputChan(vals[i]);
            if (chan == null)
                throw new DeviceConstructionException("Couldn't find output channel bound to " + vals[i], this.getClass());
            chans[i] = chan;
        }

        return new CoSimChannelOutputArray(spec, chans, instanceName);
    }
}
