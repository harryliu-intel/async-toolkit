/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim;

import com.avlsi.cast.impl.DenseSubscriptSpec;
import com.avlsi.cast.impl.SubscriptSpecInterface;
import com.avlsi.tools.tsim.BufferedChannel;

/**
 * Class for containing parameters describing a channel internal to
 * the CAST java block.  Doesn't contain the name; these are indexed
 * by name.  Can describe an array of channels.  Immutable.
 **/
final class InternalChannelInfo {
    private final int slack;
    /** null if this isn't an array. **/
    private final SubscriptSpecInterface arraySpec;

    private static final int defaultSlack = 1; // smallest possible
    private static final SubscriptSpecInterface defaultArraySpec = null;

    public InternalChannelInfo() {
        this(defaultSlack, defaultArraySpec);
    }

    public InternalChannelInfo(final int slack) {
        this(slack, defaultArraySpec);
    }

    public InternalChannelInfo(final SubscriptSpecInterface arraySpec) {
        this(defaultSlack, arraySpec);
    }

    public InternalChannelInfo(final int slack,
                               final SubscriptSpecInterface arraySpec) {
        this.slack = slack;
        this.arraySpec = arraySpec;
    }

    /**
     * Makes appropriate BufferedChannel(s) and adds them to the
     * dictionary.  Internal channels are both inputs and outputs.
     **/
    public void createInternalChannels(final String name,
                                       final ChannelDictionary dict) {
        if (arraySpec == null) {
            final BufferedChannel chan = new BufferedChannel(slack);
            dict.addInputChan(name+".in", chan);
            dict.addOutputChan(name+".out", chan);
        } else {
            for (int i=0; i<arraySpec.getNumElements(); i++) {
                final String arrayString =
                    DenseSubscriptSpec.idxToString(arraySpec.indexOf(i));
                final BufferedChannel chan = new BufferedChannel(slack);
                dict.addInputChan(name+".in"+arrayString, chan);
                dict.addOutputChan(name+".out"+arrayString, chan);
            }
        }
    }
}



