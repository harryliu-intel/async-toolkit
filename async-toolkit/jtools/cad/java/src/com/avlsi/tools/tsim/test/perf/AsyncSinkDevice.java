/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim.test.perf;

import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.tsim.ChannelInput;

/**
 * Sink device that reads numbers and ensures that they are an increasing
 * sequence.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class AsyncSinkDevice extends AbstractDevice {

    private final ChannelInput in;
    private final int max;

    public AsyncSinkDevice(final String name,
            final boolean suppressOutput,
            final ChannelInput in,
            final int max) {
        super(name, suppressOutput);

        this.in = in;
        this.max = max;
    }

    public void go() throws InterruptedException {
        final long startMillis = System.currentTimeMillis();

        for (int i = 0; i < max; ++i) {
            final int v = receive(in).intValue();

            if (v != i)
                System.err.println("ERROR: Expected " + i + " got " + v);
        }

        final long endMillis = System.currentTimeMillis();
        final double elapsedSec = (endMillis - startMillis) / 1000.0;

        System.out.println("sinking " + max + " tokens took " +
                elapsedSec + " seconds @ " + (max / elapsedSec) + " tok/s");
        // System.exit(0);
    }
}
