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
import com.avlsi.tools.tsim.ChannelOutput;

/**
 * Source device that outputs increasing numbers.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class AsyncSourceDevice extends AbstractDevice {

    private final ChannelOutput out;
    private final int max;

    public AsyncSourceDevice(final String name,
            final boolean suppressOutput,
            final ChannelOutput out,
            final int max) {
        super(name, suppressOutput);

        this.out = out;
        this.max = max;
    }

    public void go() throws InterruptedException {

        final long startMillis = System.currentTimeMillis();

        for (int i = 0; i < max; ++i) {
            send(out, i);
        }

        final long endMillis = System.currentTimeMillis();
        final double elapsedSec = (endMillis - startMillis) / 1000.0;

        System.out.println("sourcing " + max + " tokens took " +
                elapsedSec + " seconds @ " + (max / elapsedSec) + " tok/s");
    }
}
