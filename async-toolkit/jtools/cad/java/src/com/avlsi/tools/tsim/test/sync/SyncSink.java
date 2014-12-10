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

package com.avlsi.tools.tsim.test.sync;

import java.math.BigInteger;

import com.avlsi.tools.tsim.*;

/**
 * a simple class to test synchronous interface
 * will act as data sink
 *
 * @author Jagmit Sandhu, Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class SyncSink extends ClockedDevice {

    private final Clock clk;
    private final int max;
    private final int lag;

    /* input channels */
    private final SharedBus data;

    /**************************************************************************/
    /* constructor */
    public SyncSink(Clock clk,
            SharedBus data,
            String scope,
            String name,
            final int max,
            final int lag) {
        super(scope, name);

        this.clk = clk;
        this.data    = data;
        this.max = max;
        this.lag = lag;
    }

    /**************************************************************************/

    /* go method */
    public void go() throws InterruptedException {
        System.err.println("This is syncsink");
        final long startMillis = System.currentTimeMillis();

        for(int i = 0; i < max; ++i) {
            // wait for the clock to rise
            clk.wait(SharedBus.POSEDGE);

            final BigInteger d = data.getBigIntegerData();
            // System.out.println("SyncSink: received data " + d);

            final int expect = Math.max(i - lag, 0);
            if (d.intValue() != expect)
                System.err.println("ERROR: Expected " + expect + " got " + d);
        }

        final long endMillis = System.currentTimeMillis();
        final double elapsedSec = (endMillis - startMillis) / 1000.0;

        System.out.println("sinking " + max + " tokens took " +
                elapsedSec + " seconds @ " + (max / elapsedSec) + " tok/s");
    }


    /**************************************************************************/
}
