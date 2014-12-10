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

import com.avlsi.tools.tsim.*;

/**
 * Synchronous data generator.
 *
 * @author Jagmit Sandhu, Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class SyncSource extends ClockedDevice {

    private final Clock clk;
    private final int max;

    /* output channels */
    private final SharedBus data;

    /**************************************************************************/
    /* constructor */
    public SyncSource(Clock clk,
            SharedBus data,
            String scope,
            String name,
            final int max) {
        super(scope, name);

        this.clk = clk;
        this.data    = data;
        this.max = max;
    }

    /**************************************************************************/

    /* go method */
    public void go() throws InterruptedException {

        System.err.println("this is syncsource");
        final long startMillis = System.currentTimeMillis();

        for (int i = 0; i < max; ++i) {
            // wait for the clock to rise
            clk.wait(SharedBus.POSEDGE);

            // send data
            data.set(this, i, 1);

            // System.out.println("SyncSource: sent data " + i);
        }

        final long endMillis = System.currentTimeMillis();
        final double elapsedSec = (endMillis - startMillis) / 1000.0;

        System.out.println("sourcing " + max + " tokens took " +
                elapsedSec + " seconds @ " + (max / elapsedSec) + " tok/s");
    }


    /**************************************************************************/
}
