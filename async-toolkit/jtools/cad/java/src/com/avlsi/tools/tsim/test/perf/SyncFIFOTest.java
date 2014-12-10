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

import com.avlsi.tools.tsim.Clock;
import com.avlsi.tools.tsim.SchedulerRunner;
import com.avlsi.tools.tsim.SharedBus;

import com.avlsi.tools.tsim.test.sync.SyncSink;
import com.avlsi.tools.tsim.test.sync.SyncSource;

/**
 * Performance test of synchronous FIFOs.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class SyncFIFOTest {
    private static void usage() {
        System.err.println("Usage: " + SyncFIFOTest.class.getName() +
                " numTokens slack");
        System.err.println("numTokens and slack must be non-negative");
        System.exit(1);
    }

    public static void main(String[] args) {

        if (args.length != 2)
            usage();

        final int numTokens = Integer.parseInt(args[0]);
        final int slack = Integer.parseInt(args[1]);

        final Clock clk = new Clock("bus", "clock", 3600, null);

        final SharedBus data1, data2;
        if (slack < 0) {
            usage();
            data1 = data2 = null;
        } else if (slack == 0) {
            data1 = data2 = new SharedBus("bus", "data", 32, null);
        } else {
            data1 = new SharedBus("bus", "data1", 32, null);
            data2 = new SharedBus("bus", "data2", 32, null);

            new SyncFIFODevice("bus", "fifo", clk,
                    data1, data2, slack).start();
        }

        clk.start();
        new SyncSource(clk, data1, "bus", "syncTest", numTokens).start();
        new SyncSink(clk, data2, "bus", "syncTest",
                numTokens, slack == 0 ? 1 : slack + 2).start();

        new SchedulerRunner().start();
    }
}
