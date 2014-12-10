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

import java.math.BigInteger;

import com.avlsi.tools.tsim.Clock;
import com.avlsi.tools.tsim.ClockedDevice;
import com.avlsi.tools.tsim.SharedBus;

/**
 * Synchronous FIFO.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
class SyncFIFODevice extends ClockedDevice {
    private final Clock clk;
    private final SharedBus in;
    private final SharedBus out;

    private final BigInteger[] buf;
    private int nextOutput;

    /**
     * Class constructor.
     **/
    SyncFIFODevice(final String scope,
            final String name,
            final Clock clk,
            final SharedBus in,
            final SharedBus out,
            final int numStages) {

        super(scope, name);

        this.clk = clk;
        this.in = in;
        this.out = out;
        this.nextOutput = 0;
        this.buf = new BigInteger[numStages];

        for (int i = 0; i < buf.length; ++i)
            buf[i] = BigInteger.ZERO;
    }

    public void go() throws InterruptedException {
        System.out.println(Long.toBinaryString(-1));
        for (;;) {
            // wait for clock to rise
            clk.wait(SharedBus.POSEDGE);

            // send the next value to be output
            out.set(this, buf[nextOutput].longValue(), 1);

            // recieve the new value
            buf[nextOutput] = in.getBigIntegerData();

            // shuffle the values over
            nextOutput = (nextOutput + 1) % buf.length;
        }
    }
}
