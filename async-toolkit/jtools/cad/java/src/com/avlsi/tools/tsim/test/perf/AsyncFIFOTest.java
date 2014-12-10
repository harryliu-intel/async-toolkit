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

import com.avlsi.tools.tsim.BufferedChannel;

/**
 * Performance test of asynchronous FIFOs.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
class AsyncFIFOTest {
    public static void main(String[] args) {
        final boolean suppressOutput = true;

        if (args.length != 3) {
            System.err.println("Usage: " + AsyncFIFOTest.class.getName() +
                    " numTokens slack [ yes | no ]");
            System.exit(1);
        }

        final int numTokens = Integer.parseInt(args[0]);
        final int slack = Integer.parseInt(args[1]);

        final BufferedChannel ch1, ch2;
        if (args[2].startsWith("n")) {
            ch1 = ch2 = new BufferedChannel(Math.max(slack, 1));
        } else {
            ch1 = new BufferedChannel(Math.max(slack / 2, 1));
            ch2 = new BufferedChannel(Math.max(slack - slack / 2, 1));

            new AsyncBufferDevice("buf", suppressOutput, ch1, ch2).start();
        }

        new AsyncSourceDevice("source", suppressOutput, ch1, numTokens).start();
        new AsyncSinkDevice("sink", suppressOutput, ch2, numTokens).start();
    }
}
