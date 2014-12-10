/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $id: $
 */

package com.avlsi.tools.tsim.test;

import com.avlsi.tools.tsim.BufferedChannel;
import com.avlsi.tools.tsim.ChannelIO;

/** Tests the throughput of a looped channel in tsim with some tokens
 * flowing through it.
 **/

public class TestLoop {
    private static int numTokens;
    private static int loopSize;
    private static int numRelays;
    private static ChannelIO loop;
    private static TokenRelay relay;

    public static void main(String [] args) {

        for (int i=0; i<args.length; i++) {
            if (args[i].equals("-n") && i<(args.length-1)) {
                numTokens = Integer.decode(args[i+1]).intValue();
            } else if (args[i].equals("-l") && i<(args.length-1)) {
                loopSize = Integer.decode(args[i+1]).intValue();
            } else if (args[i].equals("-r") && i<(args.length-1)) {
                numRelays = Integer.decode(args[i+1]).intValue();
            }
        }

        // Sanity checks
        if (loopSize < 1) {
            System.err.println("Loop size "+loopSize+" is too small");
            return;
        }
        if (numTokens > loopSize) {
            System.err.println("Too many tokens for the loop");
            return;
        }

        loop = new BufferedChannel("loop", loopSize);
        relay = new TokenRelay("relay", loop, loop, numTokens, numRelays*numTokens);
        relay.start();
        return;
    }
}
        
