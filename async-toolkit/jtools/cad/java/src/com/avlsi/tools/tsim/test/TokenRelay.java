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

import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.Message;

/** Relays tokens from one side to the other.  Starts by producing
 * some number of tokens (like a hardware token buffer).
 **/

class TokenRelay extends AbstractDevice {
    private final String name;
    private final ChannelInput in;
    private final ChannelOutput out;
    private final int numTokens;
    private final int numRelays;
    private final boolean suppressOutput = true;

    TokenRelay (String name, ChannelInput in, ChannelOutput out,
                int numTokens, int numRelays) {
        this.name = name;
        this.in = in;
        this.out = out;
        this.numTokens = numTokens;
        this.numRelays = numRelays;
    }

    public void go() throws InterruptedException {

        // Spit out initial tokens
        for (int i = 0; i < numTokens; i++) {
            output("sending initial token: "+i);
            send(out, i);
        }

        // Relay
        Message temp;
        int cycleTime = 0;
        int lastTime = 0;           // Last time a 0 was received.
        for (int i = 0; i < numRelays; i++) {
            temp = receive(in);
            /*            if (temp.intValue() == 0) {
                cycleTime = time - lastTime;
                lastTime = time;
                } */

            output("relaying message: "+temp.getValue());
            send(out, temp);
            output("relayed");
        }

        if (isOutputSuppressed()) {
            //            double throughput = 100000.0*numTokens/cycleTime; // scaled to not be tiny
            double throughput = 100000.0*numRelays/time;
            System.out.println(numTokens+" "+throughput);
        }
        return;
    }

}

        
