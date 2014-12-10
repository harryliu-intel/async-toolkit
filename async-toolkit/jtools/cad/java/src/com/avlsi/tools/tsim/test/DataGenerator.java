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
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.Message;

/**
 * Mimics the behavior of a <code>BGZ_1of&lt;n&gt;</code>, but can be
 * told to produce a finite or infinite number of zeros.  Could be
 * modified to produce other data.
 *
 * @author Kim Wallmark
 * @version $Date$
 **/

public class DataGenerator extends AbstractDevice {
    /** Output channel **/
    private ChannelOutput out;
    private boolean infinite = true;
    private int repeats = 0;
    private int scale;

    public DataGenerator(String name, ChannelInput in[], 
			 ChannelOutput out[], boolean suppressOutput) {
	this.scale = 0;
	this.out = out[0];
    }

    /** constructor for infinite zeros **/
    public DataGenerator(int scale, ChannelOutput out) {
        if (scale > 0) {
            this.scale = scale;
        } else {
            this.scale = 0;
        }
        this.out = out;
    }

    /** constructor for finite zeros **/
    public DataGenerator(int scale, ChannelOutput out, int repeats) {
        this(scale, out);
        infinite = false;
        this.repeats = (repeats>0)?repeats:0;
    }

    /** main functionality **/
    public void go() throws InterruptedException {
        if (infinite) {
            while (true) {
                long t = time+nextTime();
                System.out.println("sending at "+t);
                send(out, new Message(0));
                System.out.println("Sent a zero");
            }
        }
        else {
            for (int i=0; i<repeats; i++) {
                long t = time+nextTime();
                System.out.println("sending at "+t);
                send(out, new Message(0));
                System.out.println("Sent a zero");
            }
        }
    }

    private final long nextTime() {
        return scale;
    }
}




