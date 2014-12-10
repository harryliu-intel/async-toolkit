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
 * Mimics the behavior of a <code>ALT_1of&lt;n&gt;</code>
 *
 * @author Kim Wallmark
 * @version $Date$
 **/

public class Alternator extends AbstractDevice {
    /** Output channel **/
    private ChannelOutput out;
    private boolean infinite = true;
    private int repeats = 0;
    private int scale;
    private int lastval = 1;

    public Alternator(String name, ChannelInput in[], 
			 ChannelOutput out[], boolean suppressOutput) {
	this.scale = 0;
	this.out = out[0];
    }

    /** constructor for infinite zeros **/
    public Alternator(int scale, ChannelOutput out) {
        if (scale > 0) {
            this.scale = scale;
        } else {
            this.scale = 0;
        }
        this.out = out;
    }

    /** constructor for finite zeros **/
    public Alternator(int scale, ChannelOutput out, int repeats) {
        this(scale, out);
        infinite = false;
        this.repeats = (repeats>0)?repeats:0;
    }

    /** main functionality **/
    public void go() throws InterruptedException {
	lastval = 1;
        if (infinite) {
            while (true) {
                long t = time+nextTime();
                System.out.println("sending at "+t);
                send(out, new Message((lastval = 1-lastval)));
                System.out.println("Sent a " + lastval);
            }
        }
        else {
            for (int i=0; i<repeats; i++) {
                long t = time+nextTime();
                System.out.println("sending at "+t);
                send(out, new Message((lastval = 1-lastval)));
                System.out.println("Sent a " + lastval);
            }
        }
    }

    private final long nextTime() {
        return scale;
    }
}




