/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id: $
 */

package com.avlsi.tools.tsim.test;

import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.Message;

/**
 * Mimics the behavior of a <code>BUF_1of&lt;n&gt;</code>
 *
 * @author Jacob Mandelson
 * @version $Date: 2002/01/28 $
 **/

public class Buffer extends AbstractDevice {
    /** Input/Output channels **/
    private ChannelInput in;
    private ChannelOutput out;
    private String name;

    public Buffer(String name, ChannelInput in[], 
			 ChannelOutput out[], boolean suppressOutput) {
	this.out = out[0];
	this.in = in[0];
	this.name = name;
    }

    /** main functionality **/
    public void go() throws InterruptedException {
        while (true) {
                Message t = receive(in);
		System.out.println(name + "Got a message, sending.");
		send(out, t);
        }
    }
}




