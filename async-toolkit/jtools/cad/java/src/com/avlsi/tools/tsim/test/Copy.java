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
 * Mimics the behavior of a <code>COPY</code>
 *
 * @author Jacob Mandelson
 * @version $Date: 2002/01/28 $
 **/

public class Copy extends AbstractDevice {
    /** Input/Output channels **/
    private ChannelInput in;
    private ChannelOutput[] out;

    public Copy(String name, ChannelInput in[], 
			 ChannelOutput out[], boolean suppressOutput) {
	this.out = out;
	this.in = in[0];
    }

    /** main functionality **/
    public void go() throws InterruptedException {
        while (true) {
                Message t = receive(in);
		System.out.println("Got a message, sending.");
		for (int i = 0; i < out.length; i++)
		    send(out[i], t);
        }
    }
}




