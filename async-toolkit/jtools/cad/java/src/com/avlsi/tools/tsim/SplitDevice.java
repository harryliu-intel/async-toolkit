/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.Message;

/**
 * @todo Undocumented
 *
 * @author jlm
 * @author Kim Wallmar
 * @author Dan Daly
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public class SplitDevice extends AbstractDevice {

    protected ChannelInput I;
    protected ChannelOutput[] outputs;

    public SplitDevice(String name, ChannelInput I,
                       ChannelOutput[] outputs, boolean suppressOutput) {
        super(name, suppressOutput);
	this.I = I;
        this.outputs = outputs;
    }

    public void go() throws InterruptedException {
        final boolean suppressOutput = isOutputSuppressed();
        final int numOutputs = outputs.length;
        for (;;) {
            setState("Waiting for input on channel " + I.getName());
            Message m = receive(I);
            if (!suppressOutput)
                output("Received message " + m);

            long t0 = Long.MIN_VALUE;
            long maxTime = Long.MIN_VALUE;
            for (int i = 0; i < numOutputs; ++i) {
                final ChannelOutput out = outputs[i];
                setState("Sending on channel " + out.getName());
                long time1 = out.send(m, time);

                if (i == 0)
                    t0 = time1;
                else if (!suppressOutput) {
                    if (time1 != t0)
                        output("Time discrepancy " + time1 + " vs. " + t0);
                    else
                        output("No time discrepancy");
                }

                if (time1 > maxTime)
                    maxTime = time1;
            }

            updateTime(maxTime);
        }
    }

    protected void cleanup() {
        I.destroy(); I=null;
        for (int i = 0; i < outputs.length; ++i) {
            outputs[i].destroy();
            outputs[i] = null;
        }
        super.cleanup();
    }

} // end of class SplitDevice

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */

