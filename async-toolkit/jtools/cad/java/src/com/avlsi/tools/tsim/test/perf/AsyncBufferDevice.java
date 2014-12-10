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

import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;

/**
 * Device with one place of buffering.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class AsyncBufferDevice extends AbstractDevice {

    private final ChannelInput in;
    private final ChannelOutput out;

    public AsyncBufferDevice(final String name,
            final boolean suppressOutput,
            final ChannelInput in,
            final ChannelOutput out) {
        super(name, suppressOutput);

        this.in = in;
        this.out = out;
    }

    public void go() throws InterruptedException {
        for (;;) {
            send(out, receive(in).getValue());
        }
    }
}
