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

package com.avlsi.tools.tsim.test.sync;

import com.avlsi.tools.tsim.*;

/**
 * Test of SyncSource feeding SyncSink.
 *
 * @author Jagmit Sandhu
 * @version $Revision$ $Date$
 **/
public class SyncTest {

    public static void main(String args[]) {

        final Clock clk = new Clock("bus", "clock", 3600, null);
        final SharedBus data = new SharedBus("bus", "data", 32, null);

        clk.start();
        new SyncSource(clk, data, "bus", "syncTest",
                Integer.MAX_VALUE).start();
        new SyncSink(clk, data, "bus", "syncTest",
                Integer.MAX_VALUE, 1).start();

        new SchedulerRunner().start();
    }
}
