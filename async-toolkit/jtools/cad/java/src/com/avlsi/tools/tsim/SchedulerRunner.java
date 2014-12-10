/*
 * Copyright 2001, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.DigitalScheduler;

/**
 * <p> This is a separate thread which just keeps calling <code>cycle()</code>.
 * You'll need this if you use any instances of <code>AccurateWait</code>. </p>
 *
 * @author Patrick Pelletier
 * @version $Revision$
 **/

public class SchedulerRunner implements Runnable {

    /**
     * Repeatedly calls <code>cycle()</code> forever and never exits.
     * @see DigitalScheduler#cycle
     **/

    public void run() {
	while (true) {
	    if (TSimDebug.DEBUG)
		System.err.println("Calling cycle()");
	    DigitalScheduler.get().cycle();
	}
    }

    /**
     * @generate Starts a new thread attached to <code>this</code> called
     * "Schedule Runner".
     **/

    public void start() {
	new Thread(this, "Scheduler Runner").start();
    }

} // end of class ScheduleRunner

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
