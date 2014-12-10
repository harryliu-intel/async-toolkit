/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.DebugOpts;

/**
 * <p> Class for having a clock, which is just a <code>SharedBus</code>.  Self
 * oscillates and the first cycle is high (1).
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public class Clock extends SharedBus implements BusDriver {

    private final ClockDriver driver;

    public Clock(String scopename, String name,
                 Sigscan sigscan,DebugOpts opts,
                 int fullCycleTime, Data initVal) {
        super(scopename, name, sigscan, opts, 1);
        driver = new ClockDriver(this, fullCycleTime, initVal);
    }

    public Clock(String scopename, String name,
                 Sigscan sigscan,DebugOpts opts,
                 int fullCycleTime) {
        this(scopename, name, sigscan, opts, fullCycleTime, null);
    }
   
    /**
     *@param scopename scope where this clock lies in the design
     *@param name Name of the clock
     *@param fullCycleTime The clock cycle time in JavaChannel units
     *@param sigscan The logging database file, set to null to disable logging
     **/
    public Clock(String scopename, String name,
                 int fullCycleTime,Sigscan sigscan) {
        super(scopename, name, 1,sigscan);
        driver = new ClockDriver(this, fullCycleTime);
        getDebugOpts().setLogBuses(true);
    }

    public Clock(String scopename, String name,Sigscan sigscan) {
        super(scopename, name, 1,sigscan);
        driver = new ClockDriver(this, 0);
        getDebugOpts().setLogBuses(true);
    }

    /*Set the clock cycle*/
    public void setCycleTime(int cycleTime) {
        driver.setCycleTime(cycleTime);
    }
    
    /*Starts the clock oscillating high*/
    public void start() { start(0); }

    /* Some watchdog tests require the clock to be stopped */
    public void stop() {
        // TBD: we don't need an offset for stop() do we?
        driver.stop();
    }

    /* There's a grey area between started and stopped; we may need to know if
     * we're going to get any more transitions when we ask it to stop. */

    public boolean isRunning() {
        return driver.isRunning();
    }

    /*Starts the clock oscillating high
     * @param offset the offset time to start high in tsim units*/
    public void start(int offset) { driver.start(offset); }
    
    /**
     * @return The full cycle time 
     **/

    public int getCycleTime() { return driver.getCycleTime(); }

} // end of class Clock

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
