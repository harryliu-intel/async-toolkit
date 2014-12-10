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
 **/
public class ClockDriver implements BusWatcher, BusDriver {

    /** The bus to drive the clock on **/
    private final SharedBus bus;
    
    private int ck=-1;

    private int lowtime=-1;
    private int hightime=-1;
    private boolean stopped = false;    // request
    private boolean running = false;    // status

    public ClockDriver(SharedBus bus, int fullCycleTime, Data initVal) {
        this.bus = bus;
        ck = fullCycleTime;
        this.lowtime  = ck / 2;
        this.hightime = ck - lowtime;   // if ck is odd, hightime = lowtime + 1
        if (initVal != null) {
            System.out.println("Driving Initial Value "+initVal+" on "+bus.getFullname());
            bus.set(this, initVal, 0);
        }
    }
    
    /**
     *@param bus The SharedBus to drive this clock on 
     *@param fullCycleTime The clock cycle time in JavaChannel units
     **/
    public ClockDriver(SharedBus bus, int fullCycleTime) {
        this(bus, fullCycleTime, null);
    }

    /*Set the clock cycle*/
    public void setCycleTime(int cycleTime) {
        this.ck = cycleTime;
        this.lowtime  = ck / 2;
        this.hightime = ck - lowtime;   // if ck is odd, hightime = lowtime + 1
    }

    /**
     * @return The full cycle time 
     **/
    public int getCycleTime() { return ck; }

    /*Starts the clock oscillating high*/
    public void start() { start(0); }

    public void stop() { 
        stopped = true; 

        // yeah, this may be a lie, but it lets the assertion in
        // busChanged() below work...
        running = true;     
    }

    public boolean isRunning() {
        return running;
    }

    /*Starts the clock oscillating high
     * @param offset the offset time to start high in tsim units*/
    public void start(int offset) {
        bus.addWatch(this);
        stopped = false;
        if (hightime <=0 || lowtime <=0) {
            System.out.println("Clock "+bus.getFullname()+
                               " Not Started, clock period == 0");
        } else {
            System.out.println("Starting "+bus.getFullname());
            bus.set(this, 0, offset);
        }
    }
    
    public void busChanged(SharedBus bus, long time) {
        if (stopped) {
            // make sure we don't transition again, (i.e. that we're really
            // stopped.
            assert(running);

            if (bus.getBooleanData()) {     // high
                bus.set(this, 0, hightime); // force low
            }
            else {
                running = false;
            }
            return;
        }
        else {
            running = true;
        }

        if (bus.getBooleanData()) {
            bus.set(this, 0, hightime);
        } else {
            bus.set(this, 1, lowtime);
        }
    }

    public void addWatch(BusWatcher watcher) { bus.addWatch(watcher); }

    public SharedBus getBus() { return bus; }
}
