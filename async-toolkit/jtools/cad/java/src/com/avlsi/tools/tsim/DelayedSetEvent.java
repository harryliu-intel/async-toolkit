/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.SequencedEvent;
import com.avlsi.tools.dsim.DigitalScheduler;

/**
 * Class for Delaying set calls at time zero.
 * Because of the handshking involved in connecting Java to verilog,
 * VerilogSharedBuses cannot be initialized at time zero, because the
 * handles to these buses are acquired in the verilog during the verilog
 * simulation.  
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class DelayedSetEvent implements SequencedEvent {
    
    /** Used by SequencedEvent **/
    private SequencedEvent next=null;
    
    /** Used by EventQueue **/
    private int index = -1;
    /** Time in JavaChannel units **/
    protected long time;

    protected final SharedBus bus;
    
    protected BusDriver driver;
    protected final Data newdata;
    protected final long settime; 

    public boolean isRandom() { return false;}

    public void setIndex(int i) { index = i;}

    public int getIndex() { return index; }

    public long getTime() { return time; }

    public DelayedSetEvent(SharedBus bus,
                          BusDriver drv, Data setdata, long settime, long delay) {
        this.bus = bus;
        this.driver = drv;
        this.newdata = new Data(bus.getBusSize());
        this.newdata.setTo(setdata);
        this.settime = settime;
        this.time = DigitalScheduler.get().getTime()+delay;;
    }

    /** This will define the event's behavior. **/
    public final void fire() {
        bus.set(driver, newdata, settime);
    }

    public boolean hasNextEvent() { return next != null; }

    public SequencedEvent getNextEvent() { return next; }

    public void setNextEvent(final SequencedEvent se) { next = se; }
    
}

