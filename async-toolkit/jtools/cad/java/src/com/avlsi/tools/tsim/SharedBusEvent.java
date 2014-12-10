/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.SequencedEvent;

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public final class SharedBusEvent implements SequencedEvent {

    private SequencedEvent next=null;
    
    /** Used by EventQueue **/
    private int index = -1;
    /** Time in JavaChannel units **/
    private long time;

    private final SharedBus bus;
    
    private BusDriver driver;
    private final Data newdata;

    public boolean isRandom() { return false;}

    public void setIndex(int i) { index = i; }

    public int getIndex() { return index; }

    public long getTime() { return time; }

    /**
     * Constructor.
     **/
    public SharedBusEvent(SharedBus bus) {
        this.bus = bus;
        this.newdata = new Data(bus.getBusSize());
    }

    public SharedBusEvent(SharedBus bus,
                          BusDriver drv, Data newdata, long time) {
        this(bus);
        set(drv, newdata, time);
    }

    /** This will define the event's behavior. **/
    public final void fire() {
        bus.recordChange(driver, newdata, time);
    }

    public void set(BusDriver drv, Data setdata, long time) {
        this.driver = drv;
        this.newdata.setTo(setdata);
        this.time = time;
    }
    
    public boolean hasNextEvent() { return next != null; }

    public SequencedEvent getNextEvent() { return next; }

    public void setNextEvent(final SequencedEvent se) { next = se; }

    public String toString() { 
        return "SharedBusEvent @ "+time+ " (dsim) bus= "+
               bus.getFullname()+ " = "+newdata+" ind= "+index;
    }

}

