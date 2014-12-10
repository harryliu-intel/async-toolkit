/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id:$
 */

package com.avlsi.tools.jfast;

import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.Event;

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date:$
 **/

public abstract class AbstractDevice implements Event{

    protected static final int METHMASK = 1<<15;
    /** For the eventqueue, -1 is not queued **/
    private int index=-1;

    /** The next time we should fire **/
    private long nextTime=0;
    
    /** The next program point to start at **/
    protected int pc=0;
   
    private final InterruptedException intex;
    /**
     * Constructor.
     **/
    public AbstractDevice(String name, boolean suppressOutput) {
        intex = new InterruptedException(name+"'s interrupt");
    }

    public void Wait(long t) throws InterruptedException {
        this.nextTime = DigitalScheduler.get().getTime()+t;
        pc++;
        DigitalScheduler.get().addEvent(this);
        throw intex;
        //return true;
    }
    
    public void fire() {
        index = -1;
        try {
            go();
        } catch (InterruptedException e) {
            //This means the module has context switched
            //System.out.println("Caught "+e.getMessage());
        }
    }

    public abstract void go() throws InterruptedException;
   
    public void start() { DigitalScheduler.get().addEvent(this); }
    
    /** Returns current index as set by the scheduler. **/
    public int getIndex() { return index; }
    /** Sets current index (by the scheduler). -1 indicates not scheduled. **/
    public void setIndex(int _index) { 
        index = _index;
    }
    /** Returns the next time that we should fire. **/
    public long getTime() { return nextTime; }
    /** Returns whether we should fire at a random time. **/
    public boolean isRandom() { return false; }
    /** sets whether we should fire at a random time. **/

}

