/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.sigscan;

import com.avlsi.tools.dsim.SequencedEvent;

import java.util.ArrayList;
import java.util.List;
import java.util.Collections;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;

/**
 * Class for Logging in TSim.  For the logging events to happen in the correct
 * order, all logging actions are done in the DigitalScheduler
 *
 * @author Dan Daly
 * @version $Date$
 **/

abstract class LogEvent implements SequencedEvent {

    //private static final Map events = Collections.synchronizedMap(new HashMap());

    //private List nextEvents = null;
    //
    private SequencedEvent next=null;
    
    /** Used by EventQueue **/
    private int index = -1;
    /** Time in JavaChannel units **/
    protected final long time;

    protected final Long ltime;

    /**Used when synchronization is done on the fiber **/
    protected boolean notifyFlag = false;
    
    //
    //Event interface methods
    //
    
    public boolean isRandom() { return false;}

    public void setIndex(int i) { index = i;
            /*System.out.println("Index set to "+index);*/}

    public int getIndex() { return index; }

    public long getTime() { return time; }
    
    /** The fire action body **/
    protected abstract void actionPerformed();
    
    /** This will define the event's behavior. **/
    public final void fire() {
        actionPerformed();
        /*
        if (nextEvents != null) {
            synchronized(this) {
                for(Iterator i=nextEvents.iterator();i.hasNext();) {
                    LogEvent le = (LogEvent) i.next();
                    //System.out.println("Performing "+le);
                    le.actionPerformed();
                }
                //Not going to bother to removeAllElements, let the
                //garbage collection eat it?
                nextEvents = null;
            }
        }
        events.remove(ltime);
        */
    }

    public void setNotifyFlag(boolean f) { notifyFlag = f; }
    
    /**Adds this event to the scheduler.  Use this call instead of using
     * com.avlsi.tools.dsim.DigitalScheduler.get().addEvent(Event e) because this
     * call will also be sure that the sequence order of LogEvents is preserved.
     * Our scheduler does not ensure sequence time order, this we add with this
     * function.
     ***/
    public void add() {
        /*
        if (notifyFlag == true) {
            //Don't do this do notifying events, it deadlocks
            //XXX Fixme make it so it doesn't deadlock
            com.avlsi.tools.dsim.DigitalScheduler.get().addEvent(this);
        } else {
            LogEvent ev = (LogEvent) events.get(ltime);
            if (ev != null) {
                //This means another event has been scheduled at this time
                ev.addNextEvent(this);
                //System.out.println("Linking @ "+time+" "+ev);
            } else {
                //If not, then schedule it
                //System.out.println("doSched @ "+time+" mapsize= "+events.size());
                events.put(ltime, this);*/
                com.avlsi.tools.dsim.DigitalScheduler.get().addEvent(this);
       /*     }
        }*/
    }

    /** Strings events that occur at the same simulation time together
     * so that their order is preserved.
     **/
    /*private void addNextEvent(LogEvent ev) {
        //System.out.println("Adding "+ev+"\nto "+this);
        synchronized(this) {
            if (nextEvents == null) nextEvents = 
                    Collections.synchronizedList(new ArrayList());
            nextEvents.add(ev);
        }
    }*/
    
    public boolean hasNextEvent() { return next != null; }

    public SequencedEvent getNextEvent() { return next; }

    public void setNextEvent(final SequencedEvent se) { next = se; }
    
    /**
     * Constructor.
     **/
    LogEvent(long time) {
        this.time = time;
        this.ltime = new Long(time);
        this.notifyFlag = false;
    }

    public String toString() { 
        StringBuffer buf = new StringBuffer(time+" : LogEvent ");
        if (hasNextEvent()) buf.append(getNextEvent().toString());
        return buf.toString();
    }
        
            
}

