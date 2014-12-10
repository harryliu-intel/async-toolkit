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

import com.avlsi.tools.dsim.DigitalScheduler;

/**
 * Class for automatically logged vars
 *
 * @author Dan Daly
 * @version $Date$
 **/

public abstract class LoggedVar extends NativeHandle {

    /**The logging options of the variable.  This object will
     * reference opts.loggingVars() */
    protected final DebugOpts opts;

    /**Is this variable being scheduled?**/
    protected final boolean scheduled;
    
    /** Database where this variable resides **/
    protected Sigscan sigscan;
    
    /** Constructor for logged variables
     * @param sigscan The database where the variable will reside
     * @param handle The native address of the variable
     * @param opts The debugging options for the variable
     * @param scheduled Is this variable scheduled?  (for non-universal
     * time simulatos like TSim)
     **/
    public LoggedVar(Sigscan sigscan, long handle,
                     DebugOpts opts,boolean scheduled) { 
        super(handle); 
        this.sigscan = sigscan;
        this.opts = opts;
        this.scheduled = scheduled;
    }

    /**Log the value of this variable at <code>time</code>
     * @param time The time to log the variable
     **/
    public void setLog(long time) {
        if (handle != 0) //If zero, logging has been turned off
            scheduleEvent(new LoggedVarEvent(this, time));
    }

    /**To be filled in by the subclasses - logs to the database **/
    public abstract void log();

    /** Called by a LoggingEvent to set the database time
     * and log the variable.  The time logged to the database
     * is the simulator time.
     * @param time not used, ignored
     **/
    public final void logEvent(long time) {
        if (sigscan != null) {
            try {
                sigscan.convertAndSetTime(getTime());
            } catch (SigscanException e) {
                System.err.println("LoggedVar: "+e.getMessage());
                sigscan.incErrorCount();
            }
            log();
            if (scheduled) doNotify();
        }
    }   

    /**
     * Used to schedule a LogEvent to the digital scheduler.  Note
     * that this call will block until the event fires and notifies.
     * @param ev The LogEvent to be scheduled
     **/
    public synchronized void scheduleEvent(LogEvent ev) {
        if (handle != 0) {
            ev.setNotifyFlag(true);
            DigitalScheduler.get().addEvent(ev);
            try {
                DigitalScheduler.get().decThreadCount();
                wait();
            } catch (InterruptedException e) {
                System.out.println("Interrupted.");
            }
        }
    }
    
    /**
     * Does the notification of the threads waiting on this object
     **/
    public synchronized void doNotify() {
        notify();
        DigitalScheduler.get().incThreadCount();
    }

    /** @return The currentTime on the scheduler **/
    protected long getTime() { return DigitalScheduler.get().getTime(); }
    
}

