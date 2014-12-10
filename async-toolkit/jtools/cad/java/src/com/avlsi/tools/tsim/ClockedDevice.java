/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.LoggingObject;
import com.avlsi.tools.sigscan.DebugOpts;
import com.avlsi.tools.sigscan.SigscanException;
import com.avlsi.tools.sigscan.TransactionType;
import com.avlsi.tools.sigscan.Transaction;
import com.avlsi.tools.sigscan.TransactionFiber;
import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.WakeAt;

/**
 * <p> Class for writing synchronous devices. </p>
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public abstract class ClockedDevice extends LoggingObject
    implements Runnable, BusDriver {

    protected Thread thread;

    public long getTime() { return DigitalScheduler.get().getTime(); }
    /**
     * Implement this instead of run()
     **/
    protected abstract void go() throws InterruptedException;

    /**
     * Override this as an exit function for end of simulation.
     **/
    protected void stop() { return; }

    /**
     * Cleanup when an InterruptedException occurs.  Typically used to
     * destroy channels.
     **/
    protected void cleanup() {
        System.err.println("InterruptedException occurred. " +
                "Override cleanup() to handle.");
    }

    /**
     * Public, but shouldn't be called.
     * For the thread interfaces, basically.
     **/
    // Thread count: start needs to handle incrementing, so it doesn't happen
    // too late, but we can (and should) handle decrementing.
    public final void run() {
        try {
            go();
        } catch (InterruptedException e) {
            // System.out.println("Interrupting: ");
            // e.printStackTrace();
            cleanup();
            // System.out.println("cleanup done: ");
            // interrupts happen during waiting.
            // The other side was not able to wake us up, and so
            // *probably* hasn't incremented the thread count.
            DigitalScheduler.get().incThreadCount();
            // System.out.println("thread count bumped.");
        } finally {
            // We want to do this on normal exit.
            DigitalScheduler.get().decThreadCount();
        }
	stop();
    }

    /**
     * Wait until the device's go() exits.
     **/
    public final void join() {
        try {
            thread.join();
        } catch (InterruptedException e) {
        }
    }
    
    /**
     * Creates a new thread and starts it.
     *
     * Also adjusts counts on the digital scheduler.
     **/
    public final void start() {
        if (thread != null) return;
        DigitalScheduler.get().incThreadCount();
        thread = new Thread(this);
        thread.start();
    }

    /**
     * Resets and kills thread currently running in this device.
     *
     * To restart it, you'll have to create a new device.
     **/
    public final void interrupt() {
        if (thread != null) thread.interrupt();
    }

    public final void interruptDevice() { interrupt(); }
    
    /**Link the message's sender transaction to currTran, a global
     * Transaction**/
    public void link(Message m) { link(m, currTran); }
    
    /**Link the message's sender transaction to the Transaction tran**/
    public void link(Message m, Transaction tran) {
            Transaction sender = m.getSender();
            if (sender != null) {
                linkSuccessor(sender, tran);
            }
    }

    public void wakeAt(long newTime) throws InterruptedException{
        new WakeAt(newTime, new Object()).sleepTil();
    }

    public void wakeAt(double newTime, double units)
            throws InterruptedException {
        wakeAt(SharedBus.convert(newTime, units));
    }

    public void delayBy(long delayTime) throws InterruptedException{
        wakeAt(getTime()+delayTime);
    }

    public void delayBy(double delayTime, double units)
            throws InterruptedException {
        wakeAt(getTime()+SharedBus.convert(delayTime, units));
    }

    public void stopSimulation() {
        DigitalScheduler.get().addEvent(new ExitEvent(getTime()));
    }

    /**Passes in null sigscans and debugOpts
     **/
    public ClockedDevice(String scopename, String name) {
        this(scopename, name, null, null);
    }

    /**
     * Constructs a LoggingObject.  Will build the default fiber, and the
     * default state variable.
     * @param scopename The scope to place this object in
     * @param name The name of this object
     * @param sigscan The database to place this in (can be null)
     * @param opts The database options
     **/
    public ClockedDevice(String scopename, String name, 
                          Sigscan sigscan, DebugOpts opts) {
        super(scopename, name, sigscan, opts);
    }

    public boolean loggingSST() { 
        if (getDebugOpts() == null) return false;
        return getDebugOpts().loggingSST();
    }

} // end of abstract class ClockedDevice

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
