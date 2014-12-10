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

package com.avlsi.tools.dsim;

/**
 * Wakes all threads waiting on <code>wait</code>.
 * Should perhaps be provided by Digital Scheduler for
 * general use.  Perhaps should also add wait method that
 * reliably sleeps until the time given.
 * @author Aaron Denney
 * @version $Date$
 **/
public class WakeAt implements Event {
    private final Object wait;
    private final long  time;

    private int     index = -1;
    private boolean fired = false;

    public WakeAt(long time, Object wait) {
        this.time = time;
        this.wait = wait;
    }

    public long getTime() {
        return time;
    }

    public int getIndex() {
        return index;
    }

    public void setIndex(int index) {
        this.index = index;
    }

    public void fire() {
        DigitalScheduler.get().incThreadCount();
        synchronized (wait) {
            fired = true;
            wait.notify();
        }
    }

    public boolean isRandom() {
        return false;
    }

    public void sleepTil() throws InterruptedException {
        try {
            synchronized (wait) {
                DigitalScheduler.get().addEvent(this);
                DigitalScheduler.get().decThreadCount();
                while (!fired) {
                    wait.wait();
                }
            }
        } catch (InterruptedException e) {
            DigitalScheduler.get().removeEvent(this);
            throw e;
        }
    }
}

