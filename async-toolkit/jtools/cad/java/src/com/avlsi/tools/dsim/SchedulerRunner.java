/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id: 
 */

package com.avlsi.tools.dsim;


/**
 * This is a separate thread which just keeps calling cycle().  You'll
 * need this if you use any AccurateWaits or using signalscan.  SchedulerRunner
 * is a singleton, just like the objects (DSim and DigSched) it controls.  You
 * can disable this thread before it is started by setting setEnable(false);
 *
 * @author Dan Daly
 */

public class SchedulerRunner implements Runnable {

    private SchedulerRunner() {
        cycler = new Thread(this, "Scheduler Runner");
    }

    private static SchedulerRunner runner = null;
    private final Thread cycler;
    private boolean running = false;
    private boolean enable = true;
    private long limit=-1;
    
    public static SchedulerRunner get() {
        if (runner == null) {
            runner = new SchedulerRunner();
        }
        return runner;
    }

    /** Method to enable or disable subsequent calls to the start method
     * Does not 'stop' the thread if it has already began running
     * @param enable Set to false to disable the thread
     **/
    public synchronized void setEnable(boolean enable) {
        this.enable = enable;
    }
       
    /**
     * Sets the number of cycles for the scheduler to run through.  Set to -1
     * to disable cycle limits.
     **/
    public synchronized void setCycleLimit(long limit) {
        this.limit = limit;
    }
    
    public synchronized void start() {
        if ((!running) && (enable)) cycler.start();
    }
   
    public synchronized boolean isRunning() {
        return running;
    }
    
    public void run() {
        running = true;
        while (true) {
            //System.out.println("Cycling.");
            if (limit < 0) {
                DigitalScheduler.get().cycle();
            } else {
                DigitalScheduler.get().cycleTo(limit);
            }
        }
    }

}
