/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.sigscan;

/**
 * Class for sigscan logging options.
 *
 * @author Dan Daly
 * @version $Date$
 **/

import com.avlsi.util.exception.AssertionFailure;

public class DebugOpts implements Cloneable {

    /**Used by TSim objects, will be cached by TSim at instancing.
     * Default = false**/
    private boolean log_screen = false;

    //
    //SST Logging Vars
    //
    
    /**Used by output to log messages to the database.  Default=false*/
    private boolean log_msgs = false;
    
    /**Used by LoggedVar for outputting.  Default = false*/
    private boolean log_vars = false;

    /**Used by fibers to log their transactions to the database.  Default = false */
    private boolean log_trans = false;
   
    /**Used by Sharedbuses to tell them to log. Default = false **/
    private boolean log_buses = false;
    
    /*Switch to temporarily disable logging*/
    protected boolean loggingEnable = true;
    
    /**
     * Default Constructor, sets all logging OFF
     **/
    public DebugOpts() { }

    /** Sets alll logging OFF except logging to the screen set by log_screen
     * @param log_screen Log to the screen?
     **/
    public DebugOpts(boolean log_screen) {
        this.log_screen = log_screen;
    }

    /**Turns all Signalscan logging ON, NOT including writing to the screen
     * @return itself, which is nice for passing into constructors*/
    public synchronized DebugOpts logAllSST() {
        log_msgs= true;
        log_vars = true;
        log_trans = true;
        log_buses = true;
        return this;
    }

    /**Test to see if logging is enabled, including suspension
     * @return true if any Signalscan logging turned on*/
    public synchronized boolean loggingSST() { 
        return ( loggingEnable && (log_msgs || log_vars || log_trans));
    }

    /**Test to see if logging is enabled, including suspension
     * @return true if any Signalscan logging turned on*/
    public synchronized boolean loggingSignalscan() { return loggingSST(); }
   
    /** Set to log to the screen or not 
     * @param l set to true if logging to the screen enabled**/
    public synchronized void setLogScreen(boolean l) { log_screen = l; }
    
    /** Set to have Signalscan log messages or not.
     * @param l set to true if logging messages to Signalscan**/
    public synchronized void setLogMsgs(boolean l) { log_msgs = l; }
    
    /** Set to log to the screen or not.
     * Checked by LoggedVars to see if their logging is enabled
     * @param l set to true if LoggedVars should log to Signalscan **/
    public synchronized void setLogVars(boolean l) { log_vars = l; }
    
    /** Set to log transactions to the database
     * @param l Set to true to log transactions to database**/
    public synchronized void setLogTrans(boolean l) { log_trans = l; }
    
    /** Set to log buses to the database
     * @param b Set to true to log buses to database**/
    public synchronized void setLogBuses(boolean b) { log_buses = b; }
    
    
    /** @return Am I logging to the screen right now? */
    public synchronized boolean loggingScreen() {
        return loggingEnable && log_screen;
    }
    
    /** @return Am I logging messages right now? **/
    public synchronized boolean loggingMsgs() {
        return loggingEnable && log_msgs;
    }

    /** @return Am I logging variables right now? */
    public synchronized boolean loggingVars() {
        return loggingEnable && log_vars;
    }

    /** @return Am I logging transactions right now? */
    public synchronized boolean loggingTrans() {
        return loggingEnable && log_trans;
    }

    /** @return Am I logging buses right now? */
    public synchronized boolean loggingBuses() {
        return loggingEnable && log_buses;
    }

    /**  This can be used as a global toggle for the debug opts, which will turn
     * all logging off or re-enable all logging that has been turned on
     * @return Set the logging enable */
    public synchronized void setLoggingEnable(boolean e) {
        loggingEnable = e;
    }

    
    /** Clone this DebugOpts
     * @param log_screen Set log screen of the new DebugOpts
     * @return a copy of the opts
     **/
    public synchronized DebugOpts clone(boolean log_screen) {
        DebugOpts o = (DebugOpts) clone();
        o.setLogScreen(log_screen);
        return o;
    }

    /** Clone this DebugOpts
     * @return a copy of the opts
     **/
    public synchronized Object clone() {
        try {
            return super.clone();
        } catch (CloneNotSupportedException e) {
            throw new AssertionFailure(e);
        }
    }
    
}

