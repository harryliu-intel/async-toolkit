/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.sigscan;

/**
 * Class for automatically logged longs
 *
 * @author Dan Daly
 * @version $Date$
 **/
public class LoggedLong extends LoggedVar {

    private long l=0;

    /**Constructor
     * @param scopename Scope to place this variable in
     * @param varname The name of the variable
     * @param sigscan The database to place this variable in
     * @param opts The debugging options for this variable
     **/
    public LoggedLong(String scopename,String varname,
                      Sigscan sigscan,DebugOpts opts) {
        this(scopename, varname, sigscan, opts, false);
    }

    /**Constructor for non-universally timed simulators such as TSim.
     * If variable changes have to be scheduled to ensure
     * variable chronological order, then set scheduled to true
     * @param scopename Scope to place this variable in
     * @param varname The name of the variable
     * @param sigscan The database to place this variable in
     * @param opts The debugging options for this variable
     * @param scheduled Set to true for TSim
     **/
    public LoggedLong(String scopename,String varname,
                      Sigscan sigscan,DebugOpts opts,boolean scheduled) {
        super(sigscan, (sigscan != null)?
                       sigscan.newLongVariable(scopename, varname):
                       0,
                       opts,
                       scheduled);
    }

    /** @return The value of this variable **/
    public long get() { return l; }

    /** This sets the variable and logs immediately if scheduled, else
     * it adds a new logEvent to the scheduler.  Note this call will
     * block if scheduled is true
     * @param d The new value**/
    public void set(long l) {
        if (scheduled) set(l,getTime()); 
        else {
            this.l = l;
            log();
        }
    }

    /** Blocking, will set and wait until <code> time </code> 
     * @param d the new value
     * @param time The time to set the variable at and block until**/
    public void set(long l,long time) {
        this.l = l;
        setLog(time);
    }

    /** Logs the variable to the database **/
    public void log() { if ((handle != 0) &&
                            (opts.loggingVars()))
                            sigscan.longChange(handle, l); }
}

