/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.sigscan;

/**
 * Class for automatically logged doubles
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class LoggedInt extends LoggedVar {

    /** The wrapped primitive*/
    private int i=0;
    
    /**Constructor
     * @param scopename Scope to place this variable in
     * @param varname The name of the variable
     * @param sigscan The database to place this variable in
     * @param opts The debugging options for this variable
     **/
    public LoggedInt(String scopename,String varname,
                     Sigscan sigscan, DebugOpts opts) {
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
    public LoggedInt(String scopename,String varname,
                     Sigscan sigscan, DebugOpts opts,
                     boolean scheduled) {
        super(sigscan, (sigscan != null)?
                       sigscan.newIntVariable(scopename, varname):0,
                       opts,
                       scheduled);
    }

    /** @return The value of this variable **/
    public int get() { return i; }

    /** This sets the variable and logs immediately if scheduled, else
     * it adds a new logEvent to the scheduler.  Note this call will
     * block if scheduled is true
     * @param d The new value**/
    public void set(int i) {
        if (scheduled) set(i, getTime());
        else {
            this.i = i;
            log();
        }
    }

    /** Blocking, will set and wait until <code> time </code> 
     * @param d the new value
     * @param time The time to set the variable at and block until**/
    private void set(int i,long time) {
        this.i = i;
        setLog(time);  //Will block until logged
    }

    /** Logs the variable to the database **/
    public void log() { if ((handle != 0) &&
                            (opts.loggingVars()))
                            sigscan.intChange(handle, i); }
}

