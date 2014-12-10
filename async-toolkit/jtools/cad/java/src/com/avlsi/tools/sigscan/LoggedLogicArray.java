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
public class LoggedLogicArray extends LoggedVar {

    private byte[] value;

    /**Constructor
     * @param scopename Scope to place this variable in
     * @param varname The name of the variable
     * @param sigscan The database to place this variable in
     * @param opts The debugging options for this variable
     **/
    public LoggedLogicArray(String scopename,String varname, int lsb, int msb,
                            Sigscan sigscan,DebugOpts opts) {
        this(scopename, varname, lsb, msb, sigscan, opts, false);
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
    public LoggedLogicArray(String scopename,String varname, int lsb, int msb,
                            Sigscan sigscan,DebugOpts opts,boolean scheduled) {
        super(sigscan, sigscan != null ?
                       sigscan.newLogicArrayVariable(scopename, varname, lsb,
                                                     msb) : 0,
                       opts,
                       scheduled);
        this.value = new byte[Math.abs(msb - lsb) + 1];
    }

    /** @return The value of this variable **/
    public byte[] get() { return value; }

    /** This sets the variable and logs immediately if scheduled, else
     * it adds a new logEvent to the scheduler.  Note this call will
     * block if scheduled is true
     * @param d The new value**/
    public void set(byte[] value) {
        if (scheduled) set(value,getTime()); 
        else {
            System.arraycopy(value, 0, this.value, 0, this.value.length);
            log();
        }
    }

    /** Blocking, will set and wait until <code> time </code> 
     * @param d the new value
     * @param time The time to set the variable at and block until**/
    public void set(byte[] value,long time) {
        System.arraycopy(value, 0, this.value, 0, this.value.length);
        setLog(time);
    }

    /** Logs the variable to the database **/
    public void log() { if ((handle != 0) &&
                            (opts.loggingVars()))
                            sigscan.logicArrayChange(handle, value); }
}
