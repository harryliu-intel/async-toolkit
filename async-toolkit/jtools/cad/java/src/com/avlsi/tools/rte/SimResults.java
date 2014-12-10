/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.rte;

import java.io.*;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * This class allows us to communicate the results of a simulation
 * run back to the server. It contains all the details we might
 * be interested in about a simulation.
 **/
public class SimResults implements Serializable {

    /** class constants **/
    public static final int PASSED = 0x1;
    public static final int FAILED = 0x0;
    public static final int NOTTESTED = 0xff;
    public static final int DIGITAL = 0x0;
    public static final int CSPCOSIM = 0x1;
    public static final int JAVACOSIM = 0x2;
    
    
    /** data members **/
    private int error;
    private int simtype;
    private long duration;
    private boolean csperror;
    private boolean fragment;
    private boolean synchronous;
    private boolean unimplementable;
    private boolean notests;
    private boolean rtexception;
    private boolean ntpcspec;
    private boolean ntpcstat;
    private boolean ntpcstated;
    
    private boolean rte_ignore;
    private String rte_ignore_str;
    private boolean cosim_will_fail;
    private String cosim_will_fail_str;

    private boolean cyclenode;
    private double coverage;
    
    /**
     * Constructor  
     */
    public SimResults() { 
	//innocent till proven guilty!
	this.error = NOTTESTED; 
	this.simtype = DIGITAL;
	this.csperror = false;
	this.fragment = false;
	this.synchronous = false;
	this.unimplementable = false;
	this.notests = false;
	this.rtexception = false;
	this.cyclenode = false;
	this.ntpcspec = false;
	this.ntpcstat = true;
        this.ntpcstated = true;
	this.coverage = 0.0;
	this.cosim_will_fail = false;
	this.rte_ignore = false;
	this.cosim_will_fail_str = "";
	this.rte_ignore_str = "";
	this.duration = 0;
    }

    /**
     * set the simulation results for this class
     */
    public void setResult(int error){
	this.error = error;
    }
    
    /** 
     * returns true if the cell is not testable 
     **/
    public boolean isTestable(){
        if(this.unimplementable || this.fragment)
	    return false;
	if(this.rte_ignore && this.rte_ignore_str.equals("; __CELL__"))
	    return false;
	return true;
    }

    /**
     * return true if error == PASSED
     */
    public boolean simPassed(){
	return this.error == PASSED;
    }
    
    /**
     * returns true if error == FAILED
     */
    public boolean simFailed(){
	return this.error  == FAILED;
    }

    /**
     * set coverage information here
     */
    public void setCoverage(double cov){
	this.coverage = cov;
    }

    /**
     * return the coverage information for this cell
     */
    public double getCoverage(){
	return this.coverage;
    }

    /**
     * 
     */

    /**
     * set the csp error flag on this cell
     */
    public void setCSPError(boolean csperr){
	this.csperror = csperr;
    }
    
    /**
     * returns the csp error flag for given cell
     */
    public boolean cspError(){
	return this.csperror;
    }

    /**
     * set fragment field in results
     */
    public void setFragment(boolean frag){
	this.fragment = frag;
    }
    
    /**
     * returns true if cell has fragment directive and false otherwise
     */
    public boolean isFragment(){
	return this.fragment;
    }

    /**
     * set the synchronus field 
     */
    public void setSynchronous(boolean sync){
	this.synchronous = sync;
    }

    /**
     * returns the synchronus field 
     */
    public boolean isSynchronous(){
	return this.synchronous;
    }

    /**
     * set the unimplementable field
     */
    public void setUnimplementable(boolean unimpl){
	this.unimplementable = unimpl;
    }

    /**
     * returns the value for the unimplementable field
     */
    public boolean isUnimplementable(){
	return this.unimplementable;
    }

    /**
     * sets the notests to passed values
     */
    public void setMissingTest(boolean notests){
	this.notests = notests;
	setCycleNode(true);
    }

    /**
     * returns values of notests 
     */
    public boolean missingTest(){
	return this.notests;
    }

    /**
     * set runtime exception field to the passed value
     */
    public void setRuntimeException(boolean rtexception){
	this.rtexception = rtexception;
    }
    
    /**
     * accessor for rteexception
     */
    public boolean runtimeException(){
	return this.rtexception;
    }

    /**
     * set the ntpcsepc flag
     */
    public void setNTPCspec(boolean ntpcspec){
	this.ntpcspec |= ntpcspec;
    }

    /**
     * return the value of the ntpc spec field
     */
    public boolean hasNTPCspec(){
        //ntpc spec is interesting only if cell is testable
        return (this.ntpcspec || !isTestable());
    }

    /**
     * set the cyclenode flag
     */
    public void setCycleNode(boolean cyclenode){
	this.cyclenode = cyclenode;
    }

    /**
     * return the value of cyclenode, this routine should be called 
     * only after fragment and synchronous flags have been appropirate
     */
    public boolean hasCycleNode(){
        //cycle_node is interesting only if cell is testable
	return (this.cyclenode||!isTestable());
    }
    
    /**
     * set the type of simulation we're performing
     **/
    public void setSimType(int simtype){
	this.simtype = simtype;
    }

    /**
     * get sim type the cell was simulated with 
     **/
    public int getSimType(){
	return this.simtype;
    }
    
    /**
     * set the measure ntpc for final statistics 
     **/
    public void setNTPCTarget(boolean ntpcstat){
	this.ntpcstat &= ntpcstat;
    }
    
    /**
     * set the measure ntpc for final statistics 
     **/
    public void setNTPCTargetModifiedDelay(boolean ntpcstated){
	this.ntpcstated &= ntpcstated;
    }
    

    /**
     * return the value we set for the ntpc target
     **/
    public boolean getNTPCTarget(){
	return this.ntpcstat;
    }
    
    /**
     * return the value we set for the ntpc target
     **/
    public boolean getNTPCTargetModifiedDelay(){
	return this.ntpcstated;
    }
    
    /**
     * return a string which completely describes the results of the
     * simulation.
     */
    public String getResultStr(){
	if(error == FAILED)return "Failed";
	else if(error == PASSED)return "Passed";
	else if(error == NOTTESTED)return "Not Tested";
	else return "Fatal";
    }

    /**
     * Return a string with detailed results analysis
     */
    public String getResultDetail(){
	String details = getResultStr();
	
	if(rtexception)
	    details += "/Exception";
	return details;
    }
    
    /** Set and retrive RTE run settings ***/
    public void setRTEIgnore(boolean ignore, String env_cell){
	if(ignore) this.rte_ignore_str +="; " +env_cell;
	this.rte_ignore |= ignore;
    }
    
    public void setCosimWillFail(boolean cosim_will_fail, String env){
	if(cosim_will_fail) this.cosim_will_fail_str +="; " +env;
	this.cosim_will_fail |= cosim_will_fail;
    }
    
    public boolean hasRTEIgnore(){return this.rte_ignore; }
    public boolean hasCosimWillFail(){return this.cosim_will_fail; }
    
    public String getRTEIgnoreStr(){return this.rte_ignore_str; }
    public String getCosimWillFailStr(){return this.cosim_will_fail_str; }

    public void setDuration(long duration){
	this.duration = duration;
    }
    public long getDuration(){return this.duration; }

    /** print all the simulation results for this cell ***/
    public String toString(){
	String s = "\nCell Result Summary \n";
	if(error == PASSED) s += "Status: PASSED\n";
	else if(error == FAILED) s += "Status: FAILED\n";
	else if(error == NOTTESTED) s += "Status: NOTTESTED\n";
	
	s += "CSPERROR: " +csperror +"\n";
	s += "isTestable(): "+isTestable() +"\n";
	s += "FRAGMENT: " +fragment +"\n";
	s += "SYNCHRONOUS: " +synchronous +"\n";
	s += "UNIMPLEMENTABLE: " +unimplementable +"\n";
	s += "NOTESTS: " +notests +"\n";
	s += "RTEXCEPTION: " +rtexception +"\n";
	s += "NTPC_SPEC: " +ntpcspec +"\n";
	s += "NTPC_STAT: " +ntpcstat +"\n";
        s += "NTPC_STAT_ESTIMATED_DELAY: " +ntpcstated +"\n";
	s += "RTE_IGNORE: "+rte_ignore +"\n";
	s += "RTE_IGNORE_STR: " +rte_ignore_str +"\n";
	s += "COSIM_WILL_FAIL: "+cosim_will_fail +"\n";
	s += "COSIM_WILL_FAIL_STR: " +cosim_will_fail_str +"\n";
	s += "CYCLE_NODE: " +cyclenode +"\n";
	s += "COVERAGE: " +coverage +"%\n";
	
	return s;
	
    }
}
