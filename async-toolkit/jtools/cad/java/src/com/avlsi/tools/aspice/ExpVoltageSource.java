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

package com.avlsi.tools.aspice;
import com.avlsi.file.common.HierName;
/**
 * Class for Sinusoidal Voltage Source
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class ExpVoltageSource extends VoltageSource{

//
//Voltage Imp variables
//
    /** Initial value of voltage or current in volts or amps. **/
    private double v1;

    /** Pulsed value of voltage or current in volts or amps. **/
    private double v2;

    /** Rise delay time in seconds. **/
    private double td1;
    /** Fall delay time in seconds. **/
    private double td2;
    /** Rise time constant in seconds.  **/
    private double tau1;
    /** Fall time constant in seconds.  **/
    private double tau2;

    
    /** Builds a exponential voltage source device 
     *
     * @param name HierName of the device
     * @param nplus positive terminal node
     * @param nminus negative terminal node
     * @param v1  Initial value of voltage in volts. 
     * @param v2  Pulsed value of voltage in volts. 
     * @param td1 Rise delay time in seconds. 
     * @param tau1  Rise time constant in seconds.
     * @param td2 Fall delay time in seconds.  
     * @param tau2  Fall time constant in seconds.
     * **/
    public ExpVoltageSource(final HierName name,
                              final Node nplus,
                              final Node nminus,
                              final double v1,
                              final double v2,
                              final double td1,
                              final double tau1,
                              final double td2,
                              final double tau2) {
        super(name, nplus, nminus);
        this.v1 = v1;
        this.v2 = v2;
        this.td1 = td1;
        this.td2 = td2;
        this.tau1 = tau1;
        this.tau2 = tau2;
    }
    
    public double getDrivenVoltage(double time,double timestep) {
        if (time <= td1) return v1;
        if (time <= td2) return v1 + (v2-v1)*(1-Math.exp(-(time-td1)/tau1) );
        return v1 + (v2-v1) * (1-Math.exp(-(td2-td1)/tau1))
                            * (Math.exp(-(time-td2)/tau2));
    }
}

