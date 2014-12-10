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

public class SinVoltageSource extends VoltageSource{

//
//Voltage Imp variables
//
    /** Voltage or current offset in volts or amps. **/
    private double vo;

    /** Voltage or current RMS amplitude in volts or amps.**/
    private double va;

    /** Source frequency in Hz.  **/
    private double freq;
    /** Time delay before beginning the sinusoidal 
     *  variation in seconds.  Response will be 0 volts or amps
     *  until the delay value is reached, even with a non-zero 
     *  DC voltage.**/
    private double td;
    /** Damping factor in units of 1/seconds.  **/
    private double theta;
    /** Phase delay in units of degrees.  **/
    private double phi;

    
    /** Builds a sine voltage source device 
     *
     * @param name HierName of the device
     * @param nplus positive terminal node
     * @param nminus negative terminal node
     * @param vo Voltage offset in volts or amps.
     * @param va Voltage RMS amplitude in volts or amps.
     * @param freq Source frequency in Hz.
     * @param td Time delay before beginning the sinusoidal 
     *           variation in seconds.  Response will be 0 volts or amps
     *           until the delay value is reached, even with a non-zero 
     *           DC voltage.
     * @param theta Damping factor in units of 1/seconds.
     * @param phi Phase delay in units of degrees.
     * **/
    public SinVoltageSource(final HierName name,
                              final Node nplus,
                              final Node nminus,
                              final double vo,
                              final double va,
                              final double freq,
                              final double td,
                              final double theta,
                              final double phi) {
        super(name, nplus, nminus);
        this.va = va;
        this.vo = vo;
        this.td = td;
        this.freq = freq;
        this.theta = theta;
        this.phi = phi;
    }
    
    public double getDrivenVoltage(double time,double timestep) {
        if (time <= td) return vo+ va*Math.sin(2*Math.PI*phi/360d);
        return vo+ va *Math.exp((td-time)*theta)
                      *Math.sin(2*Math.PI*( freq*(time -td) + phi/360d ));
    }
}

