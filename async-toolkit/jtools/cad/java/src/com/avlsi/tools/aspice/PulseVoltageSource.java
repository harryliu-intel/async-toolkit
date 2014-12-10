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

package com.avlsi.tools.aspice ;
import java.util.Iterator;
import com.avlsi.file.common.HierName;
/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class PulseVoltageSource extends VoltageSource {
    
    /** Initial value of the voltage or current, before the 
     * pulse onset (units of volts).**/
    private double v1;
    /**Pulse plateau value (units of volts).**/
    private double v2;
    /** Delay time in seconds from the beginning of transient interval to
     * the first onset ramp.
     ***/
    private double td;
    /**Duration of the onset ramp in seconds, from the initial value to the pulse
     * plateau value (reverse transit time). Default=timestep).
     **/
    private double tr;
    /**Duration of the recovery ramp in seconds, from the pulse plateau back to 
     * the initial value (forward transit time). Default=timestep.
     **/
    private double tf;
    /**Pulse width (the width of the plateau portion of the pulse) in seconds. 
     * Default=timestep. **/
    private double pw;
    /**Pulse repetition period in seconds.  Default=timestep. **/
    private double per;

    /** Builds a pulse voltage source device 
     *
     * @param nplus positive terminal node
     * @param nminus negative terminal node
     * @param v1 trough voltage
     * @param v2 peak voltage
     * @param td delay at the beginning of the period before ramping up
     * @param tr time duration of ramp (rising edge)
     * @param tf time duration of falling edge
     * @param pw time duration of peak (at v2)
     * @param per period of pulse sequence
     * **/
    public PulseVoltageSource(final HierName name,
                              final Node nplus,
                              final Node nminus,
                              final double v1,
                              final double v2,
                              final double td,
                              final double tr,
                              final double tf,
                              final double pw,
                              final double per) {
        super(name, nplus, nminus);
        //System.out.println("New PVS: v1 "+v1+" v2 "+v2+" td "+td+" tr "+tr+
        //                   " tf "+tf+" pw "+pw+" per "+per);
        this.v1 = v1;
        this.v2 = v2;
        this.td =  td;
        this.tr =  tr;
        this.tf =  tf;
        this.pw =  pw;
        this.per = per;
    }

    private double rampUp(double t){ return v1 + (v2-v1)*(t/ tr); }
    
    private double rampDown(double t){ return v1 + (v2-v1)*(1 - (t / tf)); }
    
    public double getDrivenVoltage(double time,double timestep) {
        if (time <= td) return v1;
        double gotime = (time -td) % per; //Find which part of the period we're in
        if (gotime <  tr) return rampUp(gotime);
        if (gotime <= tr+pw) return v2;
        if (gotime <  tr+pw+tf) return rampDown(gotime-tr-pw);
        return v1;
    }
}
