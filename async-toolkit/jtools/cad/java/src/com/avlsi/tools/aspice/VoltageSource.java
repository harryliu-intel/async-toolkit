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
import java.util.Iterator;

/**
 * Abstract Class to represent a generic voltage sources
 *
 * @author Dan Daly
 * @version $Name:  $ $Date$
 **/
public abstract class VoltageSource extends AbstractDevice {

//
//Parameters used to calculate the timestep
//
    /** The time last timestep **/
    private double oldtime;
    /** The current time **/
    private double time;
    
//
//Device currents
//
    /** Drain node **/
    private double drainI;
    /** Source node **/
    private double sourceI;

    /** Builds a generic voltage source device 
     *
     * @param name HierName of the device
     * @param nplus positive terminal node
     * @param nminus negative terminal node
     ***/
    public VoltageSource(final HierName name,
                         final Node nplus,
                         final Node nminus) {
        super( new Node[] {nplus, nminus});
        this.name = name;
        this.oldtime = -1e-12; // Provides a timstep of 1e-12 at t=0
    }
                         
    /** returns a tau = to 4*timestep **/
    public double getDrivenTau() {
        return 4*(time-oldtime);  //amount of curvature
    }
    
    /**
     * Get the positive terminal node.
     * @return positive terminal node
     **/
    public Node getPositiveTerminal() {
        return nodes[0];
    }

    /**
     * Get negative terminal node.
     * @return negative terminal node
     **/
    public Node getNegativeTerminal() {
        return nodes[1];
    }
    
    /** returns the device current **/
    public double getCurrent(int type) {
        switch (type) {
          case AbstractDevice.I1:      return drainI;
          case AbstractDevice.I2:      return sourceI;
        }
        return -1;
    }
    
    public String getCode() { return "V";}
    
    private double setAnalogNode(Node node, double X,
            double chargeScale, double currentScale, 
            double derivChargeScale, double derivCurrentScale) {
        double I = ((derivChargeScale*getDrivenTau() 
                                    - derivCurrentScale)*X)/currentScale;
        //node.setResult(
        //       (chargeScale*getDrivenTau()-currentScale)
        //        *(node.getVoltage() - V));
        //node.setResult(chargeScale, getDrivenTau()*(node.getVoltage() - V),
        //               currentScale, I);
        node.forceResult((derivChargeScale*getDrivenTau() 
                                    - derivCurrentScale)*X);
        
        /*  Once a node becomes driven to the voltage V, 
         *  it cannot be influenced by its neighboring nodes.  So the 
         *  diagonal matrix element is set to mAq*tau - mAi
         *  */
        for (final Iterator i= node.getNeighborMap().keySet().iterator();
                i.hasNext();) {
            Object nodeId = i.next();
            double[] matrixValue = (double[]) node.getNeighborMap().get(nodeId);
            if (nodeId.equals(node.getHash())) {
                   matrixValue[0] = derivChargeScale*getDrivenTau() 
                                    - derivCurrentScale;
            } else matrixValue[0] = 0;
        }
        return I;
    }
 
    private double V=0,lastV=0;

    /**
     * Evaluates current, charge, and their derivatives for all ports
     * on a device and then informs its nodes of these values.
     * @param chargeScale scalar for charge calculation
     * @param currentScale scalar for current calculation
     * @param derivChargeScale scalar for charge derivative calculation
     * @param derivCurrentScale scalar for current derivative calculation
     * @param time Current time of the simulation
     **/
    public void evalVoltage(double chargeScale, double currentScale,
                            double derivChargeScale, double derivCurrentScale,
                            double time) {
        this.time = time;
        this.lastV = V;
        double timestep = time-oldtime;
        this.V = getDrivenVoltage(time,timestep);
        drainI = setAnalogNode(getPositiveTerminal(),V-lastV,
                chargeScale,currentScale,
                derivChargeScale, derivCurrentScale);
        sourceI = setAnalogNode(getNegativeTerminal(),lastV-V,
                chargeScale,currentScale,
                derivChargeScale, derivCurrentScale);
        oldtime = time;
    }

    /** The function to override to provide specific behavior **/
    public abstract double getDrivenVoltage(double time, double timestep);

    
}

