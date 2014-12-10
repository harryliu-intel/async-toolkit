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
 * Class to represent resistors in .aspice files.
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public final class Capacitor extends AbstractDevice { 
    /** Capacitance of capacitor in */
    private double capacitance;
    
    /** Device currents **/
    
    /** Drain node **/
    private double drainI;
    /** Source node **/
    private double sourceI;

    /**
     * Class constructor.
     * @param name HierName of device
     * @param source node
     * @param drain node
     * @param capacitance of capacitor
     **/
    public Capacitor(final HierName name,
                     final Node source,
                     final Node drain,
                     final double capacitance)
    {
        super(new Node[] { source, drain });
        this.capacitance = capacitance;
        this.name = name;
    }

    public Capacitor(final Node source,
                     final Node drain,
                     final double capacitance) {
        this(null, source, drain, capacitance);
    }
    /**
     * Get source node.
     * @return source node
     **/
    public Node getSource() {
        return nodes[0];
    }

    /**
     * Get drain node.
     * @return drain node
     **/
    public Node getDrain() {
        return nodes[1];
    }

    /**
     * Get capacitance of capacitor.
     * @return capacitance of capacitor
     **/
    public double getCapacitance() {
        return capacitance;
    }

    /**
     * Set capacitance of capacitor.
     **/
    public void setCapacitance(double capacitance) {
        this.capacitance = capacitance;
    }
    
    /** returns the device current **/
    public double getCurrent(int type) {
        switch (type) {
          case AbstractDevice.I1:      return drainI;
          case AbstractDevice.I2:      return sourceI;
        }
        return -1;
    }


    public String getCode() { return "C";}
    /**
     * Evaluates current, charge, and their derivatives for all ports
     * on a device and then informs its nodes of these values.
     * @param chargeScale scalar for charge calculation
     * @param currentScale scalar for current calculation
     * @param derivChargeScale scalar for charge derivative calculation
     * @param derivCurrentScale scalar for current derivative calculation
     **/
    public void evalVoltage(double chargeScale, double currentScale,
                            double derivChargeScale, double derivCurrentScale,
                            double time)
    {
        Node source = getSource();
        Node drain = getDrain();

        double charge = (drain.getVoltage() - source.getVoltage())
                        * capacitance;

        drainI = capacitance * (drain.getVoltageChange() - source.getVoltage());
        sourceI = -drainI;

        source.setResult(chargeScale, charge, currentScale, 0);
        drain.setResult(chargeScale, -charge, currentScale, 0);

        source.setMatrix(source,
                         derivChargeScale, capacitance, derivCurrentScale, 0);
        source.setMatrix(drain,
                         derivChargeScale, -capacitance, derivCurrentScale, 0);
        drain.setMatrix(source,
                        derivChargeScale, -capacitance, derivCurrentScale, 0);
        drain.setMatrix(drain,
                        derivChargeScale, capacitance, derivCurrentScale, 0);
    }
    
}
