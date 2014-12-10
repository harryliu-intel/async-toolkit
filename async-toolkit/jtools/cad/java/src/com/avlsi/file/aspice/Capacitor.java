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

package com.avlsi.file.aspice;

import com.avlsi.file.common.HierName;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.circuit.CapacitorInterface;

/**
 * Class to represent resistors in .aspice files.
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public final class Capacitor implements DeviceInterface,CapacitorInterface {
    /** Number of Ports */
    private static final int NUM_PORTS = 2;

    /** Capacitance of capacitor in */
    private double capacitance;

    /** Name of source node */
    private final HierName source;

    /** Name of drain node */
    private final HierName drain;

    /**
     * Class constructor.
     * @param source name of source node
     * @param drain name of drain node
     * @param capacitance of capacitor
     **/
    public Capacitor(final HierName source,
                     final HierName drain,
                     final double capacitance)
    {
        // ensure that source <= drain, lexicographically
        if (source.compareTo(drain) <= 0) {
            this.source = source;
            this.drain = drain;
        } else {
            this.source = drain;
            this.drain = source;
        }

        this.capacitance = capacitance;
    }

    /** Not implemented, returns null **/
    public HierName getName() { return null;}
    
    /**
     * Get number of ports
     * @return name of source node
     **/
    public int getNumPorts() {
        return NUM_PORTS;
    }

    /**
     * Get name of source node.
     * @return name of source node
     **/
    public HierName getSource() {
        return source;
    }

    /**
     * Get name of drain node.
     * @return name of drain node
     **/
    public HierName getDrain() {
        return drain;
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

    /**
     * return string suitable for inclusion in an aspice file.
     **/
    public String getAspiceString() {
        return "cap ("
            + getSource().getAspiceString() + ","
            + getDrain().getAspiceString() + ") ("
            + NumberFormatter.format(getCapacitance()) + ");";
    }

    /**
     *
     **/
    public String toString() {
        return getClass().getName() + " ("
            + getSource().toString() + ","
            + getDrain().toString() + ") ("
            + getCapacitance() + ");";
    }

    /**
     * Evaluates current, charge, and their derivatives for all ports
     * on a device for a given set of input voltages.
     * @param data array of port data objects to use for return values
     * @param voltage array of voltages connected to device
     * @throws IllegalArgumentException
     * @return array of current/charge/etc. data for each port,
     *         indexed by input voltage
     **/
    public PortData[] evalVoltage(PortData[] data, double[] voltage) {
        if (data.length != NUM_PORTS || voltage.length != NUM_PORTS)
            throw new IllegalArgumentException("Expected " + NUM_PORTS +
                                               " ports");

        double charge = (voltage[1] - voltage[0]) * capacitance;

        data[0].setVoltage(voltage[0]);
        data[0].setCurrent(0);
        data[0].setCurrentDerivative(0, 0);
        data[0].setCurrentDerivative(1, 0);
        data[0].setCharge(charge);
        data[0].setChargeDerivative(0,  capacitance);
        data[0].setChargeDerivative(1, -capacitance);

        data[1].setVoltage(voltage[1]);
        data[1].setCurrent(0);
        data[1].setCurrentDerivative(0, 0);
        data[1].setCurrentDerivative(1, 0);
        data[1].setCharge(-charge);
        data[1].setChargeDerivative(0, -capacitance);
        data[1].setChargeDerivative(1,  capacitance);

        return data;
    }
}
