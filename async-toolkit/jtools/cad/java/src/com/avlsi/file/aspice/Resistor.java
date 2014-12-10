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
import com.avlsi.circuit.ResistorInterface;

/**
 * Class to represent resistors in .aspice files.
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public final class Resistor implements DeviceInterface,ResistorInterface {
    /** Number of Ports */
    private static final int NUM_PORTS = 2;

    /** Conductance of resistor in 1/ohms */
    private double conductance;

    /** Name of source node */
    private final HierName source;

    /** Name of drain node */
    private final HierName drain;

    /**
     * Class constructor.
     * @param source name of source node
     * @param drain name of drain node
     * @param conductance of resistor
     **/
    public Resistor(final HierName source,
                    final HierName drain,
                    final double conductance)
    {
        // ensure that source <= drain, lexicographically
        if (source.compareTo(drain) <= 0) {
            this.source = source;
            this.drain = drain;
        } else {
            this.source = drain;
            this.drain = source;
        }

        this.conductance = conductance;
    }
    /** Not implemented, returns null **/
    public HierName getName() { return null;}

    /**
     * Get number of ports
     * @return number of ports
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
     * Get conductance of resistor.
     * @return conductance of resistor
     **/
    public double getConductance() {
        return conductance;
    }

    /**
     * Set conductance of resistor.
     **/
    public void setConductance(double conductance) {
        this.conductance = conductance;
    }

    /**
     * return string suitable for inclusion in an aspice file.
     **/
    public String getAspiceString() {
        return "res ("
            + getSource().getAspiceString() + ","
            + getDrain().getAspiceString() + ") ("
            + NumberFormatter.format(1/getConductance()) + ");";
    }

    public String toString() {
        return getAspiceString();
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

        double current = (voltage[1] - voltage[0]) * conductance;

        data[0].setVoltage(voltage[0]);
        data[0].setCurrent(current);
        data[0].setCurrentDerivative(0,  conductance);
        data[0].setCurrentDerivative(1, -conductance);
        data[0].setCharge(0);
        data[0].setChargeDerivative(0, 0);
        data[0].setChargeDerivative(1, 0);
        
        data[1].setVoltage(voltage[1]);
        data[1].setCurrent(-current);
        data[1].setCurrentDerivative(0, -conductance);
        data[1].setCurrentDerivative(1,  conductance);
        data[1].setCharge(0);
        data[1].setChargeDerivative(0, 0);
        data[1].setChargeDerivative(1, 0);

        return data;
    }

}
