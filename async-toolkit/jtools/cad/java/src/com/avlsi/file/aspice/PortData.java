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

/**
 * Class for port data evaluated by device.
 *
 * This class wraps all of the data values associated with
 * a single port (node) out of a device.  It contains
 * current, voltage, and charge values as doubles.  It also
 * has an array of partial derivatives for both current and
 * charge with respect to voltages on each device node.
 * So dI/dV[0] is the derivative of current on this node
 * with respect to the 0th node of the device (usually source).
 *
 **/
class PortData {
    private double V;
    private double I;
    private double Q;
    private double dI[];
    private double dQ[];

    public PortData(int numPorts) {
        this.V = this.I = this.Q = 0;
        this.dI = new double[numPorts];
        this.dQ = new double[numPorts];

        for (int i = 0; i < numPorts; i++)
            dI[i] = dQ[i] = 0;
    }

    public PortData(double V, double I, double Q,
                    double dI[], double dQ[]) {
        this.V = V;
        this.I = I;
        this.Q = Q;
        this.dI = dI;
        this.dQ = dQ;
    }

    /**
     * Get voltage
     * @return voltage
     **/
    public double getVoltage() {
        return V;
    }

    /**
     * Set voltage
     * @param voltage
     **/
    public void setVoltage(double V) {
        this.V = V;
    }

    /**
     * Get current
     * @return current
     **/
    public double getCurrent() {
        return I;
    }

    /**
     * Set current
     * @param current
     **/
    public void setCurrent(double I) {
        this.I = I;
    }

    /**
     * Get charge
     * @return charge
     **/
    public double getCharge() {
        return Q;
    }

    /**
     * Set charge
     * @param charge
     **/
    public void setCharge(double Q) {
        this.Q = Q;
    }

    /**
     * Get derivative vector of current
     * @return current derivative array
     **/
    public double[] getCurrentDerivative() {
        return dI;
    }

    /**
     * Get derivative of current
     * @return current derivative
     **/
    public double getCurrentDerivative(int i) {
        return dI[i];
    }

    /**
     * Set derivative of current
     * @param voltage with respect to which voltage
     * @param current derivative value
     **/
    public void setCurrentDerivative(int voltage, double current) {
        dI[voltage] = current;
    }

    /**
     * Get derivative vector of charge
     * @return charge derivative array
     **/
    public double[] getChargeDerivative() {
        return dQ;
    }

    /**
     * Get derivative of charge
     * @return charge derivative
     **/
    public double getChargeDerivative(int i) {
        return dQ[i];
    }

    /**
     * Set derivative of charge
     * @param voltage with respect to which voltage
     * @param charge derivative value
     **/
    public void setChargeDerivative(int voltage, double charge) {
        dQ[voltage] = charge;
    }

}
