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
 * Base class for devices
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public abstract class AbstractDevice implements Cloneable{
    
    public static final int I1 = 1;
    public static final int I2 = 2;
    public static final int I3 = 3;
    public static final int I4 = 4;

    public static final String[] names = { "ERROR", "I1","I2","I3","I4"};
    /**
     * List of nodes connected to this device.
     * Classes extending this class understand the ordering.
     **/
    protected Node[] nodes;

    /** Name of the Device **/
    protected HierName name;
    
    /**
     * Construct device
     **/
    public AbstractDevice(Node[] nodes) {
        this.nodes = nodes;

        for (int i = 0; i < nodes.length; i++)
            nodes[i].addNeighborNodes(nodes);
    }

    /**
     * Get nodes connected to this device
     * @return list of nodes this device connects
     **/
    public Node[] getNodes() {
        return nodes;
    }

    public HierName getName() {
        return name;
    }

    public void setName(HierName n) { name = n; }

    public double getCurrent(int type) { return -1; }

    /**
     * Evaluates current, charge, and their derivatives for all ports
     * on a device and then informs its nodes of these values.
     * @param chargeScale scalar for charge calculation
     * @param currentScale scalar for current calculation
     * @param derivChargeScale scalar for charge derivative calculation
     * @param derivCurrentScale scalar for current derivative calculation
     * @param time Current time of the simulation (for sources)
     **/
    public abstract void evalVoltage(
        double chargeScale, double currentScale,
        double derivChargeScale, double derivCurrentScale,double time);

    /** Should return the SPICE prefic code (like M, D, C, etc) **/
    public abstract String getCode();
    public AbstractDevice copy(HierName name, Node[] nodes)
            throws AbstractDeviceException {
        try {
            AbstractDevice ad = (AbstractDevice) this.clone();
            ad.name = name;
            ad.nodes = nodes;
            for (int i = 0; i < nodes.length; i++)
                ad.nodes[i].addNeighborNodes(ad.nodes);
            return ad;
        } catch(CloneNotSupportedException e) {
            throw new AbstractDeviceException("Could not clone: "+
                                            e.getMessage(), e);
        }
    }

    /*****************************************************************
     * AbstractCircuit exception class
     *****************************************************************/
    public static class AbstractDeviceException extends java.lang.Exception { 
        public AbstractDeviceException() { super(); }
        public AbstractDeviceException(final String desc) { super(desc); }
        public AbstractDeviceException(final String desc, Throwable cause) {
            super(desc, cause);
        }
    }
 
}
