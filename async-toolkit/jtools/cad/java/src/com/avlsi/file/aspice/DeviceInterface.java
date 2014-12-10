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
 * Interface for devices
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
interface DeviceInterface {
    /** Gets the number of ports on the device. **/
    int getNumPorts();

    /**
     * Evaluates current, charge, and their derivatives for all ports
     * on a device for a given set of input voltages.
     * @param data array of port data objects to use for return values
     * @param voltage array of voltages connected to device
     * @throws IllegalArgumentException
     * @throws NoModelException
     * @return array of current/charge/etc. data for each port,
     *         indexed by input voltage
     **/
    PortData[] evalVoltage(PortData[] data, final double[] voltage)
        throws NoModelException;
}
