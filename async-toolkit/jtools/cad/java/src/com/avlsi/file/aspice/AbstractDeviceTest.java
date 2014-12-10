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

import java.io.IOException;

/**
 * Class that tests devices
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/

abstract class AbstractDeviceTest {
    // Device
    protected final DeviceInterface device;

    // Voltage value ranges for test graphs
    protected final double low  = 0.0;
    protected final double high = 2.0;

    // Number of iterations (data points to gather)
    protected long numIter = 100;

    // Minus one because we test both endpoints
    protected final double step = (high-low) / (numIter-1);

    protected AbstractDeviceTest(DeviceInterface device) {
        this.device = device;
    }

    // Technically stated there should be API function calls that
    // set step/low/high and numIter is automatically recalculated
    // from that, but it's much easier to make the variables constant
    // instead.  How often do you really want to change that information
    // during run-time?

    /**
     * Test the device
     * @param voltageRange Boundary point voltages to test
     * @param numIter Number of iterations to test
     **/
    public void test()
    {
        try {
             final int numPorts = device.getNumPorts();
             double[][] voltageRange = getVoltageRange();
             GraphPortData[] graphers = makeGraphers();

             // You need at least 2 data points for a graph
             if (numIter < 2)
                 numIter = 2;
            
             // Compute starting voltages and voltage step between iterations
             // Allocate Port Data object
             double  [] voltage = new double  [numPorts];
             double  [] step    = new double  [numPorts];
             PortData[] data    = new PortData[numPorts];

             for (int i = 0; i < numPorts; i++) {
                 voltage[i] = voltageRange[i][0];
                 step[i] = (voltageRange[i][1] - voltageRange[i][0]) /
                           (numIter - 1);
                 data[i] = new PortData(numPorts);
             }

             long startTime = System.currentTimeMillis();

             for (long i = 0; i < numIter; i++) {
                 device.evalVoltage(data, voltage);

                 for (int j = 0; j < graphers.length; j++)
                     graphers[j].writeCoordinate(data);

                 for (int j = 0; j < numPorts; j++)
                     voltage[j] += step[j];
             }

             long endTime = System.currentTimeMillis();
             double diff = (endTime - startTime) / 1000.0;
             System.out.println(numIter + " " + device.getClass().getName() +
                                " tests took " + diff + " seconds.");
             System.out.println("  That's " + (numIter / diff) +
                                " evaluations per second");

             for (int i = 0; i < graphers.length; i++)
                graphers[i].close();
        } catch (IOException e) {
            System.out.println(e);
        } catch (NoModelException e) {
            System.out.println("Couldn't find a model for tested device.");
        }
    }

    protected abstract double[][] getVoltageRange();
    protected abstract GraphPortData[] makeGraphers()
        throws IOException;
}
