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

import com.avlsi.file.common.HierName;

/**
 * Class that tests diodes
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/

public class DiodeTest extends AbstractDeviceTest {
    public DiodeTest(int type,
                          String source, String drain,
                          double width, double length,
                          double area, double perimeter)
    {
        super(new Diode(type,
                        HierName.makeHierName(source),
                        HierName.makeHierName(drain),
                        width, length, area, perimeter));
    }

    public double[][] getVoltageRange() {
        // voltageRange[i] is range info for port i
        // voltageRange[i][0] is start voltage, and [i][1] is end voltage
        return new double[][] {
            new double[] { 0, 0 }, // Fix Source voltage
            new double[] { 0, 2 }, // Scan Drain voltage
        };
    }

    public GraphPortData[] makeGraphers()
        throws IOException
    {
        final int source = 0;
        final int drain  = 1; 

        final int voltagePort = drain;
        final String devName = device.getClass().getName();

        final GraphPortData[] gpds = new GraphPortData[4];

        try {
            gpds[0] = new GraphCurrent(devName, voltagePort, source);
            gpds[1] = new GraphCurrentDerivative(devName, voltagePort, source,
                                                 source);
            gpds[2] = new GraphCurrent(devName, voltagePort, drain);
            gpds[3] = new GraphCurrentDerivative(devName, voltagePort, drain,
                                                 source);
            return gpds;
        } catch (IOException e) {
            for (int i = 0; i < gpds.length; i++)
                if (gpds[i] != null)
                    gpds[i].close();
        
            throw e;
        }
    }
}
