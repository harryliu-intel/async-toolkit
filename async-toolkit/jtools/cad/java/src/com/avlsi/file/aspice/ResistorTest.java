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
 * Class that tests resistors
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/

public final class ResistorTest extends AbstractDeviceTest {
    public ResistorTest(String source, String drain, double conductance) {
        super(new Resistor(HierName.makeHierName(source),
                           HierName.makeHierName(drain),
                           conductance));

    }

    public double[][] getVoltageRange() {
        // voltageRange[i] is range info for port i
        // voltageRange[i][0] is start voltage, and [i][1] is end voltage
        return new double[][] {
            new double[] { low, low  }, // Fix Source voltage
            new double[] { low, high }  // Scan Drain voltage
        };
    }

    public GraphPortData[] makeGraphers()
        throws IOException
    {
        // Compare current on source with voltage on drain
        final int currentPort = 0;
        final int voltagePort = 1;
        final String devName = device.getClass().getName();

        final GraphPortData[] gpds = new GraphPortData[2];

        try {
            gpds[0] = new GraphCurrent(devName, voltagePort, currentPort);
            gpds[1] = new GraphCurrentDerivative(devName, voltagePort,
                                                 currentPort, currentPort);
            return gpds;
        } catch (IOException e) {
            for (int i = 0; i < gpds.length; i++)
                if (gpds[i] != null)
                    gpds[i].close();

            throw e;
        }
    }
}
