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
 * Interface for graphing current
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/

public class GraphCurrentDerivative extends GraphPortData {

    private final int currentPort;
    private final int voltageDerivativePort;

    public GraphCurrentDerivative(String fileName, int voltagePort,
                                  int currentPort)
        throws IOException
    {
        this(fileName, voltagePort, currentPort, voltagePort);
    }

    public GraphCurrentDerivative(String fileName, int voltagePort,
                                  int currentPort, int voltageDerivativePort)
        throws IOException
    {
        super(fileName + "_dI" + currentPort + "_dV" + voltageDerivativePort,
              voltagePort);

        this.currentPort = currentPort;
        this.voltageDerivativePort = voltageDerivativePort;
    }

    public double getY(PortData[] data) {
        //GGG Wow is that a great example of too much description making
        // something hard to understand...
        return data[currentPort].getCurrentDerivative(voltageDerivativePort);
    }
}
