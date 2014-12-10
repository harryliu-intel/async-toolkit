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

public class GraphChargeDerivative extends GraphPortData {

    private final int chargePort;
    private final int voltageDerivativePort;

    public GraphChargeDerivative(String fileName, int voltagePort,
                                 int chargePort)
        throws IOException
    {
        this(fileName, voltagePort, chargePort, voltagePort);
    }

    public GraphChargeDerivative(String fileName, int voltagePort,
                                 int chargePort, int voltageDerivativePort)
        throws IOException
    {
        super(fileName + "_dQ" + chargePort + "_dV" + voltageDerivativePort,
              voltagePort);

        this.chargePort = chargePort;
        this.voltageDerivativePort = voltageDerivativePort;
    }

    public double getY(PortData[] data) {
        //GGG Wow is that a great example of too much description making
        // something hard to understand...
        return data[chargePort].getChargeDerivative(voltageDerivativePort);
    }
}
