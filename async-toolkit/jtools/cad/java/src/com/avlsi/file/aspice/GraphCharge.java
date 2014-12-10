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

public class GraphCharge extends GraphPortData {

    private final int chargePort;

    public GraphCharge(String fileName, int voltagePort, int chargePort)
        throws IOException
    {
        super(fileName + "_Q" + chargePort, voltagePort);

        this.chargePort = chargePort;
    }

    public double getY(PortData[] data) {
        return data[chargePort].getCharge();
    }
}
