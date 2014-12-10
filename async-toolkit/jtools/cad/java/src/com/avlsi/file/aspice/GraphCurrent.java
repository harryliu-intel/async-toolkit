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

public class GraphCurrent extends GraphPortData {

    private final int currentPort;

    public GraphCurrent(String fileName, int voltagePort, int currentPort)
        throws IOException
    {
        super(fileName + "_I" + currentPort, voltagePort);

        this.currentPort = currentPort;
    }

    public double getY(PortData[] data) {
        return data[currentPort].getCurrent();
    }
}
