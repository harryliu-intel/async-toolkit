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

import java.io.Writer;
import java.io.PrintWriter;
import java.io.BufferedWriter;
import java.io.FileWriter;

import java.io.IOException;

/**
 * Interface for graphing port data
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/

abstract class GraphPortData {

    private PrintWriter file;

    private final int voltagePort;

    protected GraphPortData(Writer w, int voltagePort) {
        this.file = new PrintWriter(w);
        this.voltagePort = voltagePort;
    }

    protected GraphPortData(String fileName, int voltagePort)
        throws IOException
    {
        this(new BufferedWriter(new FileWriter(fileName+".gplot")),
             voltagePort);
    }

    public void close()
        throws IOException
    {
        file.close();
    }

    public void writeCoordinate(PortData[] data) {
        writeCoordinate(getX(data), getY(data));
    }

    public void writeCoordinate(double x, double y) {
        file.println(x + " " + y);
    }

    public double getX(PortData[] data) {
        return data[voltagePort].getVoltage();
    }

    public abstract double getY(PortData[] data);
}
