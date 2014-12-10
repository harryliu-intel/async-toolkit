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

package com.avlsi.file.spice;

/**
 * Class for passing parsed commands from the spice parser to a general 'simulator'
 *
 * @author Dan Daly
 * @version $Date$
 **/

public interface SimulatorInterface {
    void printStatement(String type, String[] args) 
            throws SpiceFileFormatException;
//    void voltageSourceStatement(String name, String plusnode, 
//            String minusnode, String args[]) throws SpiceFileFormatException;
    //void currentSourceStatement(String name, String plusnode,
            //String minusnode, String args[]);
    void setStatement(String key, String value)
            throws SpiceFileFormatException;
}

