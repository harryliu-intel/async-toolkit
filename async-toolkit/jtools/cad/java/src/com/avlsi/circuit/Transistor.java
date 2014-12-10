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

package com.avlsi.circuit;
import com.avlsi.file.common.HierName;

/**
 * Class for Transistors to be used for AbstractCircuit and its
 * subclasses
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class Transistor implements TransistorInterface{

    /** name of device (optional) **/
    protected final HierName name;
    
    /** Names of connecting nodes **/
    protected final HierName source, drain, gate, bulk;
    
    /** type of transistor **/
    protected final int type;

    /** Width of transistors in meters **/
    protected final double width;

    /** Length of transistors in meters **/
    protected final double length;
    
    /**
     * Constructor.
     **/
    public Transistor(HierName name,
                      int type, 
                      HierName source,
                      HierName drain,
                      HierName gate,
                      HierName bulk,
                      double width,
                      double length) {
        this.name = name;
        this.source = source;
        this.drain = drain;
        this.gate = gate;
        this.bulk = bulk;
        this.width = width;
        this.length = length;
        this.type = type;
    }

    public int getType() { return type; }

    public double getWidth() { return width; }

    public double getLength() { return length; }

    public HierName getName() { return name; }
    
    public HierName getSource() { return source;}
    public HierName getDrain() { return drain;}
    public HierName getGate() { return gate; }
    public HierName getBulk() { return bulk;}
}

