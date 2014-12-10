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
 * Class for Capacitor to be used with building an AbstractCircuit or a
 * subclass of it
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class Capacitor implements CapacitorInterface{

    /** name of device (optional) **/
    protected final HierName name;
    
    /** Names of connecting nodes **/
    protected final HierName source, drain;

    /** Capacitance in farads **/
    protected final double capacitance;
    
    /**
     * Constructor.
     **/
    public Capacitor(HierName name,
                    HierName source,
                    HierName drain,
                    double capacitance) {
        this.name = name;
        this.source = source;
        this.drain = drain;
        this.capacitance = capacitance;
    }
    
    public HierName getName() { return name; }

    public double getCapacitance() { return capacitance; }
    
    public HierName getSource() { return source;}
    
    public HierName getDrain() { return drain;}

}

