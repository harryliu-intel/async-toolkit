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
 * Class for Resistor to be used with building an AbstractCircuit or a
 * subclass of it
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class Resistor implements ResistorInterface{

    /** name of device (optional) **/
    protected final HierName name;
    
    /** Names of connecting nodes **/
    protected final HierName source, drain;

    /** Conductance or inverse resitance of resistor in mho(inverse ohm) **/
    protected final double conductance;
    
    /**
     * Constructor.
     **/
    public Resistor(HierName name,
                    HierName source,
                    HierName drain,
                    double conductance) {
        this.name = name;
        this.source = source;
        this.drain = drain;
        this.conductance = conductance;
    }
    
    public HierName getName() { return name; }

    public double getConductance() { return conductance; }
    
    public HierName getSource() { return source;}
    
    public HierName getDrain() { return drain;}

}

