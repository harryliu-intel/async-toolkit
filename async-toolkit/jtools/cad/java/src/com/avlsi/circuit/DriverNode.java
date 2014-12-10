/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 * $Id$
 */

package com.avlsi.circuit;

import java.util.ArrayList;
import java.util.Iterator;

import com.avlsi.file.common.HierName;

/*********************************************************************
 * Circuit graph driver node class.  Includes a list of driver
 * transistors attached to this node and associated diodes.
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 *********************************************************************/
final class DriverNode extends AbstractNode {

    private final ArrayList driverList = new ArrayList();
    private final ArrayList diodeList  = new ArrayList();

    /** 
     * Constructor.  Takes a node name.  Constructs the AbstractNode
     * base class with two marks.
     **/
    DriverNode(final HierName name) {
        super(name,1);
    }

    //
    // Abstract method implementations
    //

    /** 
     * Connect a resistor to the node. 
     * Unimplemented in DegenerateNode. 
     **/
    void connectRes(final ResDevice r) {}

    /** 
     * Connect a capacitor to the node.  
     * Unimplmented in DegenerateNode.
     ***/
    void connectCap(final CapDevice c) {}

    /** 
     * Connect a transistor to the node.  For DriverNode, only retains
     * the MosDevice if it's attached to the node by its source or
     * drain terminal.
     ***/
    void connectMos(final MosDevice m) {
        if (m.sourceNode() == this || m.drainNode() == this) 
            driverList.add(m);
    }

    /** Connects a diode to the node. **/
    void connectDiode(final DiodeDevice d) {
        diodeList.add(d);
    }

    /** 
     * Returns iterator over all resistors connected to this node.
     * Null for DriverNode.
     **/
    public Iterator getResDevices() { return null; }

    /** 
     * Returns iterator over all MosDevices connected to this node.  
     * For DriverNode, includes only those MosDevices that have their
     * source/drain terminals attached to the node.
     **/
    public Iterator getMosDevices() { return driverList.iterator(); }

    /** Returns an iterator over all CapDevices. Null for DriverNode. **/
    public Iterator getCapDevices() { return null; }

    /** Returns an iterator over all DiodeDevices. **/
    public Iterator getDiodeDevices() { return diodeList.iterator(); }

    /** Returns an iterator over all 

    /** Clear all marks in attached device edges **/
    void clearDeviceMarks() {}

    /** Returns total wire capacitance lumped to this node **/
    public float wireCap() { return 0.0f; }

    /** Returns total gate capacitance lumped to this node **/
    public float gateCap() { return 0.0f; }

    /** 
     * Returns total "good" capacitance (lumped to a fixed node
     * relative to this one).
     **/
    public float goodCap() { return 0.0f; }

    /** 
     * Returns total "bad" capacitance (lumped to nodes that could 
     * transition during this node's transition).
     **/
    public float badCap() { return 0.0f; }

    /** 
     * Returns number of (wire) Capacitors attached to this node .
     * For DriverNode, this is always 0.
     **/
    public int numCap() { return 0; }

    /** Returns number of resistors attached to this node **/
    public int numRes() { return 0; }

    /** 
     * Returns number of Mosfet attached to this node via source/drain.
     **/
    public int numDrivers() { return driverList.size(); }

    /** 
     * Returns number of gates attached to this node 
     * For DriverNode, this is always 0.
     **/
    public int numGates() { return 0; }
}

