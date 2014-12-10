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

/**********************************************************************
 * Circuit graph node class.  Implements the full functionality of
 * AbstractNode, at the expense of increased memory consumption 
 * compared to LumpedNode or DriverNode.
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **********************************************************************/
final class Node extends AbstractNode {

    private final ArrayList mosList   = new ArrayList();
    private final ArrayList resList   = new ArrayList();
    private final ArrayList capList   = new ArrayList();
    private final ArrayList diodeList = new ArrayList();

    /** 
     * Constructor.  Takes a node name.  Constructs the AbstractNode
     * base class with two marks.
     **/
    Node(final HierName name) {
        super(name,2);
    }

    //
    // Abstract method implementations
    //

    /** Connects a resistor to the node. **/
    void connectRes(final ResDevice r) {
        resList.add(r);
    }

    /** Connects a capacitor to the node. **/
    void connectCap(final CapDevice c) {
        capList.add(c);
    }

    /** Connects a transistor to the node. **/
    void connectMos(final MosDevice m) { 
        mosList.add(m); 
    }

    /** Connects a diode to the node. **/
    void connectDiode(final DiodeDevice d) {
        diodeList.add(d);
    }

    /** Returns an iterator over all resistors connected to this node **/
    public Iterator getResDevices() { return resList.iterator(); }

    /** Returns an iterator over all capacitors connected to this node **/
    public Iterator getCapDevices() { return capList.iterator(); }

    /** Returns an list of MosDevices connected to this node. **/
    public Iterator getMosDevices() { return mosList.iterator(); }

    /** Returns an iterator over all diodes connected to this node **/
    public Iterator getDiodeDevices() { return diodeList.iterator(); }

    /** Clears all marks in attached device edges **/
    void clearDeviceMarks() {
        Iterator it = resList.iterator();
        while (it.hasNext()) {
            ResDevice rd = (ResDevice)it.next();
            rd.clearMark();
        }
    }

    /** Returns total wire capacitance lumped to this node **/
    public float wireCap() { 
        float wc = 0.0f;
        Iterator it = capList.iterator();
        while (it.hasNext()) {
            CapDevice cd = (CapDevice)it.next();
            wc += cd.getValue();
        }
        return wc;
    }

    /** Returns total gate capacitance lumped to this node **/
    public float gateCap() { 
        float gc = 0.0f;
        Iterator it = mosList.iterator();
        while (it.hasNext()) {
            MosDevice md = (MosDevice)it.next();
            if (md.gateNode() == this) gc += md.gateCap();
        }
        return gc;
    }

    /** 
     * Returns total "good" capacitance (lumped to a fixed node
     * relative to this one).  For now, just {@link #gateCap()}.
     **/
    public float goodCap() { return gateCap(); }

    /** 
     * Returns total "bad" capacitance (lumped to nodes that could 
     * transition during this node's transition).  For now, just
     * {@link #wireCap()}.
     **/
    public float badCap() { return wireCap(); }

    /** Returns number of (wire) Capacitors attached to this node. **/
    public int numCap() { return capList.size(); }

    /** Returns number of resistors attached to this node **/
    public int numRes() { return resList.size(); }

    /** 
     * Returns number of Mosfet attached to this node via source/drain 
     * For LumpedNode.
     **/
    public int numDrivers() { 
        int n = 0;
        Iterator it = mosList.iterator();
        while (it.hasNext()) {
            MosDevice md = (MosDevice)it.next();
            if (md.sourceNode() == this || md.drainNode() == this) ++n;
        }
        return n;
    }

    /** 
     * Returns number of gates attached to this node 
     **/
    public int numGates() {
        int n = 0;
        Iterator it = mosList.iterator();
        while (it.hasNext()) {
            MosDevice md = (MosDevice)it.next();
            if (md.gateNode() == this) ++n;
        }
        return n;
    }
}

