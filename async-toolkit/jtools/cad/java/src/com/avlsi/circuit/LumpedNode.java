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
 * Circuit graph lumped node class.  Lumps all gate and wire 
 * capacitance attached to the node.  Keeps a count of the number of
 * connected source/drain terminals.  Maintains a list of resistive
 * edges.  This node class is intended to contain the minimal amount
 * of data to allow resistive subnet evaluation and maximum resistive 
 * path analysis.
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **********************************************************************/
final class LumpedNode extends AbstractNode {

    private final ArrayList resList = new ArrayList();
    private float wireCap;
    private float goodCap;
    private float gateCap;
    private int numDrivers;

    /** 
     * Constructor.  Takes a node name.  Constructs the AbstractNode
     * base class with two marks.
     **/
    LumpedNode(final HierName name) {
        super(name,2);
    }

    //
    // Abstract method implementations
    //

    /** Connect a resistor to the node. **/
    void connectRes(final ResDevice r) {
        resList.add(r);
    }

    /** 
     * Connect a capacitor to the node.  Accumulates the
     * wire cap and "good" cap.
     ***/
    void connectCap(final CapDevice c) {
        if (!c.isDegenerate()) {
            wireCap += c.getValue();
            // Note: What effect does degenerate capacitance actually
            // have?  Is it maybe perfect "good" cap (always switching
            // in same direction)?  For now, it isn't included in the
            // good cap sum.
            if (c.isGood()) goodCap += c.getValue();
        }
    }

    /** 
     * Connect a transistor to the node.  If this node is attached
     * to the gate, accumulates the gate cap.  Otherwise, increments
     * the driver count as long as the transistor is non-weak.
     ***/
    void connectMos(final MosDevice m) {
        if (m.gateNode() == this) gateCap += m.gateCap();
        else if (!m.isWeak()) numDrivers++;
    }

    /** Connects a diode to the node.  Unimplemented for LumpedNode. **/
    void connectDiode(final DiodeDevice d) {}

    /** Return iterator over all resistors connected to this node **/
    public Iterator getResDevices() {
        return resList.iterator();
    }

    /** 
     * Returns an iterator over all capacitors connected to this node.
     * Null for LumpedNode.
     **/
    public Iterator getCapDevices() { return null; }

    /** 
     * Returns an iterator over all diodes connected to this node.
     * Null for LumpedNode.
     **/
    public Iterator getDiodeDevices() { return null; }

    /** Return list of MosDevices devices.  Null for LumpedNode. **/
    public Iterator getMosDevices() { return null; }

    /** Clear all marks in attached device edges **/
    void clearDeviceMarks() {
        Iterator it = resList.iterator();
        while (it.hasNext()) {
            ResDevice rd = (ResDevice)it.next();
            rd.clearMark();
        }
    }

    /** Returns total wire capacitance lumped to this node **/
    public float wireCap() { return wireCap; }

    /** Returns total gate capacitance lumped to this node **/
    public float gateCap() { return gateCap; }

    /** 
     * Returns total "good" capacitance (lumped to a fixed node
     * relative to this one).
     **/
    public float goodCap() { return goodCap; }

    /** 
     * Returns total "bad" capacitance (lumped to nodes that could 
     * transition during this node's transition).
     **/
    public float badCap() { return wireCap-goodCap; }

    /** 
     * Returns number of (wire) Capacitors attached to this node 
     * For LumpedNode, this is always 1 if wireCap &gt; 0.0; 0
     * otherwise.
     **/
    public int numCap() { return (wireCap > 0.0) ? 1 : 0; }

    /** Returns number of resistors attached to this node **/
    public int numRes() { return resList.size(); }

    /** 
     * Returns number of Mosfet attached to this node via source/drain 
     * For LumpedNode, the number only includes non-weak drivers, as
     * defined by {@link MosDevice#isWeak()}.
     **/
    public int numDrivers() { return numDrivers; }

    /** 
     * Returns number of gates attached to this node 
     * For LumpedNode, this is 1 if gateCap &gt; 0.0; 0 otherwise.
     **/
    public int numGates() { return (gateCap > 0.0) ? 1 : 0; }
}

