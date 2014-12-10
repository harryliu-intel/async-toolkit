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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.aspice.Resistor;
import com.avlsi.file.aspice.Transistor;
import com.avlsi.file.aspice.Diode;
import com.avlsi.file.common.Capacitor;

/**********************************************************************
 * Lumped RC circuit graph data structures.  The top-level
 * LumpedRCGraph class represents an entire (flat) cell of
 * transistors, resistors, and capacitors.  The underlying LumpedNode
 * class lumps wire cap and gate cap, and lumps all source/drain
 * regions into an integer corresponding to the number of non-weak 
 * drivers connected to the node.
 * <p>
 * The LumpedRCGraph can be built either from an existing 
 * {@link com.avlsi.file.aspice.AspiceCell}, or using the 
 * <tt>addDevice()</tt> methods from the
 * {@link AbstractCircuit} parent class.
 * <p>
 * Once the LumpedRCGraph is built, the 
 * {@link #identifyNets(com.avlsi.circuit.LumpedRCGraph.CallbackInterface)} 
 * method can be used to partition the graph into resistive subnetworks
 * ({@link ResistiveSubnet}).  The ResistiveSubnet class defines
 * various useful mining algorithms, such as <tt>identifyVDN()</tt> to
 * identify the net's virtual driver node, and
 * <tt>findMaxGateRC()</tt> to determine the maximum resistive path on
 * that net and its associated wire and gate capacitance.
 * <p>
 * Assumptions made in the code:
 * <ul>
 * <li> The drain/gate/source terminals of each Mosfet are all unique
 *      nodes (although could be part of the same net).  This is 
 *      imposed by the {@link MosDevice} marking scheme.
 * <li> No cycles in the circuit graph.  Resistance calculations will
 *      be inaccurate if cycles exist.  Luckily, from the standpoint of
 *      conservative layout verification, cycles will cause the resistance 
 *      to be overestimated.
 * <li> The virtual driver node identification algorithm assumes that
 *      All paths from driver nodes meet at a single node, i.e.
 *      <pre>
 *      Ex 1:                 Ex 2:
 *             D1  D2                D1  D2
 *              \  /   D3             \  /  D3
 *               N1   /                N1   /
 *                \  /                /  \ /
 *                VDN                G1   N2
 *                 |                      |
 *               G1..GN                 G2..GN
 *      </pre>
 *      <tt>D1..D3</tt> are nodes attached to transistor source/drain
 *      regions, <tt>N1..N2</tt> are nodes attached only to parasitic
 *      capacitors, and <tt>G1..GN</tt> are nodes attached to
 *      transistor gates.  Example 1 is what the algorithm expects.
 *      In Example 2, the VDN node might be identified as either
 *      <tt>N1</tt> or <tt>N2</tt>, but in either case the resistive
 *      segment between <tt>N1</tt> and <tt>N2</tt> will be
 *      overcounted.
 * <li> The circuit is flat, i.e. contains no subcells.  Any subcells
 *      will not be considered to be part of the circuit.
 * </ul>
 * Features that need to be added:
 * <ul>
 * <li> Multiple VDN nodes need to be supported.  <tt>identifyVDN()</tt>
 *      needs to identify cases where multiple VDN nodes are necessary
 *      (namely when the aggregate resistance from VDN to drivers is 
 *      above some threshold) and leave the VDN nodes unmerged.
 *      <tt>findMaxGateRC()</tt> needs to consider paths from all VDN
 *      nodes.
 * </ul>
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **********************************************************************/
public class LumpedRCGraph extends AbstractCircuit {

    static boolean VDN_DEBUG;
    static boolean GATERC_DEBUG;

    /*****************************************************************
     * Update callback interface for identifyNets(), used for updating
     * UI progress meter.
     *****************************************************************/
    public interface CallbackInterface {
        void identifyNetsUpdate(int numNets, final Set uncoveredNodes);
    }

    /***********************************************
     * Enables VDN debugging output.
     ***********************************************/
    public static void enableVdnDebug() {
        VDN_DEBUG = true;
        ResistiveSubnet.VDN_DEBUG = true;
        AbstractNode.VDN_DEBUG = true;
    }

    /***********************************************
     * Enables GATE_RC debugging output.
     ***********************************************/
    public static void enableGateRCDebug() {
        GATERC_DEBUG = true;
        ResistiveSubnet.GATERC_DEBUG = true;
        AbstractNode.GATERC_DEBUG = true;
    }

    /***********************************************
     * The number of resistors in the circuit.
     ***********************************************/
    private int numResistors;

    /***********************************************
     * The number of capacitors in the circuit.
     ***********************************************/
    private int numCapacitors;

    /***********************************************
     * The number of transistors in the circuit.
     ***********************************************/
    private int numTransistors;

    /***********************************************
     * HierName-to-Node map for the entire circuit.
     ***********************************************/
    private HashMap nodeMap;

    /***********************************************
     * Set of all resistive subnets
     ***********************************************/
    private HashSet resSubnets;

    /***********************************************
     * Uninitialized LumpedRCGraph constructor.
     ***********************************************/
    public LumpedRCGraph(String cellType) {
        super(cellType);
        nodeMap = new HashMap();
        resSubnets = new HashSet();
    }

    /*************************************************************
     * LumpedRCGraph constructor.  Builds a lumped RC circuit
     * graph from the given AspiceFile, ignoring any of its 
     * subcells.
     *************************************************************/
    public LumpedRCGraph(AspiceFile aspiceCell) throws Exception
    {
        super(aspiceCell.getName());

        //
        // Allocate nodeMap and resSubnet structures,
        // estimating initial capacity based on the number
        // of resistors and transistors in the Aspice cell.
        //
        nodeMap = new HashMap((int)(aspiceCell.getResistorCount()/0.75),0.75f);
        resSubnets = new HashSet((int)(aspiceCell.getTransistorCount()/3/0.75),
                                 0.75f);

        //
        // Add all resistors in circuit
        //
        Iterator it = aspiceCell.getResistors();
        Resistor r;
        while (it.hasNext()) addResistor((Resistor)it.next());

        //
        // Add all capacitors
        //
        it = aspiceCell.getCapacitors();
        Capacitor c;
        while (it.hasNext()) addCapacitor((Capacitor)it.next());

        //
        // Add all transistors
        //
        it = aspiceCell.getTransistors();
        Transistor t;
        while (it.hasNext()) addTransistor((Transistor)it.next());
    }

    /*****************************************************************
     * Add a resistor to the circuit graph
     *****************************************************************/
    public void addResistor(ResistorInterface r)
    {
        AbstractNode n0 = (AbstractNode)nodeMap.get(r.getSource());
        if (n0 == null) {
            n0 = new LumpedNode(r.getSource());
            nodeMap.put(r.getSource(),n0);
        }

        AbstractNode n1 = (AbstractNode)nodeMap.get(r.getDrain());
        if (n1 == null) {
            n1 = new LumpedNode(r.getDrain());
            nodeMap.put(r.getDrain(),n1);
        }

        ResDevice rd = new ResDevice(n0,n1,(float)(1/r.getConductance()));
        n0.connectRes(rd);
        n1.connectRes(rd);

        numResistors++;
    }

    /*****************************************************************
     * Add a capacitor to the circuit graph
     *****************************************************************/
    public void addCapacitor(CapacitorInterface c)
    {
        AbstractNode n0 = (AbstractNode)nodeMap.get(c.getSource());
        if (n0 == null) {
            n0 = new LumpedNode(c.getSource());
            nodeMap.put(c.getSource(),n0);
        }

        AbstractNode n1 = (AbstractNode)nodeMap.get(c.getDrain());
        if (n1 == null) {
            n1 = new LumpedNode(c.getDrain());
            nodeMap.put(c.getDrain(),n1);
        }

        CapDevice cd = new CapDevice(n0,n1,(float)c.getCapacitance());
        n0.connectCap(cd);
        n1.connectCap(cd);

        numCapacitors++;
    }

    /*****************************************************************
     * Add a (MOSFET) transistor to the circuit graph
     *****************************************************************/
    public void addTransistor(TransistorInterface t)
    {
        AbstractNode d = (AbstractNode)nodeMap.get(t.getDrain());
        if (d == null) {
            d = new LumpedNode(t.getDrain());
            nodeMap.put(t.getDrain(),d);
        }

        AbstractNode g = (AbstractNode)nodeMap.get(t.getGate());
        if (g == null) {
            g = new LumpedNode(t.getGate());
            nodeMap.put(t.getGate(),g);
        }

        AbstractNode s = (AbstractNode)nodeMap.get(t.getSource());
        if (s == null) {
            s = new LumpedNode(t.getSource());
            nodeMap.put(t.getSource(),s);
        }

        MosDevice m = new MosDevice(d,g,s,t.getType(),t.getWidth(),
                                    t.getLength());
        d.connectMos(m);
        g.connectMos(m);
        s.connectMos(m);

        numTransistors++;
    }

    /*****************************************************************
     * Add a diode to the circuit graph (unimplemented).
     *****************************************************************/
    public void addDiode(DiodeInterface d) {}

    /*****************************************************************
     * Returns the number of resistors in the circuit.
     *****************************************************************/
    public int getResistorCount() { return numResistors; }

    /*****************************************************************
     * Returns the number of capacitors in the circuit.
     *****************************************************************/
    public int getCapacitorCount() { return numCapacitors; }

    /*****************************************************************
     * Returns the number of transistors in the circuit.
     *****************************************************************/
    public int getTransistorCount() { return numTransistors; }

    /*****************************************************************
     * Returns the number of diodes in the circuit.
     *****************************************************************/
    public int getDiodeCount() { return 0; }

    /*****************************************************************
     * Returns the number of nodes in the circuit.
     *****************************************************************/
    public int getNodeCount() { return nodeMap.keySet().size(); }

    /*****************************************************************
     * Searches the graph for all independent resistive networks.
     * For each of these, tallies total capacitance and identifies
     * driver and gate nodes.
     *****************************************************************/
    public void identifyNets(final CallbackInterface callback)
    {
        HashSet uncoveredNodes = new HashSet(nodeMap.values());
        int numNets = 0;
        if (callback != null) 
            callback.identifyNetsUpdate(numNets,uncoveredNodes);
        while (!uncoveredNodes.isEmpty()) {
            Iterator it = uncoveredNodes.iterator();
            AbstractNode base = (AbstractNode)it.next();
            ResistiveSubnet rsn = new ResistiveSubnet(base);
            base.buildResistiveSubnet(rsn,uncoveredNodes);
            //
            // Add this subnet to the resSubnets set as long as it 
            // isn't GND,Vdd,_SReset,_PReset, and contains at least
            // one resistor.
            //
            if (!rsn.getCanonicalNameAsString().equals("_SReset") &&
                !rsn.getCanonicalNameAsString().equals("_SReset!") &&
                !rsn.getCanonicalNameAsString().equals("_PReset") &&
                !rsn.getCanonicalNameAsString().equals("_PReset!") &&
                !rsn.getCanonicalNameAsString().equals("GND") &&
                !rsn.getCanonicalNameAsString().equals("Vdd") &&
                !rsn.isDegenerate())
                resSubnets.add(rsn);
            // 
            // Update net count and do callback update
            //
            numNets++;
            if (callback != null) 
                callback.identifyNetsUpdate(numNets,uncoveredNodes);
        }
        clearMarks();
    }

    /*****************************************************************
     * Returns an iterator over all the resistive subnets in the
     * circuit graph.  For best results (i.e. non-null), should be 
     * called after calling 
     * {@link #identifyNets(com.avlsi.circuit.LumpedRCGraph.CallbackInterface)}.
     *****************************************************************/
    public Iterator getNetsIterator() { 
        return resSubnets.iterator(); 
    }

    /*****************************************************************
     * Returns the set of all resistive subnets in the circuit graph.
     * Must be called after 
     * {@link #identifyNets(com.avlsi.circuit.LumpedRCGraph.CallbackInterface)}.
     *****************************************************************/
    public final Set getNets() { 
        return resSubnets;
    }

    /*************************************************
     * Clears all node marks throughout the net graph.
     *************************************************/
    public void clearMarks()
    {
        // Note: numMarks argument to clearAllMarks will eventually
        // require careful consideration...
        Iterator it = nodeMap.values().iterator();
        while (it.hasNext()) ((AbstractNode)it.next()).clearAllMarks(2);
    }

    /*************************************************
     * Static subcircuit repository nested subclass.
     *************************************************/
    public static class Repository extends AbstractCircuit.Repository {
        /**
         * LumpedRCGraph generator method.
         **/
        protected AbstractCircuit newCircuitGenerator(final String cellType) {
            return new LumpedRCGraph(cellType);
        }
    }

    /*****************************************************************
     * Class to represent exceptions in LumpedRCGraph processing.
     *****************************************************************/
    public static final class Exception extends java.lang.Exception {
        public Exception(final String message) {
            super(message);
        }
    }
}

