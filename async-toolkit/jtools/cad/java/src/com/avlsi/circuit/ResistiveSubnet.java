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
import java.util.Comparator;

import com.avlsi.file.common.HierName;

/**********************************************************************
 * Class representing a resistive subnetwork of a CircuitGraph.
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **********************************************************************/
public class ResistiveSubnet {

    static boolean VDN_DEBUG;
    static boolean GATERC_DEBUG;
    static boolean USE_MAX_VDN_RC = true;

    /*********************************************
     * Scratch data class for VDN RC calculation.
     *********************************************/
    static class DriverRCData {
        public float R;
        public float C;
        public float maxR;
        public float maxC;
        public int numDrivers;
    }
    
    /** Subnet's base node */
    private AbstractNode base;

    /** Subnet's canonical node name */
    private HierName canonicalName; 

    /** Cycles detected in subnet graph? */
    private boolean has_cycles;  
       
    /** Total wire cap tally */
    private float wireCap; 
             
    /** Total gate cap tally */
    private float gateCap; 
             
    /** Total good cap tally */
    private float goodCap;

    /** True when base is VDN */
    private boolean base_is_vdn; 
       
    /** Resistance from drivers to VDN */
    private float vdnRes; 
              
    /** Wire cap from drivers to VDN */
    private float vdnCap;
               
    /** Max resistance from VDN to gate */
    private float maxPathRes;           

    /** nodes connected to source/drains **/
    final private ArrayList driverNodes = new ArrayList();

    /** nodes connected to gates **/
    final private ArrayList gateNodes = new ArrayList();

    /** 
     * Constructor.  Requires any node within the subnet
     * as the initial base node.
     **/
    ResistiveSubnet(AbstractNode b) { base = b; }

    /**
     * Adds an AbstractNode to the subnet.
     **/
    void addNode(AbstractNode n) {
        //
        // Accumulate wire cap
        //
        wireCap += n.wireCap();
        goodCap += n.goodCap();
        gateCap += n.gateCap();
        //
        // Classify mosfet terminals
        //
        if (n.gateCap() > 0.0f) gateNodes.add(n);
        if (n.numDrivers() > 0) driverNodes.add(n);
        //
        // Check if this node has a more canonical name
        //
        if (canonicalName==null || 
            n.name.getResistiveSubnetName().compareTo(canonicalName)<0)
            canonicalName = n.name.getResistiveSubnetName();
    }

    /**
     * Sets this subnet to be degenerate (contains no resistive
     * segments.
     **/
    void setDegenerate() {
        // reusing available bits...
        vdnRes = -1.0f;
    }

    /** Call this when a cycle is encountered in the subnet.  **/
    void encounteredCycle() { has_cycles = true; }

    /**
     * Returns true if this subnet contains no resistive segments
     * (i.e. it is one node.)
     **/
    public boolean isDegenerate() { return vdnRes == -1.0f; }

    /** 
     * Returns true if one or more cycles have been detected in the
     * subnet's resistive graph.
     **/
    public boolean hasCycles() { return has_cycles; }

    /** Returns an Iterator over the subnet's driver AbstractNodes.  **/
    public Iterator getDriverNodes() { return driverNodes.iterator(); }

    /** Returns an Iterator over the subnet's gate AbstractNodes.  **/
    public Iterator getGateNodes() { return gateNodes.iterator(); }

    /** Returns the subnet's total wire capacitance. **/
    public float getWireCap() { return wireCap; }

    /** Returns the subnet's total capacitance lumped to fixed nodes. **/
    public float getGoodCap() { return goodCap; }

    /** Returns the subnet's total gate load capacitance. **/
    public float getGateCap() { return gateCap; }
    
    /*****************************************************************
     * Returns the subnet's effective resistance from VDN to drivers.
     * When {@link #USE_MAX_VDN_RC} is set, this is the max resistance
     * over all paths from VDN to driver nodes.  Otherwise, it is some
     * funky parallel combination over all paths (assumes all driver
     * conductances are equal and much greater than the wire resistance.)
     * Will be 0.0 before {@link #identifyVDN()} is called.
     *****************************************************************/
    public float getVdnRes() { 
        return (vdnRes < 0.0f) ? 0.0f : vdnRes; 
    }

    /*****************************************************************
     * Returns the subnet's total wire capacitance from VDN to drivers,
     * not including the wire cap attached to the VDN node itself. 
     *****************************************************************/
    public float getVdnCap() { return vdnCap; }

    /*****************************************************************
     * Returns the max resistance over all paths from VDN to gate
     * load nodes.  Will be 0.0 before {@link #findMaxPathRes()} is
     * called.  Will be -1.0 if the subnet contains no gate load.
     *****************************************************************/
    public float getMaxPathRes() { return maxPathRes; }

    /*****************************************************************
     * Returns the maximum RC delay this net might see, namely
     * maxRC = maxPathRes * (0.5 * wireCap + gateCap).
     * (Assumes wire cap is distributed and all the gate cap is
     * lumped at the end of the path w/ maximum resistance.)
     *****************************************************************/
    public float getMaxRC() {
        return maxPathRes * (0.5f * wireCap + gateCap);
    }

    /*****************************************************************
     * Routine to identify the subnet's virtual driver node.
     * {@link #base } is set to the VDN.  Nodes in the subnet 
     * should either be of LumpedNode or ExplicitNode type.
     * <p>
     * NOTE: Nodes in the resistive net are left marked (mark index 0).
     * Will need to clear all marks before other algorithms (such as 
     * {@link #findMaxPathRes()}) can be executed.
     *****************************************************************/
    public void identifyVDN()
    {
        //
        // Only proceed if there's at least two driver nodes
        // and this subnet isn't degenerate
        //
        int driverCount = driverNodes.size();
        if (driverCount == 1) {
            base = (AbstractNode)driverNodes.iterator().next();
            base_is_vdn = true;
        }
        if (driverCount <= 1 || vdnRes < 0.0f) return;
        //
        // Initialization
        //
        final HashMap markOwners = new HashMap(driverCount*10);
        final HashSet[] activePaths = new HashSet[driverCount];
        final AbstractNode[] headNodes = new AbstractNode[driverCount];
        int[] mergeCount = new int[driverCount];
        int maxMergeCount = 0;
        int i;
        for (i=0; i<driverCount; i++) {
            final AbstractNode n = (AbstractNode)driverNodes.get(i);
            activePaths[i] = new HashSet(10);
            activePaths[i].add(n);
            n.setMark(0);
            markOwners.put(n,new Integer(i));
            mergeCount[i] = 1;
        }
        //
        // Breadth-first search from all driver nodes
        // until all paths have joined.
        //
        Iterator it;
        AbstractNode m = (AbstractNode)driverNodes.get(0);
        i = 0;
        while (maxMergeCount < driverCount) {
            it = activePaths[i].iterator();
            final HashSet newPaths = new HashSet(activePaths[i].size()*2);
            while (mergeCount[i]>0 && it.hasNext()) {
                final AbstractNode n = (AbstractNode)it.next();
                Iterator jt = n.getResDevices();
                while (mergeCount[i]>0 && jt.hasNext()) {
                    ResDevice r = (ResDevice)jt.next();
                    m = r.nextAfter(n);
                    if (VDN_DEBUG) System.err.println("vdn - "+i+": "+
                        n.name.getAsString('.')+"->"+
                        m.name.getAsString('.'));
                    if (m.getMark(0)) {
                        // This node has been covered: merge with 
                        // owner if it's not me and it's still active.
                        int owner = ((Integer)markOwners.get(m)).
                                    intValue();
                        if (owner != i) {
                            // only merge if this isn't a cycle
                            if (mergeCount[owner] > 0) {
                                // only merge if the owner is still active
                                mergeCount[owner] += mergeCount[i];
                                mergeCount[i] = 0;
                                if (mergeCount[owner] > maxMergeCount)
                                    maxMergeCount = mergeCount[owner];
                                activePaths[i].clear();
                            }
                            else {
                                // otherwise claim this node
                                markOwners.put(m,new Integer(i));
                                newPaths.add(m);
                            }
                        }
                    }
                    else {
                        // This node is new to the search:
                        // mark it and claim ownership.
                        m.setMark(0);
                        markOwners.put(m,new Integer(i));
                        newPaths.add(m);
                    }
                }
                if (mergeCount[i]>0) it.remove();
            }
            // Add all newfound paths to active
            // list, unless we merged.
            if (mergeCount[i]>0) activePaths[i].addAll(newPaths);
            i = (i+1) % driverCount;
        }
        //
        // Virtual driver node will be the last merged Node, m.
        //
        base = m;
        base_is_vdn = true;
        // 
        // Now search backwards from the VDN to determine
        // the effective resistance & wire cap at the VDN.
        // Subtract wire cap of base node from total since
        // otherwise it will be double counted in 
        // findMaxPathRes().
        //
        DriverRCData vdnrc = base.calculateRCtoDrivers();
        if (!USE_MAX_VDN_RC) {
            vdnRes = vdnrc.R;
            vdnCap = vdnrc.C - base.wireCap();
        }
        else {
            vdnRes = vdnrc.maxR;
            vdnCap = vdnrc.maxC - base.wireCap();
        }
    }

    /** 
     * Returns VDN node.  May call {@link #identifyVDN()} if it hasn't
     * yet been.
     **/
    public AbstractNode getVDN() {
        if (!base_is_vdn) identifyVDN();
        return base;
    }

    /*****************************************************************
     * Determines path from VDN to gate load node with maximum 
     * resistance.  Resistance value can be obtained with 
     * {@link #getMaxPathRes()} afterwards.
     *****************************************************************/
    public void findMaxPathRes() {
        if (!base_is_vdn) identifyVDN();
        maxPathRes = base.findMaxPathRes() + vdnRes;
    }

    /*****************************************************************
     * Returns the subnet's canonical HierName.
     *****************************************************************/
    public HierName getCanonicalName() {
        return canonicalName;
    }

    /*****************************************************************
     * Returns a String containing the ResistiveSubnets's canonical
     * net name, stripped of any trailing ':\w+' suffix.  Hierarchy
     * separation character is '.'.
     *****************************************************************/
    public String getCanonicalNameAsString() {
        return canonicalName.getAsString('.');
    }

    /*****************************************************************
     * Comparator to sort ResistiveSubnets by max path resistance.
     *****************************************************************/
    static final class ResComparator implements Comparator {
        public int compare(Object o1, Object o2) {
            final ResistiveSubnet rs1 = (ResistiveSubnet) o1;
            final ResistiveSubnet rs2 = (ResistiveSubnet) o2;
            float dr = rs1.maxPathRes - rs2.maxPathRes;
            if (dr == 0.0f) return 0;
            else return (dr < 0.0f) ? -1 : 1;
        }
        public boolean equals(final Object o) {
            return getClass() == o.getClass();
        }
    }

    /*****************************************************************
     * Comparator to sort ResistiveSubnets by 
     * max RC = maxPathR * (0.5 * wireCap + gateCap)
     *****************************************************************/
    static final class ResCapComparator implements Comparator {
        public int compare(Object o1, Object o2) {
            final ResistiveSubnet rs1 = (ResistiveSubnet) o1;
            final ResistiveSubnet rs2 = (ResistiveSubnet) o2;
            float drc = rs1.getMaxRC() - rs2.getMaxRC();
            if (drc == 0.0f) return 0;
            else return (drc < 0.0f) ? -1 : 1;
        }
        public boolean equals(final Object o) {
            return getClass() == o.getClass();
        }
    }

    /*****************************************************************
     * Comparator to sort ResistiveSubnets by 
     * wireCap / (wireCap + gateCap) 
     * (correlates with coupling susceptibility).
     *****************************************************************/
    static final class CouplingComparator implements Comparator {
        public int compare(Object o1, Object o2) {
            final ResistiveSubnet rs1 = (ResistiveSubnet) o1;
            final ResistiveSubnet rs2 = (ResistiveSubnet) o2;
            float wg1 = (rs1.gateCap == 0.0f) ? 1.0f :
                        rs1.wireCap / (rs1.wireCap + rs1.gateCap);
            float wg2 = (rs2.gateCap == 0.0f) ? 1.0f :
                        rs2.wireCap / (rs2.wireCap + rs2.gateCap);
            float dwg = wg1 - wg2;
            if (dwg == 0.0f) return 0;
            else return (dwg < 0.0f) ? -1 : 1;
        }
        public boolean equals(final Object o) {
            return getClass() == o.getClass();
        }
    }

    /** ResComparator generator **/
    public static ResComparator newResComparator() {
        return new ResComparator();
    }

    /** ResCapComparator generator **/
    public static ResCapComparator newResCapComparator() {
        return new ResCapComparator();
    }

    /** CouplingComparator generator **/
    public static CouplingComparator newCouplingComparator() {
        return new CouplingComparator();
    }
}

