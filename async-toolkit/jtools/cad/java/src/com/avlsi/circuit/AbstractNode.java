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

import java.util.Iterator;
import java.util.HashSet;
import java.util.ArrayList;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.util.debug.Debug;

/**********************************************************************
 * Circuit graph node abstract base class.
 * @author Mike Davies
 * @version $Revision$ $Date$
 **********************************************************************/
public abstract class AbstractNode {

    static boolean VDN_DEBUG;
    static boolean GATERC_DEBUG;
    static boolean CONJUNCTIVE_PATH_DEBUG;

    /** The node name **/
    public HierName name;

    /****************************************************************
     * Note that mark state for CapEdges and MosEdges are stored in
     * the Node class (so they're directed), since these devices are
     * accounted for on a per-terminal basis.  Resistor mark state
     * is stored in the ResEdge class, since they are accounted for
     * on a per-device basis.
     ****************************************************************/

    final private boolean[] mark;

    /** 
     * Constructor.  Takes a node name and the number of marks this 
     * node will need.
     **/
    AbstractNode(final HierName name, int numMarks) {
        this.name=name;
        mark = new boolean[numMarks];
    }

    //
    // Abstract methods to be implemented by subclasses:
    //

    /** Connects a resistor to the node. **/
    abstract void connectRes(final ResDevice r);

    /** Connects a capacitor to the node. **/
    abstract void connectCap(final CapDevice c);

    /** Connects a transistor to the node. **/
    abstract void connectMos(final MosDevice m);

    /** Connects a diode to the node. **/
    abstract void connectDiode(final DiodeDevice m);

    /** Returns an iterator over all MosDevices connected to this node. **/
    public abstract Iterator getMosDevices();

    /** Returns an iterator over all ResDevices connected to this node. **/
    public abstract Iterator getResDevices();

    /** Returns an iterator over all CapDevcies connected to this node. **/
    public abstract Iterator getCapDevices();

    /** Returns an iterator over all DiodeDevices connected to this node. **/
    public abstract Iterator getDiodeDevices();

    /** Clears all marks in attached device edges **/
    abstract void clearDeviceMarks();

    /** Returns total wire capacitance lumped to this node **/
    public abstract float wireCap();

    /** Returns total gate capacitance lumped to this node **/
    public abstract float gateCap();

    /** Returns total "good" capacitance (lumped to a fixed node) **/
    public abstract float goodCap();

    /** 
     * Returns total "bad" capacitance (lumped to nodes that could 
     * transition during this node's transition).
     **/
    public abstract float badCap();

    /** Returns number of (wire) Capacitors attached to this node **/
    public abstract int numCap();

    /** Returns number of resistors attached to this node **/
    public abstract int numRes();

    /** Returns number of Mosfet attached to this node via source/drain **/
    public abstract int numDrivers();

    /** Returns number of gates attached to this node **/
    public abstract int numGates();

    //
    // Shared methods/algorithms
    //

    /** Sets the specified mark **/
    void setMark(int num) { mark[num] = true; }

    /** Clears the specified mark **/
    void clearMark(int num) { mark[num] = false; }

    /** Returns state of specified mark **/
    boolean getMark(int num) { return mark[num]; }

    /** Clears all marks associated with this node **/
    void clearAllMarks(int numMarks) {
        clearDeviceMarks();
        for (int i=0; i<numMarks; i++) mark[i]=false;
    }

    /*****************************************************************
     * Returns true if this node belongs to the same resistive 
     * subnetwork as the specified AbstractNode.  The determination is 
     * based on the names of the two nodes: returns true if they are 
     * identical up to the last ':' (or until the end of the string if 
     * there is no ':').  This is obviously dependent on the behavior
     * of the extractor.  (Valid with Assura RCX and Simplex.)
     *****************************************************************/
    public boolean sameResNet(final AbstractNode n) 
    {
        String baseName = name.getAsString('.');
        int i = baseName.lastIndexOf(':');
        if (i != -1) baseName = baseName.substring(0,i);
        return n.name.getAsString('.').startsWith(baseName);
    }

    /*****************************************************************
     * Returns true if the node's name, up to the final ':' if
     * present, matches the specified string.  Hierarchy delimiter
     * character is '.'.
     *****************************************************************/
    public boolean sameResNet(final String net) {
        String baseName = name.getAsString('.');
        int i = baseName.lastIndexOf(':');
        if (i != -1) baseName = baseName.substring(0,i);
        return net.equals(baseName);
    }

    /*****************************************************************
     * True if the node HierName's suffix matches either "GND" or
     * "GND!" case-insensitively.
     *****************************************************************/
    public boolean isGND() {
        String s = name.getSuffixString().toLowerCase();
        if (s.equals("gnd") || s.equals("gnd!"))
            return true;
        else 
            return false;
    }

    /*****************************************************************
     * True if the node HierName's suffix matches either "Vdd" or
     * "Vdd!" case-insensitively.
     *****************************************************************/
    public boolean isVdd() {
        String s = name.getSuffixString().toLowerCase();
        if (s.equals("vdd") || s.equals("vdd!"))
            return true;
        else 
            return false;
    }

    /*****************************************************************
     * True if the ndoe HierName's suffix is a number or ends with a
     * '#', after ignoring any trailing ":xxx" resistive subnet
     * identifier.
     *****************************************************************/
    public boolean isInternal() {
        String s = name.getSuffixString();
        int i = s.indexOf(':');
        s = s.substring(0, (i==-1) ? s.length() : i);
        if (s.endsWith("#")) return true;
        try {
            Integer.parseInt(s);
        }
        catch (NumberFormatException e) {
            return false;
        }
        return true;
    }

    /*****************************************************************
     * Returns the node's name as a string with a trailing suffix
     * of the form ".x(:Y)?$" removed, where x is an integer between
     * 0 and 3, 'd', or 'e', and X is any string (loose regexp syntax).  
     * Note that although '.' is used as the hierarchy delimiter in 
     * the returned string, the '.' in the suffix matching is done on 
     * the HierName's suffix String.
     *****************************************************************/
    public String channelBaseName() {
        String s = name.getSuffixString();
        int i = s.lastIndexOf(':');
        s = s.substring(0, (i==-1) ? s.length() : i);
        i = s.lastIndexOf('.');
        if (i == -1 || i == s.length()-1) return s;
        else {
            String b = s.substring(i+1);
            if (b.equals("e") || b.equals("d") || b.equals("0") ||
                b.equals("1") || b.equals("2") || b.equals("3"))
                return b;
            else return s;
        }
    }

    /**********************************************************
     * Recursive depth-first-search routine to construct this
     * node's resistive subnet.  Each Node in the search trace
     * removes itself from the uncoveredNodes set and updates
     * various state calculations within the ResistiveSubnet.
     * Note that a particular Node <em>may</em> be covered more than
     * once, in cases where cycles exist in the graph.  The
     * marking scheme adequately handles such cases.
     * <p>
     * Node.mark[0] is used in this algorithm.  The method 
     * expects {@link #wireCap()}, {@link #gateCap()}, and
     * {@link #numDrivers()} to return meaningful values.  For
     * a non-degenerate search, {@link #getResDevices()} must 
     * return a non-empty Iterator.
     **********************************************************/
    void buildResistiveSubnet(ResistiveSubnet rsn, HashSet uncoveredNodes) 
    {
        //
        // Check if we've touched this node already, if so
        // set has_cycles; if not, remove this node from the 
        // set of uncovered nodes
        //
        if (!mark[0]) {
            uncoveredNodes.remove(this);
            Iterator it = getResDevices();
            if (!it.hasNext()) rsn.setDegenerate();
            rsn.addNode(this);
            mark[0] = true;
            //
            // Follow resistive connections
            //
            while (it.hasNext()) {
                ResDevice r = (ResDevice)it.next();
                if (!r.getMark()) {
                    r.setMark();
                    r.nextAfter(this).buildResistiveSubnet(rsn,uncoveredNodes);
                }
            }
        }
        else rsn.encounteredCycle();
    }

    /**********************************************************
     * Recursive depth-first-search routine to construct this
     * node's resistive subnet test circuit.  For nodes in the
     * subnet, {@link #getCapDevices()}, {@link #getMosDevices()},
     * {@link #getResDevices()}, and {@link #getDiodeDevices()}
     * should all be meaningful.
     * <p>
     * Uses mark[0], and requires that all nodes in the circuit
     * are initially unmarked.
     **********************************************************/
    void buildTestSubnet(TestSubnet subnet)
    {
        if (!mark[0]) {
            mark[0] = true;
            subnet.addSubnetNode(this);
            // 
            // Add capacitors
            //
            Iterator it = getCapDevices();
            while (it.hasNext()) {
                CapDevice cd = (CapDevice)it.next();
                //
                // Add the capacitor to the test subnet, if the
                // other terminal hasn't been added to the subnet.
                //
                AbstractNode n = cd.nextAfter(this);
                if (!n.mark[0]) subnet.addCapacitor(cd,n);
            }
            // 
            // Add transistors (for driver nodes, this
            // will cause the pull-up/pull-down network
            // attached to this node to be determined).
            //
            it = getMosDevices();
            while (it.hasNext()) {
                MosDevice md = (MosDevice)it.next();
                if (md.gateNode() == this)
                    subnet.addGateMos(md);
                else
                    subnet.addDriverNode(this);
            }
            // 
            // Add diodes
            //
            it = getDiodeDevices();
            while (it.hasNext()) {
                DiodeDevice dd = (DiodeDevice)it.next();
                subnet.addDiode(dd);
            }
            //
            // Follow resistive edges.
            //
            it = getResDevices();
            while (it.hasNext()) {
                ResDevice rd = (ResDevice)it.next();
                if (!rd.getMark()) {
                    rd.setMark();
                    subnet.addResistor(rd);
                    if (! (rd.nextAfter(this) instanceof Node)) {
                        System.err.println("Error: "+
                          rd.nextAfter(this).name.getAsString('.')+
                          " isn't a Node.");
                    }
                    rd.nextAfter(this).buildTestSubnet(subnet);
                }
            }
        }
        else subnet.encounteredCycle();
    }

    /** Helper class for buildConjunctivePaths() **/
    static class GateSourceType {
        HierName gname;
        HierName sname;
        int t;
        boolean reached_supply;
        GateSourceType(AbstractNode g, AbstractNode s, int t) {
            this.gname = g.name.getResistiveSubnetName(); 
            this.sname = s.name.getResistiveSubnetName(); 
            this.t = t;
        }
        boolean equals(GateSourceType gst) {
            return (gst != null) && gst.gname.equals(gname) && 
                   gst.sname.equals(sname) && (gst.t == t);
        }
    }

    boolean buildConjunctivePaths(ConjunctivePath p, TestSubnet subnet)
    {
        if (CONJUNCTIVE_PATH_DEBUG) {
            System.err.print(this.name.getAsString('.')+": ");
            p.printToStream(System.err);
        }
        // 
        // First check path termination condition, i.e. if this
        // node is GND or Vdd, or if this path ended up back at
        // a subnet driver node.
        //
        if (isGND() || isVdd()) {
            p.addTerm(this.name,ConjunctivePath.SUPPLY);
            subnet.addConjunctivePath(p);
            return true;
        }
        else if (subnet.isDriverNode(this) && p.getNumTerms() > 0 ||
                 !isInternal() && p.getNumTerms() > 1) {
            //
            // NOTE: Forced to add the !isInternal() condition to 
            // deal with sneak paths.  Since we don't know anything
            // about the exclusion relationships between input
            // nodes, we can't do any better than to assume that any
            // named node belongs to a different (mutually exclusive)
            // output subnet.
            //     This assumption will definitely fail for most 
            // pass-gates layout (since both sides typically are named), 
            // so for that reason the getNumTerms() > 1 condition is 
            // added.  This works since no shared gate network will 
            // ever separate two output nodes by a single transistor.
            //     This assumption relies on the internal nodes of
            // transistor pull-up/down networks being the only numbered
            // nodes in the cell under analysis.
            // 
            if (CONJUNCTIVE_PATH_DEBUG)
                System.err.println("Reached driver node "+
                                   name.getAsString('.'));
            return false;
        }
        //
        // Add diodes for this node
        // 
        /*
        if (!subnet.isInternalDriverNode(this)) {
            Iterator it = getDiodeDevices();
            System.err.print("Adding diodes to "+name.getAsString('.')+": ");
            while (it.hasNext()) {
                System.err.print(".");
                DiodeDevice d = (DiodeDevice) it.next();
                subnet.addDiode(d);
            }
            System.err.println("");
        }
        */
        mark[0] = true;
        // 
        // Otherwise...
        // If we haven't reached a supply rail, split the path for
        // each transistor branch.
        //
        boolean reached_supply = false;
        ArrayList driverTerms = new ArrayList();
        Iterator it = getMosDevices();
        while (it.hasNext()) {
            MosDevice md = (MosDevice)it.next();
            if (md.drainNode() == this && !md.sourceNode().mark[0] ||
                md.sourceNode() == this && !md.drainNode().mark[0]) {
                AbstractNode g = md.gateNode();
                AbstractNode s = (md.drainNode()==this) ? md.sourceNode() :
                                                          md.drainNode();
                // 
                // Determine if this transistor is logically 
                // redundant, i.e. if we've already encountered a
                // transistor with the same (s,g,type).
                //
                GateSourceType gst = new GateSourceType(g,s,md.getType());
                Iterator jt = driverTerms.iterator();
                boolean redundant = false;
                while (jt.hasNext()) {
                    GateSourceType gstDone = (GateSourceType)jt.next();
                    if (gst.equals(gstDone)) {
                        redundant = true;
                        if (gstDone.reached_supply)
                            subnet.addDriverMos(md);
                    }
                }
                if (CONJUNCTIVE_PATH_DEBUG)
                    System.err.println("redundant = "+redundant);
                // 
                // If it isn't redundant, continue the path down each
                // transistor branch.
                //
                if (!redundant) {
                    driverTerms.add(gst);
                    // Branch the ConjunctivePath
                    ConjunctivePath pb = new ConjunctivePath(p);
                    // 
                    // Only add the term if it's non-constant.
                    // If the gate is constant (supply or reset), and
                    // if it permanently turns the transistor off, 
                    // terminate the conjunctive path here.
                    //
                    String suffix = 
                        g.name.getResistiveSubnetName().getSuffixString();
                    if (suffix.equals("GND!")) {
                        if (md.getType() == DeviceTypes.N_TYPE) continue;
                        // else Don't add the term since it's constant
                    }
                    else if (suffix.equals("Vdd!") || suffix.equals("_PReset!")
                             || suffix.equals("_SReset!")) {
                        if (md.getType() == DeviceTypes.P_TYPE) continue;
                        // else Don't add the term since it's constant
                    }
                    else {
                        pb.addTerm(g.name.getResistiveSubnetName(),
                                   md.getType());
                    }
                    if (s.buildConjunctivePaths(pb,subnet)) {
                        // This branch reached a supply rail
                        reached_supply = true;
                        gst.reached_supply = true;
                    }
                    if (gst.reached_supply) {
                        subnet.addDriverMos(md);
                    }
                }
            }
        }
        //
        // Unmark on the way back up
        //
        mark[0] = false;
        return reached_supply;
    }


    /**********************************************************
     * Recursively calculates effective R and C beginning from
     * all leaves with driver nodes.  Nodes with !mark[0] are
     * not followed.  The wire cap is summed over all paths, 
     * the resistance is combined over all paths as if the 
     * driver strengths are all identical (i.e. weighted by
     * the number of drivers on each path).  Also calculates
     * the maximum R over all driver paths, and the C along
     * that path.
     * <p>
     * mark[0..1] are used by this algorithm.  The method also
     * requires {@link #getResDevices()}, {@link #numDrivers()}, and
     * {@link #wireCap()} to have meaningful implementations.
     **********************************************************/
    ResistiveSubnet.DriverRCData calculateRCtoDrivers()
    {
        ResistiveSubnet.DriverRCData rcd =
            new ResistiveSubnet.DriverRCData();
        Iterator it = getResDevices();
        mark[1] = true;
        while (it.hasNext()) {
            ResDevice rd = (ResDevice)it.next();
            if (!rd.getMark()) {
                rd.setMark();
                AbstractNode n = rd.nextAfter(this);
                if (n.mark[0] && !n.mark[1]) {
                    ResistiveSubnet.DriverRCData subrcd =
                        n.calculateRCtoDrivers();
                    if (VDN_DEBUG) System.err.println("vdn_R - "
                        +n.name.getAsString('.')+" reported "
                        +subrcd.R+" ("+subrcd.numDrivers+")");
                    rcd.R += subrcd.numDrivers*(rd.getValue() + subrcd.R);
                    rcd.numDrivers += subrcd.numDrivers;
                    if (rd.getValue() + subrcd.maxR > rcd.maxR 
                        && subrcd.numDrivers > 0) {
                        rcd.maxR = rd.getValue() + subrcd.maxR;
                        rcd.maxC = subrcd.maxC;
                    }
                    if (subrcd.numDrivers > 0) rcd.C += subrcd.C;
                }
            }
        }
        rcd.numDrivers += this.numDrivers();
        if (rcd.numDrivers > 0) {
            rcd.R /= rcd.numDrivers;
            rcd.C += wireCap();
            rcd.maxC += wireCap();
        }
        return rcd;
    }

    /*****************************************************************
     * Depth-first search to find the maximum resistive path from VDN
     * to gate load, over all such paths in the resistive subnet.
     * This method assumes that {@link #getResDevices()} and {@link
     * #gateCap()} are meaningfully defined. mark[0] is used in the
     * search.
     * <p>
     * This algorithm requires that the resistive subnetwork be 
     * cycle-free.  However, if the network does contain cycles, the
     * maximum resistance will be overestimated, which will often be
     * acceptable for conservative circuit analysis.
     *****************************************************************/
    float findMaxPathRes()
    {
        float maxres = -1.0f;
        Iterator it = getResDevices();
        mark[0] = true;
        while (it.hasNext()) {
            ResDevice rd = (ResDevice)it.next();
            AbstractNode n = rd.nextAfter(this);
            if (!n.mark[0]) {
                float res = n.findMaxPathRes();
                if (GATERC_DEBUG) System.err.println("findMaxPathRes() - "
                    +n.name.getAsString('.')+" reported "
                    +res+" ("+(res >= 0.0 ? "has":"no")+" gates)");
                if (res >= 0.0f && rd.getValue() + res > maxres)
                    maxres = rd.getValue() + res;
            }
        }
        // maxres of -1.0 is returned if no gate cap exists at
        // or beyond this node.
        if (maxres == -1.0f && gateCap() > 0.0f) maxres = 0.0f;
        return maxres;
    }
}

