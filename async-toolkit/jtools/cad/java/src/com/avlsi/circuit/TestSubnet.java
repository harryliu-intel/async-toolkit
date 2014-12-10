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
import java.io.PrintWriter;
import java.io.PrintStream;
import java.io.OutputStream;
import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.Capacitor;
import com.avlsi.file.aspice.AspiceCell;
import com.avlsi.file.aspice.CastTestEnvironment;
import com.avlsi.file.aspice.Resistor;
import com.avlsi.file.aspice.Diode;
import com.avlsi.file.aspice.Transistor;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.text.NumberFormatter;

/**********************************************************************
 * Class representing a resistive subnetwork with associated 
 * transistors and parasitic capacitance, for slew rate and capacitive
 * coupling test structure generation.
 * <p>
 * Assumptions made:
 * <ul>
 *  <li> No diode-connected transistors.  They will likely cause
 *       duplicate transistors to be added to the AspiceCell circuit.
 *  <li> No degenerate moscap structures (source and drain shorted).
 * </ul>
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **********************************************************************/
public final class TestSubnet {

    public static boolean CONJUNCTIVE_PATH_DEBUG;
    public static int   DSIM_TIME_TAU_MAX = 1500;
    public static float DTU               = 5e-13f;
    public static float T_RESET           = 2e-9f;

    public static HierName GND = HierName.makeHierName("GND!");
    public static HierName Vdd = HierName.makeHierName("Vdd!");

    /** This subnet's resistive subnet name **/
    private HierName name;

    /** The most canonical name of all the subnet's node names **/
    private HierName canonicalName;

    /** Cycles detected in subnet graph? **/
    private boolean has_cycles;

    /** AspiceCell to be simulated **/
    //private AspiceCell cell;
    private String cell;

    /** Nodes in the resistive subnet **/
    private HashSet subnetNodes = new HashSet();

    /** Map of coupling node names to coupling capacitance **/
    private HashMap couplingCap = new HashMap();

    /** Subnets (as res subnet HierNames) that are driven by this net **/
    private HashMap  drivenSubnets = new HashMap();

    /** nodes connected to source/drains **/
    private HashSet driverNodes = new HashSet();

    /** HierNames (res subnet names) of internal driver network nodes **/
    private HashSet internalDriverNodes = new HashSet();

    /** nodes connected to gates **/
    private ArrayList gateNodes = new ArrayList();

    /** Set of all conjunctive paths driving this node up **/
    private HashSet conjunctivePullUpPaths = new HashSet();

    /** Set of all conjunctive paths driving this node down **/
    private HashSet conjunctivePullDownPaths = new HashSet();

    /** Number of feedback (staticizer) pull-up paths **/
    private int numFeedbackPullUps;

    /** Number of feedback (staticizer) pull-down paths **/
    private int numFeedbackPullDowns;

    /** Set of all MosDevices in driver networks **/
    private HashSet driverMosDevices = new HashSet();

    /** Set of input driver subnets (terms in pull-up/down paths) **/
    private HashSet inputSubnets = new HashSet();

    /** 
     * Set of all subnets that are both driven by this subnet and
     * drive this subnet.  (Staticizer nodes.)
     **/
    private HashSet feedbackSubnets = new HashSet();

    /**
     * Cast test environment file object.
     **/
    private CastTestEnvironment castEnv;

    /**
     * List of resistors in subnet.
     **/
    private ArrayList resistorList = new ArrayList();

    /**
     * List of diodes in subnet.
     **/
    private ArrayList diodeList = new ArrayList();

    /**
     * List of capacitors in subnet.
     **/
    private ArrayList capacitorList = new ArrayList();

    /**
     * List of transistors in subnet.
     **/
    private ArrayList transistorList = new ArrayList();

    /**
     * Name munging state
     **/
    boolean encounteredBaseName;
    boolean makeBaseNumberOne;

    /** 
     * Constructor.  Requires any node within the subnet
     * as the initial base node.
     **/
    TestSubnet(HierName subnetName, String cellName) { 
        name = subnetName;
        cell = cellName;
        //cell = new AspiceCell(cellName);
    }

    /**
     * Initialize the CastTestEnvironment file object.
     **/
    public void setCastTestEnvironment(final CastTestEnvironment env) {
        castEnv = new CastTestEnvironment(env);
    }

    /** 
     * Returns the AspiceCell representing the circuit to be
     * simulated.
    public AspiceCell getAspiceCell() {
        return cell;
    }
     **/

    /** Call this when a cycle is encountered in the subnet.  **/
    void encounteredCycle() { has_cycles = true; }

    /** Adds a node to the resistive subnet **/
    void addSubnetNode(AbstractNode node) {
        /*subnetNodes.add(node);*/
        //
        // Munge the node's name such that it is guaranteed that there
        // will exist some node in the subnet with the resistive
        // subnet base name.
        //
        if (canonicalName==null || node.name.compareTo(canonicalName) < 0)
            canonicalName = node.name;
        // node.name = subnetNodeNameMunge(node.name);
    }
    
    /**
     * Munges the subnet node names such that it is guaranteed that
     * a node name of the resistive subnet base name (no ":X" suffix)
     * exists.  Swaps ":1" with the base name unless the base name
     * is encountered first.
     * <p>
     * Also removes any trailing '!' in the name.
     **/
    private HierName subnetNodeNameMunge(HierName x) {
        HierName y = null;
        if (x.getResistiveSubnetName().equals(name)) {
            if (x.equals(name)) {
                if (makeBaseNumberOne)
                    y = HierName.makeSiblingName(x,x.getSuffixString()+":1");
                else {
                    encounteredBaseName = true;
                    y = x;
                }
            }
            else {
                String suffix = x.getSuffixString();
                int i = suffix.indexOf(':');
                if (i == -1) y = x;
                if (suffix.substring(i+1,suffix.length()).equals("1") &&
                    (makeBaseNumberOne || !encounteredBaseName)) {
                    makeBaseNumberOne = true;
                    y = x.getResistiveSubnetName();
                }
                else y = x;
            }
        }
        else y = x;
        Debug.assertTrue(y!=null);
        if (y.isGlobal()) {
            int i = y.getSuffixString().lastIndexOf('!');
            y = HierName.makeSiblingName(y,y.getSuffixString().substring(0,i));
        }
        return y;
    }

    /** Add a capacitor to the subnet **/
    void addCapacitor(CapDevice cd, AbstractNode remoteNode) {
        HierName remoteSubnetName = remoteNode.name.getResistiveSubnetName();
        // 
        // First tally parasitic cap to the
        // couplingCap map.
        //
        Double cap = (Double) couplingCap.get(remoteSubnetName);
        if (cap != null) 
            cap = new Double(cap.doubleValue() + cd.getValue());
        else
            cap = new Double(cd.getValue());
        couplingCap.put(remoteSubnetName,cap);
        // 
        // Now generate the appropriate capacitor device in
        // the AspiceCell.  If this is a capacitor from one
        // node within the subnet to another, add the exact
        // cap.  Otherwise lump the remote terminal to its
        // subnet name.
        //
        if (remoteNode.name.getResistiveSubnetName().equals(name))
            capacitorList.add(cd.getCapacitor());
        else
            capacitorList.add(cd.getCapacitorLumpedToSubnet(remoteNode));
    }

    /** Add a diode to the subnet **/
    void addDiode(DiodeDevice dd) {
        //
        // Add the Diode to the internal (AspiceCell-style) list
        //
        diodeList.add(dd.getDiode());
        /*cell.addDiode(dd.getDiode());*/
    }

    /** Adds all diodes from the AbstractNode. **/
    void addDiodesFrom(AbstractNode n) {
        Iterator it = n.getDiodeDevices();
        while (it.hasNext())
            diodeList.add(((DiodeDevice)it.next()).getDiode());
    }

    /** Add a resistor to the subnet **/
    void addResistor(ResDevice rd) {
        //
        // Add the Resistor to the internal (AspiceCell-style) list
        //
        resistorList.add(rd.getResistor());
        //cell.addResistor(rd.getResistor());
    }

    /**
     * Abstract test source helper class.  Represents the .cast 
     * code necessary to produce a particular input stimulus.
     **/
    abstract class TestSource {
        protected HierName n;
        TestSource(final HierName node) { n=node; }
        abstract void addToCastEnv();
        HierName getSubnetName() { return n; }
    }

    /**
     * A constant-GND test source.
     **/
    class GNDTestSource extends TestSource {
        GNDTestSource(HierName node) { super(node); }
        void addToCastEnv() {
            //castEnv.addInstantiation("node "+n.getAsString('.')+"=GND;");
            castEnv.addAspLine("wire("+n.getAsString('.')+",GND);");
        }
    }

    /**
     * TestSource that we can rely to be a constant Vdd.
     **/
    class VddTestSource extends TestSource {
        VddTestSource(HierName node) { super(node); }
        void addToCastEnv() {
            //castEnv.addInstantiation("node "+n.getAsString('.')+"=Vdd;");
            castEnv.addAspLine("wire("+n.getAsString('.')+",Vdd);");
        }
    }

    /**
     * TestSource for a "neutral" source, i.e. one that we know
     * nothing about its value.  Set it to a fixed Vdd/2.
     **/
    class NeutralTestSource extends TestSource {
        NeutralTestSource(HierName node) { super(node); }
        void addToCastEnv() {
            String s = n.getAsString('.');
            castEnv.addAspLine("source{ (true/2-"+s+")*1e10 -> "+s+" }");
            castEnv.addAspLine("cap (GND,"+s+")(1);");
        }
    }

    /**
     * Undriven source (i.e. it's not a source at all, but an
     * output or feedback node in the environment.
     **/
    class UndrivenTestSource extends TestSource {
        UndrivenTestSource(HierName node) { super(node); }
        void addToCastEnv() {
            //castEnv.addDeclaration("node "+n.getAsString('.')+"; "+
            //                       "// (undriven)");
        }
    }

    /**
     * A driven source.  Specified as a digital sequence of values
     * separated by some fixed interval.  Generated with a cast
     * source_node cell from "env.cast".
     **/
    class DrivenTestSource extends TestSource {
        private ArrayList points = new ArrayList();
        private int period = 100;
        DrivenTestSource(HierName node) { super(node); }
        void setPoint(int i, boolean value) {
            if (i>points.size()) {
                System.err.println("setPoint! node="+n.getAsString('.'));
                i = points.size();
            }
            else if (i==points.size()) {
                points.add(i,new Boolean(value));
            }
            else {
                points.set(i,new Boolean(value));
            }
        }
        void setPeriod(int p) { period = p; }
        int getPeriod() { return period; }
        void setLength(int length) {
            if (length > points.size()) {
                Boolean last = (Boolean) points.get(points.size()-1);
                for (int i=points.size(); i < length; i++)
                    points.add(i,last);
            }
            else {
                for (int i=points.size()-1; i >= length; i--)
                    points.remove(i);
            }
        }
        int getLength() { return points.size(); }
        void addToCastEnv() {
            castEnv.addImport("env.cast");
            castEnv.addDeclaration("node \""+n.getAsString('.')+"\";");
            StringBuffer sb = new StringBuffer();
            if (points.size() > 2) {
                sb.append("source_node("+period+","+points.size()+",{");
                for (int i=0; i<points.size(); i++) {
                    sb.append(((Boolean)points.get(i)).booleanValue() ? "1":"0");
                    if (i+1!=points.size()) sb.append(",");
                }
            }
            else {
                // Stupid source_node(x,2,{..}) bug workaround
                sb.append("source_node("+period/2+","+points.size()*2+",{");
                for (int i=0; i<points.size(); i++) {
                    sb.append(((Boolean)points.get(i)).booleanValue()?"1":"0");
                    sb.append(",");
                    sb.append(((Boolean)points.get(i)).booleanValue()?"1":"0");
                    if (i+1!=points.size()) sb.append(",");
                }
            }
            sb.append("}) _(\""+n.getAsString('.')+"\");");
            castEnv.addInstantiation(sb.toString());
        }
    }


    /** 
     * Add a transistor attached at its gate to the subnet.
     * Note: this assumes that any transistor is connected to the
     * resistive subnet at a single terminal.  Does not support
     * diode-connected transistors.
     **/
    void addGateMos(MosDevice md) {
        //
        // Add the transistor
        //
        /*cell.addTransistor(md.getTransistor());*/
        transistorList.add(md.getTransistor());
        gateNodes.add(md.gateNode());
        HierName sn = md.sourceNode().name.getResistiveSubnetName();
        HierName dn = md.drainNode().name.getResistiveSubnetName();
        internalDriverNodes.add(sn);
        internalDriverNodes.add(dn);
        //
        // Determine how to drive the driven subnets in the test
        // environment.
        //
        if (md.sourceNode().isGND() || md.sourceNode().isVdd()) {
            drivenSubnets.put(sn, new UndrivenTestSource(sn));
        }
        else if (md.drainNode().isGND() || md.drainNode().isVdd()) {
            drivenSubnets.put(dn, new UndrivenTestSource(dn));
        }
        else if (md.sourceNode().isInternal() && !md.drainNode().isInternal()) {
            drivenSubnets.put(dn, new UndrivenTestSource(dn));
            if (md.getType() == DeviceTypes.N_TYPE)
                drivenSubnets.put(sn, new GNDTestSource(sn));
            else
                drivenSubnets.put(sn, new VddTestSource(sn));
        }
        else if (!md.sourceNode().isInternal() && md.drainNode().isInternal()) {
            drivenSubnets.put(sn, new UndrivenTestSource(sn));
            if (md.getType() == DeviceTypes.N_TYPE)
                drivenSubnets.put(dn, new GNDTestSource(dn));
            else
                drivenSubnets.put(dn, new VddTestSource(dn));
        }
        else {
            if (drivenSubnets.get(sn) == null)
                drivenSubnets.put(sn, new NeutralTestSource(sn));
            if (drivenSubnets.get(dn) == null)
                drivenSubnets.put(dn, new NeutralTestSource(dn));
        }
    }

    /**
     * Adds the specified node to the subnet's list of driver nodes.
     * Once all driver nodes have been identified, 
     * {@link #generateInputSources} can be called to determine all
     * pull-up/pull-down paths and add all driver transistors to the
     * test circuit.
     **/
    void addDriverNode(AbstractNode n) {
        driverNodes.add(n);
    }

    /**
     * Adds the specified Mosfet transistor to the set of transistors
     * in this subnet's driver networks.
     **/
    void addDriverMos(MosDevice md) {
        if (driverMosDevices.add(md)) {
            //
            // This MosDevice wasn't already added, so add it to the
            // AspiceCell.
            //
            transistorList.add(md.getTransistorWithGateLumped());
            HierName sn = md.sourceNode().name.getResistiveSubnetName();
            HierName dn = md.drainNode().name.getResistiveSubnetName();
            if (!sn.equals(name)) 
                if (internalDriverNodes.add(sn)) addDiodesFrom(md.sourceNode());
            if (!dn.equals(name))
                if (internalDriverNodes.add(dn)) addDiodesFrom(md.drainNode());
            /*cell.addTransistor(md.getTransistor());*/
        }
    }

    boolean isDriverNode(AbstractNode n) {
        return driverNodes.contains(n);
    }

    boolean isInternalDriverNode(AbstractNode n) {
        return internalDriverNodes.contains(n);
    }

    public void findConjunctiveDriverPaths() {
        Iterator it = driverNodes.iterator();
        while (it.hasNext()) {
            AbstractNode n = (AbstractNode) it.next();
            ConjunctivePath p = new ConjunctivePath();
            boolean reached_supply = n.buildConjunctivePaths(p,this);
            //Debug.assertTrue(reached_supply);
        }
        if (numFeedbackPullUps == conjunctivePullUpPaths.size() &&
            numFeedbackPullDowns == conjunctivePullDownPaths.size() &&
            feedbackSubnets.size() == 1) {
            // 
            // Case documented below, in addConjunctivePath
            //
            it = conjunctivePullUpPaths.iterator();
            while (it.hasNext()) {
                ConjunctivePath p = (ConjunctivePath) it.next();
                p.clearFeedback();
                ArrayList terms = p.getTerms();
                for (int i=0; i<terms.size()-1; i++) {
                    ConjunctivePath.Term t = (ConjunctivePath.Term)terms.get(i);
                    feedbackSubnets.remove(t.node);
                    inputSubnets.add(t.node);
                }
            }
            it = conjunctivePullDownPaths.iterator();
            while (it.hasNext()) {
                ConjunctivePath p = (ConjunctivePath) it.next();
                p.clearFeedback();
                ArrayList terms = p.getTerms();
                for (int i=0; i<terms.size()-1; i++) {
                    ConjunctivePath.Term t = (ConjunctivePath.Term)terms.get(i);
                    feedbackSubnets.remove(t.node);
                    inputSubnets.add(t.node);
                }
            }
        }
    }

    /** 
     * Used by AbstractNode.buildConjunctivePaths() to add new
     * ConjunctivePaths (formed by branches in the logic) to
     **/
    void addConjunctivePath(ConjunctivePath p) {
        // Make sure p has at least one transistor.
        // Debug.assertTrue(p.getNumTerms() > 1);
        //if (p.getNumTerms
        if (p.drivesUp())
            conjunctivePullUpPaths.add(p);
        else
            conjunctivePullDownPaths.add(p);
        if (CONJUNCTIVE_PATH_DEBUG) {
            System.err.print("addConjunctivePath: adding ");
            p.printToStream(System.err);
        }
        //
        // Look for subnets that appear both in ConjunctivePull{Up,Down}Paths
        // terms and in the drivenSubnets set.  Remove such nets
        // from drivenSubnets and set the feedback flag of this
        // conjunctive path.  Note: In rare cases, this condition will 
        // hold when this subnets staticizes an input to the ConjunctivePath 
        // through one of the drivenSubnets.  To catch this, a check for 
        // single feedback conjunctive pull-up/down paths is done in 
        // findConjunctiveDriverPaths, and if no real driver paths remain,
        // the feedback paths are set to be non-feedback.
        //
        ArrayList terms = p.getTerms();
        for (int i=0; i<terms.size()-1; i++) {
            ConjunctivePath.Term t = (ConjunctivePath.Term) terms.get(i);
            if (drivenSubnets.remove(t.node) != null) {
                feedbackSubnets.add(t.node);
                p.setFeedback();
                if (p.drivesUp()) numFeedbackPullUps++;
                else numFeedbackPullDowns++;
            }
            else if (feedbackSubnets.contains(t.node)) {
                p.setFeedback();
                if (p.drivesUp()) numFeedbackPullUps++;
                else numFeedbackPullDowns++;
            }
            else inputSubnets.add(t.node);
        }
    }

    /**
     * Returns iterator over the subnet's conjunctive pull-up paths.
     **/
    public Iterator getConjunctivePullUpPaths() {
        return conjunctivePullUpPaths.iterator();
    }

    /**
     * Returns iterator over the subnet's conjunctive pull-down paths.
     **/
    public Iterator getConjunctivePullDownPaths() {
        return conjunctivePullDownPaths.iterator();
    }

    /**
     * Returns iterator over the subnet's feedback nets (as HierNames).
     **/
    public Iterator getFeedbackSubnets() {
        return feedbackSubnets.iterator();
    }

    /**
     * Temporary state for the conjunctive path feedback-skipping
     * iterator utility methods.
     **/
    private ConjunctivePath pTmp;

    /**
     * Determines if the given conjunctivePull{Up,Down}Paths iterator
     * has another non-feedback path.
     **/
    private boolean hasNextNonFeedbackPath(Iterator cpit) {
        boolean nonFb = false;
        while (!nonFb && cpit.hasNext()) {
            pTmp = (ConjunctivePath) cpit.next();
            nonFb = !pTmp.isFeedbackPath();
        }
        return nonFb;
    }

    /**
     * Returns the next conjunctivePull{Up,Down}Path from the given
     * iterator.  hasNextNonFeedbackPath() must be called before this.
     **/
    private ConjunctivePath nextNonFeedbackPath() {
        return pTmp;
    }

    /**
     * Generates the .cast file input stimuli for the test subnet.
     **/
    private void generateInputSources() {
        //
        // Construct some useful temporary data structures:
        // a list of non-feedback pull-up/down paths,
        // a mappings from input subnets to pull-up/down terms.
        //
        int numPullUps = conjunctivePullUpPaths.size()-numFeedbackPullUps;
        int numPullDns = conjunctivePullDownPaths.size()-numFeedbackPullDowns;
        ConjunctivePath[] pullUps = null;
        ConjunctivePath[] pullDns = null;
        HashSet pullUpTerms = new HashSet();
        HashSet pullDnTerms = new HashSet();
        if (numPullUps > 0) {
            pullUps = new ConjunctivePath[numPullUps];
            Iterator it = conjunctivePullUpPaths.iterator();
            for (int i=0; i < numPullUps; i++) {
                ConjunctivePath p = null;
                do {
                    Debug.assertTrue(it.hasNext());
                    p = (ConjunctivePath) it.next();
                } while (p.isFeedbackPath());
                pullUps[i] = p;
                pullUpTerms.addAll(p.getTerms());
            }
        }
        if (numPullDns > 0) {
            pullDns = new ConjunctivePath[numPullDns];
            Iterator it = conjunctivePullDownPaths.iterator();
            for (int i=0; i < numPullDns; i++) {
                ConjunctivePath p = null;
                do {
                    Debug.assertTrue(it.hasNext());
                    p = (ConjunctivePath) it.next();
                } while (p.isFeedbackPath());
                pullDns[i] = p;
                pullDnTerms.addAll(p.getTerms());
            }
        }

        //
        // Generate input sequences
        //
        HashMap sourceMap = new HashMap();
        Iterator it = inputSubnets.iterator();
        while (it.hasNext()) {
            HierName n = (HierName) it.next();
            sourceMap.put(n,new DrivenTestSource(n));
        }
        int length;
        int iup = -1, idn = -1;
        ConjunctivePath pup = null, pdn = null;
        if (numPullDns > 0 && numPullUps > 0) {
            idn = 0; pup = pullUps[0];
            iup = 0; pdn = pullDns[0];
            length = numPullUps > numPullDns ? numPullUps : numPullDns;
        }
        else {
            // No pull-up or pull-down terms! Drive the TestSubnet directly.
            // (Hope that a 'name' node within the subnet actually exists.)
            sourceMap.clear();
            sourceMap.put(name,new DrivenTestSource(name));
            pdn = new ConjunctivePath();
            pdn.addTerm(name,DeviceTypes.P_TYPE);
            pdn.addTerm(GND,ConjunctivePath.SUPPLY);
            pup = new ConjunctivePath();
            pup.addTerm(name,DeviceTypes.N_TYPE);
            pup.addTerm(Vdd,ConjunctivePath.SUPPLY);
            length = 1;
        }
        HashSet enabledInputSet = new HashSet();
        for (int i=0; i < length; i++) {
            ConjunctivePath p = null;
            //
            // Handle up transition
            //
            enabledInputSet.clear();
            if (iup != -1 && iup < numPullUps) p = pullUps[iup++];
            else p = pup;
            for (int j=0; j < p.getNumTerms()-1; j++) {
                ConjunctivePath.Term t =
                    (ConjunctivePath.Term) p.getTerms().get(j);
                DrivenTestSource src = (DrivenTestSource) sourceMap.get(t.node);
                src.setPoint(2*i,t.type==DeviceTypes.N_TYPE);
                enabledInputSet.add(t.node);
            }
            //
            // Disable all non-relevant pull-up & pull-down input nodes
            //
            it = pullUpTerms.iterator();
            for (int j=0; j<2; j++) {
                while (it.hasNext()) {
                    ConjunctivePath.Term t = (ConjunctivePath.Term) it.next();
                    if (!enabledInputSet.contains(t.node)) {
                        DrivenTestSource src = 
                            (DrivenTestSource) sourceMap.get(t.node);
                        if (src != null) 
                            src.setPoint(2*i,t.type==DeviceTypes.P_TYPE);
                    }
                }
                it = pullDnTerms.iterator();
            }

            //
            // Handle down transition
            //
            enabledInputSet.clear();
            if (idn != -1 && idn < numPullDns) p = pullDns[idn++];
            else p = pdn;
            for (int j=0; j < p.getNumTerms()-1; j++) {
                ConjunctivePath.Term t =
                    (ConjunctivePath.Term) p.getTerms().get(j);
                DrivenTestSource src = (DrivenTestSource) sourceMap.get(t.node);
                src.setPoint(2*i+1,t.type==DeviceTypes.N_TYPE);
                enabledInputSet.add(t.node);
            }
            //
            // Disable all non-relevant pull-down & pull-up input nodes
            //
            it = pullDnTerms.iterator();
            for (int j=0; j<2; j++) {
                while (it.hasNext()) {
                    ConjunctivePath.Term t = (ConjunctivePath.Term) it.next();
                    if (!enabledInputSet.contains(t.node)) {
                        DrivenTestSource src = 
                            (DrivenTestSource) sourceMap.get(t.node);
                        if (src != null)
                            src.setPoint(2*i+1,t.type==DeviceTypes.P_TYPE);
                    }
                }
                it = pullUpTerms.iterator();
            }
        }
        // 
        // Now send all test sources to the CastTestEnvironment
        //
        it = sourceMap.values().iterator();
        while (it.hasNext()) {
            DrivenTestSource src = (DrivenTestSource) it.next();
            // 
            // Check if any input sources were missed.
            // (This can happen in extremely rare cases when
            // a conjunctive path is incorrectly identified as
            // a feedback path, e.g. A2S_FAST_1of2._R.0,1)
            //
            if (src.getLength() == 0) 
                for (int i=0; i<2*length; i++) src.setPoint(i,false);
            src.setPeriod(DSIM_TIME_TAU_MAX);
            src.addToCastEnv();
        }
        //
        // Set the simulation run time to match the number of unique input
        // vectors.
        //
        float timeMax = DSIM_TIME_TAU_MAX * DTU * (2 * length + 1) + T_RESET;
        String timeMaxStr = NumberFormatter.format(timeMax,4);
        castEnv.addAspLine(" .timemax="+timeMaxStr);
    }

    /**
     * Writes the cast test environment to the specified file.
     **/
    public void writeCastEnvironmentFile(String envFile) 
        throws IOException
    {
        // 
        // Write out the identified conjunctive paths
        // to the cast file, as comments to help with
        // debugging.
        //
        castEnv.addDeclaration("// Pull-up conjunctive paths:");
        Iterator it = conjunctivePullUpPaths.iterator();
        while (it.hasNext()) {
            ConjunctivePath cp = (ConjunctivePath) it.next();
            castEnv.addDeclaration("//  "+cp.getAsString());
        }
        castEnv.addDeclaration("// Pull-down conjunctive paths:");
        it = conjunctivePullDownPaths.iterator();
        while (it.hasNext()) {
            ConjunctivePath cp = (ConjunctivePath) it.next();
            castEnv.addDeclaration("//  "+cp.getAsString());
        }
        // 
        // Generate all input prs sources, keeping track
        // of which nodes have been sourced.
        //
        generateInputSources();
        //
        // Generate all output driver network internal node sources.
        //
        it = drivenSubnets.values().iterator();
        while (it.hasNext()) {
            TestSource ts = (TestSource) it.next();
            ts.addToCastEnv();
        }
        // 
        // Add coupling cap nodes (those not already covered by
        // input sources), shorted to GND.  Don't modify _SReset,
        // _PReset, Vdd, or any of the subnet nodes.
        //
        it = couplingCap.keySet().iterator();
        while (it.hasNext()) {
            HierName remoteNet = (HierName) it.next();
            String s = remoteNet.getAsString('.');
            int i = s.indexOf('!');
            if (i != -1) s = s.substring(0,i);
            if (!inputSubnets.contains(remoteNet) && 
                !feedbackSubnets.contains(remoteNet) &&
                !internalDriverNodes.contains(remoteNet) &&
                !remoteNet.equals(name) && !remoteNet.isVdd() &&
                !s.equals("_PReset") && !s.equals("_SReset"))
                castEnv.addAspLine("wire("+s+","+"GND);");
        }
        // 
        // Instantiate the subnet
        //
        it = capacitorList.iterator();
        while (it.hasNext()) {
            Capacitor c = (Capacitor) it.next();
            castEnv.addAspLine(c.getAspiceString());
        }
        it = resistorList.iterator();
        while (it.hasNext()) {
            Resistor r = (Resistor) it.next();
            castEnv.addAspLine(r.getAspiceString());
        }
        it = transistorList.iterator();
        while (it.hasNext()) {
            Transistor t = (Transistor) it.next();
            castEnv.addAspLine(t.getAspiceString());
        }
        it = diodeList.iterator();
        while (it.hasNext()) {
            Diode d = (Diode) it.next();
            castEnv.addAspLine(d.getAspiceString());
        }
        //
        // Make sure the resistive subnet name exists in the aspice
        // namespace.
        //
        if (!canonicalName.getAsString('.').equals(name))
            castEnv.addAspLine("wire("+canonicalName.getAsString('.')+","+
                               name.getAsString('.')+");");
        // 
        // Print the CastTestEnvironment to the PrintStream.
        //
        PrintStream ps = new PrintStream(new BufferedOutputStream(
                            new FileOutputStream(envFile)));
        castEnv.printToStream(ps);
        ps.close();
    }

    /** 
     * Returns true if one or more cycles have been detected in the
     * subnet's resistive graph.
     **/
    public boolean hasCycles() { return has_cycles; }

    /** Returns an Iterator over the subnet's driver AbstractNodes.  **/
    public Iterator getDriverNodes() { return driverNodes.iterator(); }

    /** Returns an Iterator over the subnet's gate AbstractNodes.  **/
    public Iterator getGateNodes() { return gateNodes.iterator(); }

    /** 
     * Returns an Iterator over all the subnet HierNames 
     * driven by this subnet. 
     **/
    public Iterator getDrivenSubnets() { 
        return drivenSubnets.keySet().iterator(); 
    }

    /**
     * Returns an iterator over all TestSources controlling 
     * the driven subnets of this TestSubnet.
     **/
    public Iterator getDrivenSubnetSources() { 
        return drivenSubnets.values().iterator(); 
    }

    /** 
     * Prints total parasitic capacitance lumped to each remote
     * subnet.
     **/
    public void printParasiticCapToStream(final OutputStream s) {
        PrintWriter pw = new PrintWriter(s);
        pw.println("#");
        pw.println("# Nodes coupling to "+cell+":");
        pw.println("#");
        Iterator it = couplingCap.keySet().iterator();
        while (it.hasNext()) {
            HierName remoteNet = (HierName) it.next();
            Double cap = (Double) couplingCap.get(remoteNet);
            pw.println(remoteNet.getAsString('.') + ": " + cap);
        }
        pw.close();
    }
}

