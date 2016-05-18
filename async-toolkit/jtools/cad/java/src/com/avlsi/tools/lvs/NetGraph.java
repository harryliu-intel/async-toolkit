/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.lvs;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.ArrayList;
import java.util.List;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Set;
import java.util.SortedSet;
import java.util.stream.Collectors;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.NullEnvironment;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.ExclusiveNodeSet;
import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.NetlistAdapter;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.fast.CellType;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.aspice.Transistor;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.cdl.parser.CDLInterfaceSimplifier;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLFactoryAdaptor;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.impl.VisitorImpl;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.container.MultiSet;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.Partition;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.prs.UnimplementableProductionRuleException;
import com.avlsi.util.bool.*;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.cell.CellUtils;
import com.avlsi.fast.CastDesign;

/**
 * Data structures and algorithms for a searchable transistor graph.
 *
 * Used for lvs netlist vs prs matching.
 * Used to compute minimal gate networks from prs.
 * Used to add staticizers and match gates.
 * Used to make simulatable prs given a netlist.
 *
 * @author Andrew Lines
 * @version $Revision$ $Date$
 *
 **/
public final class NetGraph {

    /** Enable debugging output. */
    private static final boolean verbose = false;

    /** Represent node name aliases.  Non-aliased nodes may be omitted. */
    public AliasedSet namespace;

    /** Excllo and Exclhi declarations. */
    ExclusiveNodeSets exclusives;

    /** List of LVSProblems for normal error reporting. */
    List problems;

    /** All NetNodes in this NetGraph, sorted by name. */
    MultiSet<NetNode> nodes;

    /** the set of nodes we don't want staticizers on. */
    public final Set nostaticizers;

    /** Power supply NetNode. */
    public NetNode Vdd,GND;

    /** Unique number for next internally generated NetNode name. */
    private int nextNodeNum;

    /** is it safe to insert production rules as DNFs pre-merged into a tree? */
    private boolean treeUnsafe;

    /** Only terminate paths at power supply, not other outputs? */
    private boolean pathsToRailOnly = false;

    /** Type name of the gate, if this a netgraph for a gate in the library */
    public String gateType;

    /** Have foldingFactors been calculated for all edges? **/
    private boolean doneFoldingFactors = false;

    /** Default width and length for transistors **/
    private final double defaultWidth, defaultLength; 

    /** For cdl2prs, assume inverters overpower other paths **/
    private boolean assumeStrongInverters = false;

    /** Instance of a library gate, used by NetNode. */
    public static class GateInstance {

        /** Gate type name. */
        final String type;

        /**
         * Mapping of gate dummy names. The keys are the parameters of the
         * gate, and the values are the names of nodes used to instantiate the
         * gate.
         **/
        TreeMap /*<HierName,HierName>*/ map;

        /** The NetGraph of this gate */
        private final NetGraph gateGraph;

        /** This gate is used to precharge an internal node */
        private final boolean precharge;

        /** Constructor. */
        GateInstance(TreeMap map, NetGraph gateGraph) {
            this(gateGraph.gateType, map, gateGraph);
        }

        GateInstance(String type, TreeMap map, NetGraph gateGraph) {
            this(type, map, gateGraph, false);
        }

        GateInstance(String type, TreeMap map, NetGraph gateGraph,
                     boolean precharge) {
            this.type = type;
            this.map = new TreeMap(map);
            this.gateGraph = gateGraph;
            this.precharge = precharge;
        }

        /**
         * Returns an unmodifiable map from formal parameters to actual
         * parameters.
         **/
        public Map /*<HierName,HierName>*/ getUnmodifiableMap() {
            return Collections.unmodifiableMap(map);
        }

        /** Returns the gate type. */
        public String getType() {
            return type;
        }

        /** Returns the NetGraph of this gate. */
        public NetGraph getNetGraph() {
            return gateGraph;
        }

        /**
         * Finds the port node name in the gate for the specified
         * name in the parent.
         **/
        public /*@ non_null @*/ HierName getPortName(
                                                     final /*@ non_null @*/ HierName parentNodeName) {
            // the gate map is a map from port name (in the gate) to name
            // in the parent.  
            HierName found = null;
            for (Iterator i = map.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry/*<HierName,HierName>*/ e =
                    (Map.Entry) i.next();
                HierName parentName = (HierName) e.getValue();
                if (parentName.equals(parentNodeName)) {
                    if (found == null)
                        found = (HierName) e.getKey();
                    else {
                        // Error, same node passed to multiple ports of same
                        // cell. We'll just assert for now.
                        // TODO: Better error handling.  This could happen
                        // because of bad cast.  (Same node passed to
                        // multiple ports).
                        throw new AssertionError();
                    }
                }
            }

            assert found != null;
            return found;
        }

        /** Debugging string. */
        public String toString() {
            return type + " " + map + "\n";
        }

        /** Gate string in skill compatible format. */
        public String skillString(int num) {
            String s = "(" + type + " \"X" + num + "\" (list";
            Iterator t = map.keySet().iterator();
            while (t.hasNext()) {
                HierName from = (HierName) t.next();
                HierName to = (HierName) map.get(from);
                s += " \"" +  from + "\":\"" + to + "\"";
            }
            s += "))\n";
            return s;
        }

        /** Return if this gate is used to precharge an internal node. */
        public boolean isPrechargePrimitive() {
            return precharge;
        }
    }

    /** A node in a NetGraph. */
    public final class NetNode implements Comparable {
        /** Name of the node. */
        public final HierName name;

        /** Sorted set of all edges which have this node as a source or drain. */
        public MultiSet edges;

        /** Saved set of edges for undoing changes to NetGraph. */
        MultiSet oldEdges;

        /** Paths from this node to another output/global node. */
        MultiSet paths;

        /** True if the output of a half-operator. */
        boolean output;
        
        /** True if name is explicitly named, not auto-generated (a number). */
        boolean named;
        
        /** True if in port list of cell. */
        boolean is_port;
        
        /** True if gate of a transistor. */
        boolean is_gate;

        /** True if S/D of N transistor. */
        boolean hasN;

        /** True if S/D of P transistor. */
        boolean hasP;
 
        /** Used to avoid cyclic paths in findPaths. */
        boolean visited;

        /** True if this node has the passgate directive. */
        boolean passgate;

        /** True if this node also gates a non-feedback path. */
        boolean nonFeedback;

        /**
         * Points to a NetNode which is the input of an inverter
         * driving this node, or null if none.
         */
        NetNode inverseOf = null;

        /**
         * Points to a NetNode which is a drives this node with an
         * inverter subgraph, but may have interfering fanins.  Used
         * for CDL2Cast.
         */
        NetNode interferingInverseOf = null;

        /**
         * Points to a NetNode which is the inverse of this node, or
         * null if none.  Note that there may be multiple inverses,
         * and this only picks one at random!  This is suitable only
         * for attaching a WEAK_INV feedback.
         */
        NetNode inverse = null;

        /** 
         * Points to a NetNode which is the output of an inverter
         * gated by this node, where that output feeds back into this
         * node for a staticizer, or null if none.
         */
        NetNode feedbackFrom = null;

        /** Which library gate this node belongs to, if any. */
        GateInstance gate = null, staticizer = null;

        /**
         * fold numbers for the pull-up and pull-down networks.
         * Symmetrized pull-ups and pull-downs use fold for the normal
         * direction and fold+1 for the reverse.
         **/
        int up_fold = 0, down_fold = 0;

        /** 
         * Average drive strength (transistor squares [w/l]) of paths driving
         * this node down ([0]) and up ([1]).
         **/
        double[] aveDriveStrength = null;

        /** Number of unique edges (NFET/PFET) that this nodes drive **/
        int[] edgeFanout = { 0, 0 };

        /** Total gate area that this node drives (NFET/PFET) **/
        double[] gateLoad = { 0.0, 0.0 };

        /** Is this a power supply? **/
        boolean isPower  = false;
        boolean isGround = false;

        /** Construct a NetNode with a given HierName, or null for unnamed. */
        NetNode (HierName name) {
            if (name == null)
                do {
                    name = HierName.makeHierName(nextNodeNum + "");
                    nextNodeNum++;
                } while (findNetNode(name) != null);
            this.name = name;
            output    = false;
            is_port   = false;
            is_gate   = false;
            hasN      = false;
            hasP      = false; 
            passgate  = false;
            nonFeedback = false;
            visited   = false;
            named     = !name.isGenerated() && !name.isNumeric();
            edges     = new MultiSet();
            paths     = new MultiSet();
        }

        /** Construct a NetNode with a unique auto-generated name. */
        NetNode () {
            this(null);
        }

        /** Add a NetEdge to this NetNode's set of edges. */
        void addEdge (final NetEdge edge) {
            this.edges.add(edge);
        }

        /** Return all paths found for this NetNode. */
        public Collection getPaths () {
            return (Collection) paths;
        }

        /** Return paths that match feedback */
        public ArrayList getPaths (boolean feedback) {
            final ArrayList a = new ArrayList();
            for (Iterator t = paths.iterator(); t.hasNext(); ) {
                NetPath path = (NetPath) t.next();
                if (path.feedback==feedback) a.add(path);
            }
            return a;
        }

        /** Return all non-feedback paths found for this NetNode. */
        public ArrayList getLogicPaths () {
            return getPaths(false);
        }

        /** Return all feedback paths found for this NetNode. */
        public ArrayList getFeedbackPaths () {
            return getPaths(true);
        }

        /** Return all feedback edges driving this node. */
        public Collection getFeedbackEdges() {
            MultiSet uniqueEdges = new MultiSet();
            for (Iterator t=paths.iterator(); t.hasNext(); ) {
                NetPath path = (NetPath) t.next();
                if (!path.feedback) continue;
                for (Iterator s=path.edges.iterator(); s.hasNext(); ) {
                    NetEdge edge = (NetEdge) s.next();
                    uniqueEdges.addIfUnique(edge);
                }
            }
            return (Collection) uniqueEdges;
        }

        /** 
         * Returns all NetNodes that drive this node through non-feedback paths 
         **/
        public Set getLogicFaninNodes () {
            HashSet faninNodes = new HashSet();
            ArrayList paths = getLogicPaths();
            for (Iterator t=paths.iterator(); t.hasNext();)
                faninNodes.addAll(((NetPath)t.next()).getGateNodes());
            return faninNodes;
        }

        /** Check if this NetNode is Vdd. */
        public boolean isVdd () {
            return isPower;
        }

        /** Check if this NetNode is GND. */
        public boolean isGND () {
            return isGround;
        }

        /** Check if this NetNode is a power rail. */
        public boolean isRail() {
            return isVdd() || isGND();
        }

        /** Does this NetNode output gate a weak feedback inverter? */
        public boolean gatesWeakInverter() {
            return output &&                    // is an operator output
                inverseOf != null &&            // is an inverse of another node
                inverseOf.feedbackFrom == this; // drives feedback to that node
        }

        /**
         * Is this NetNode the output of a small inverter used only in
         * a staticizer?  If so, LVS won't check it against prs that
         * won't be there.  Usage in Jauto is extremely confusing...
         */
        public boolean isStaticizerInverter() { 
            return gatesWeakInverter() && // must gate a weak inverter
                !is_port &&               // can't be a port
                !nonFeedback;             // can't gates something besides feedback
        }
        
        /** Does this NetNode output gate a small inverter of a staticizer? */
        public boolean gatesSmallInverter() {
            return (feedbackFrom!=null) &&           // must get feedback
                feedbackFrom.isStaticizerInverter(); // from a staticizer inv
        }

        /** Does this node have a weak staticizer pulling it up? **/
        public boolean hasWeakFeedBack(int type) {
            for (Iterator j=getFeedbackPaths().iterator(); j.hasNext(); ) {
                NetPath path = (NetPath) j.next();
                if (path.type!=type) continue;
                if (path.isWeakFeedBack()) return true;
            }
            return false;
        }

        /**
         * Is the NetNode connected to a staticizer or weak inverter?
         * Jauto will attach extra load if it is.
         */
        public boolean isStaticized(){
            return (staticizer!=null);
        }

        /** Is this node an output node. */
        public boolean isOutput() {
            return output;
        }

        /** Check if the node is explicitly named. */
        public boolean isNamed() {
            return named;
        }

        /** Check if the node is in port list. */
        public boolean isPort() {
            return is_port;
        }

        /** Check if the node gates a transistor. */
        public boolean isGate() {
            return is_gate;
        }

        /** Would this node need a contact in the layout? */
        public boolean needsContact() {
            return isRail() || output || (edges.size()!=2);
        }

        /** Returns an iterator for edges of this NetNode. */
        public Iterator getEdges() {
            return edges.iterator();
        }

        /** Returns the name of this NetNode. */
        public HierName getName() {
            return name;
        }

        /** Returns the gate attached to this NetNode, or null if none. */
        public GateInstance getGate() {
            return gate;
        }

        /** Returns the staticizer attached to this NetNode, or null if none. */
        public GateInstance getStaticizer() {
            return staticizer;
        }
        
        /** Set output field of this NetNode. */
        void markOutput() {
            output = !isRail() && (edges.size()>0) && (is_port || is_gate || passgate);
        }
        
        /** Set inverseOf field of this NetNode, set inverse field of fanin node. */
        void findInverter () {
            if (this.isRail()) return;
            NetNode faninN = null;
            NetNode faninP = null;
            boolean interfering = false;
            boolean complex = false;
            final Iterator t = edges.iterator();
            while ( t.hasNext() ) {
                final NetEdge edge = (NetEdge) t.next();

                // found a 1N or 1P edge to appropriate power rail
                if (edge.type==DeviceTypes.P_TYPE && (edge.source.isVdd() || edge.drain.isVdd())) {
                    if (faninP==null) faninP=edge.gate;
                    else if (faninP!=edge.gate) complex=true;
                }
                else if (edge.type==DeviceTypes.N_TYPE && (edge.source.isGND() || edge.drain.isGND())) {
                    if (faninN==null) faninN=edge.gate;
                    else if (faninN!=edge.gate) complex=true;
                } else {
                    interfering = true;
                }
            }
            if (!complex && faninN!=null && faninN==faninP) {
                if (interfering) interferingInverseOf = faninN;
                else {
                    inverseOf = faninN;
                    faninN.inverse = this;
                }
            }
        }

        /**
         * Find an edge from this node that is in the right direction,
         * type, and fold and gated by G, and not connected to an
         * output.  This is used to build a shared transistor tree
         * from a production rule in addConjunctiveTermSharedRecursive.
         **/
        private NetEdge findCompatibleNetEdge(NetNode G, boolean source, int type, int fold) {
            for (Iterator i = getEdges(); i.hasNext(); ) {
                NetEdge e = (NetEdge) i.next();
                if (e.type != type) continue;
                if (e.fold != fold) continue;
                if (source) {
                    if (e.source != this) continue;
                    if (e.drain.output) continue;
                } else {
                    if (e.drain != this) continue;
                    if (e.source.output) continue;
                }
                if (e.gate != G) continue;
                return e;
            }
            return null;
        }

        /** Compare NetNodes by name. */
        public int compareTo (Object b) {
            return this.name.compareTo(((NetNode) b).name);
        }

        /** Debugging string. */
        public String toString () {
            return "NetNode: name=" + name + 
                " edges=" + edges.size() + 
                " visited=" + visited + 
                " output=" + output +
                (inverse!=null ? " inverse=" + inverse.name.toString() : "") +
                (inverseOf!=null ? " inverseOf=" + inverseOf.name.toString() : "") +
                (interferingInverseOf!=null ? " interferingInverseOf=" +
                 interferingInverseOf.name.toString() : "") +
                (feedbackFrom!=null ? " feedbackFrom=" + feedbackFrom.name.toString() : "") +
                " nonFeedback=" + nonFeedback;
        }

        /** Recursively search for paths from current node to rail/output NetNodes. */
        private void findPathsRecurse(MultiSet paths, ArrayList pathedges,
                                      NetNode node, int type) {

            // terminate recursion at a rail or an output node
            if (node.isRail() ||
                (!pathsToRailOnly && pathedges.size()>0 &&
                 node.output && (!node.passgate || node.is_port))) {
                NetPath path = new NetPath(this,node,type,pathedges);
                // check exclusion
                if (pathedges.size()>0 && !path.isExcluded()) paths.add(path);
                return;
            }

            // add a sneak path but continue recursion
            if (pathedges.size()>0 && !pathsToRailOnly && node.hasN && node.hasP) {
                NetPath path = new NetPath(this,node,type,pathedges);
                if (!path.isExcluded()) paths.add(path); // check exclusion
            }

            // recurse through edges
            node.visited=true;
            for (Iterator t = node.edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                if (assumeStrongInverters &&
                    node.interferingInverseOf!=null &&
                    edge.gate!=node.interferingInverseOf) continue;
                if (edge.type!=type) continue; // recurse through desired type only
                if (edge.precharge) continue; // ignore precharge transistors
                ArrayList newedges = new ArrayList(pathedges);
                newedges.add(edge);
                NetNode S=edge.source;
                NetNode D=edge.drain;
                if (!S.visited) findPathsRecurse(paths,newedges,S,type);
                if (!D.visited) findPathsRecurse(paths,newedges,D,type);
            }
            node.visited=false;
        }

        /** 
         * Search for paths from this NetNode to power NetNodes.  Only
         * looks for paths which don't change transistor type, that is
         * a P-path to Vdd and an N-path to GND.  Will also stop
         * searching at any outputs, unless pathsToRailOnly is true.
         */
        public void findPaths() {
            this.paths = findPaths(pathsToRailOnly);
        }

        /** 
         * Search for paths from this NetNode to power NetNodes.  Only
         * looks for paths which don't change transistor type, that is
         * a P-path to Vdd and an N-path to GND.  Will also stop
         * searching at any outputs, unless pathsToRailOnly is true.
         */
        public MultiSet findPaths(final boolean pathsToRailOnly) {
            final boolean saved = NetGraph.this.pathsToRailOnly;
            NetGraph.this.pathsToRailOnly = pathsToRailOnly;
            MultiSet paths = new MultiSet();
            ArrayList pathedges = new ArrayList();
            findPathsRecurse(paths,pathedges,this,DeviceTypes.N_TYPE);
            findPathsRecurse(paths,pathedges,this,DeviceTypes.P_TYPE);
            NetGraph.this.pathsToRailOnly = saved;
            return paths;
        }

        /**
         * Check if more paths have been created since previous
         * findPaths. Doens't worry about duplicate paths. Used to
         * check if a sneak path was introduced by merging
         * transistors.
         */
        boolean hasMorePaths() {
            boolean more = false;
            MultiSet paths = new MultiSet();
            ArrayList pathedges = new ArrayList();
            findPathsRecurse(paths,pathedges,this,DeviceTypes.N_TYPE);
            findPathsRecurse(paths,pathedges,this,DeviceTypes.P_TYPE);
            for (Iterator t = paths.iterator(); t.hasNext(); ) {
                NetPath path = (NetPath) t.next();
                if (this.paths.find(path) == null) more = true;
            }
            if (verbose) System.err.println("hasMorePaths=" + more + "\n" + paths);
            return more;
        }

        /** Check if another output is in a shared network with this node. */
        public boolean isShared() {
            if (isRail()) return false; // don't recurse through a global node
            boolean shared = false;
            // recurse through edges
            visited = true;
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                NetNode S = edge.source;
                NetNode D = edge.drain;
                if (!S.visited) shared = shared || S.output || S.isShared();
                if (!D.visited) shared = shared || D.output || D.isShared();
            }
            visited = false;
            return shared;
        }

        /** Recursively delete all edges reachable from this node. */
        public void deleteEdges() {
            if (isRail()) return; // don't recurse through a global node
            // recurse through edges
            visited = true;
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                NetNode S = edge.source;
                NetNode D = edge.drain;
                if (!S.visited) {S.edges.remove(edge); S.deleteEdges();}
                if (!D.visited) {D.edges.remove(edge); D.deleteEdges();}
            }
            // delete edges and paths but leave nodes
            edges = new MultiSet();
            paths = new MultiSet();
            visited = false;
        }

        /** Check if node is output of combinational logic. */
        public boolean isCombinational() {
            if (!output) return false;

            // get logic from paths
            MultiSet netUp  = new MultiSet();
            MultiSet netDn  = new MultiSet();
            MultiSet holdUp = new MultiSet();
            MultiSet holdDn = new MultiSet();
            Dnf.fromNetNode(this,netUp,netDn,holdUp,holdDn);

            // abort if feedback paths exist
            if ((holdUp.size()>0)||(holdDn.size()>0)) return false;

            // compare if dual pullup equals pulldn in canonical form
            MultiSet dualUp = Dnf.dual(netUp);
            dualUp = Dnf.getCanonicalForm(dualUp,ProductionRule.DOWN,exclusives);
            netDn  = Dnf.getCanonicalForm(netDn, ProductionRule.DOWN,exclusives);
            return (dualUp.compareTo(netDn)==0);
        }

        /**
         * Are there any paths from this node to another output node?
         */
        private boolean hasSneakPaths() {
            boolean sneaky = false;
            for (Iterator t = paths.iterator(); t.hasNext(); ) {
                NetPath path = (NetPath) t.next();
                if (path.getDir()==0) sneaky = true;
            }
            if (verbose) System.err.println("hasSneakPaths=" + sneaky + "\n" + paths);
            return sneaky;
        }

        /**
         * Report illegal or sneak paths as LVSProblems.
         */
        public void reportSneakPaths() {
            for (Iterator t = paths.iterator(); t.hasNext(); ) {
                NetPath path = (NetPath) t.next();
                if ((path.getDir()==0) && 
                    (!path.getStartNode().passgate ||
                     !path.getEndNode().passgate))
                    problems.add(new LVSProblem("Sneak path: " + path));
            }
        }

        /**
         * Returns average strength (in transistor squares [w/l]) driving
         * this node down (0) and up (0).  Returns null if no paths
         * drive this node.
         **/
        public double[] getAverageDriveStrength() {
            if (!output) return null;
            if (!doneFoldingFactors) setFoldingFactors();
            return aveDriveStrength == null ? null : 
                new double[] { aveDriveStrength[0], aveDriveStrength[1] };
        }

        /** 
         * Returns the number of NFET(0) and PFET(1) transistors that
         * this node drives.
         **/
        public int[] getTransistorFanout() {
            if (!doneFoldingFactors) setFoldingFactors();
            return new int[] { edgeFanout[0], edgeFanout[1] };
        }

        public double[] getTransistorLoad() {
            if (!doneFoldingFactors) setFoldingFactors();
            return new double[] { gateLoad[0], gateLoad[1] };
        }
    }
    
    /** An edge of the NetGraph, with NetNodes for source, gate, and drain. */
    public final class NetEdge implements Comparable {

        /** The source NetNode, used to search the graph. */ 
        public final NetNode source;
        
        /** The drain NetNode, used to search the graph. */ 
        public final NetNode drain;
        
        /** The gate NetNode, used to check exclusion and report paths. */ 
        public final NetNode gate;
        
        /** Either N_TYPE or P_TYPE of DeviceTypes. */
        public final int type;

        /**
         * "Symmetrization" number constants.  These constants are
         * also defined in a base CAST cell, for user convenience.
         **/
        public static final int 
            DEFAULT_FOLD = 0,
            FORWARD_FOLD = 1,
            REVERSE_FOLD = 2,
            PARTIAL_FOLD = 3,
            TRUNK_FOLD   = 4;

        /** Merging of NetEdges is restricted to equal fold numbers. */
        final int fold;

        /** Width of the transistor. */
        public double width;

        /** Length of the transistor. */
        public double length;

        /** 
         * Relative size (relative width vs half-operator) of the transistor.
         * (Only set and used by Jauto code; should be removed)
         **/
        public double size = 1.0;

        /** 
         * Number of transistors in the longest stack going through this 
         * transistor. (Only set and used by Jauto code; should be removed)
         **/
        public int depth = 1;

        /** 
         * Number of half-operators who share this transistor. 
         * (Only set and used by Jauto code; should be removed)
         **/
        public int shareCount = 1;

        /** Does this NetEdge appear in any NetPath? */
        boolean used = false;
 
        /** Does this NetEdge appear in a logic NetPath? */
        boolean usedLogic = false;

        /** Was this NetEdge added from a library gate? */
        public final boolean library;

        /** Was this NetEdge added from a library gate but not in the pcell? */
        public final boolean floating;

        /** Transistor type */
        private final int transistorType;

        /** 
         * Folding factor. (2 means there is one other equivalent edge in 
         * the design, so only count this one as "half" a transistor.)
         * -1.0 indicates uninitialized.
         **/
        double foldingFactor = -1.0;

        /** Is this NetEdge used to precharge an internal node? */
        public final boolean precharge;

        /** Construct a new non-precharge NetEdge from simple parameters. */
        NetEdge (final NetNode S, 
                 final NetNode G, 
                 final NetNode D, 
                 final int T,
                 final int F,
                 final double W,
                 final double L, 
                 final boolean library,
                 final int transistorType ) {
            this(S, G, D, T, F, W, L, library, transistorType, false);
        }

        /** Construct a new NetEdge from simple parameters. */
        NetEdge (final NetNode S, 
                 final NetNode G, 
                 final NetNode D, 
                 final int T,
                 final int F,
                 final double W,
                 final double L, 
                 final boolean library,
                 final int transistorType,
                 final boolean precharge ) {
            gate   = G;
            source = S;
            drain  = D;
            used   = false;
            type   = T;
            fold   = F;
            width  = W;
            length = L;
            this.library = library;
            this.floating = library && (W==0);
            if (this.floating) width = defaultWidth;
            source.addEdge(this);
            drain.addEdge(this);
            gate.is_gate = true;
            if (type==DeviceTypes.N_TYPE) drain.hasN=source.hasN=true;
            if (type==DeviceTypes.P_TYPE) drain.hasP=source.hasP=true;
            this.transistorType = transistorType;
            this.precharge = precharge;
            Debug.assertTrue(L>0,"Transistor gate length must be >0");
        }

        /** Construct a new NetEdge from a Transistor, add NetNodes if necessary. */
        NetEdge (final Transistor t) {
            this(createNetNode( t.getSource() ),
                 createNetNode( t.getGate() ),
                 createNetNode( t.getDrain() ),
                 t.getType(), 0,
                 t.getWidth(), t.getLength(), false, 0, false);
            NetNode bulk = createNetNode(t.getBulk());
            if ( ( t.getType() == DeviceTypes.N_TYPE ) && ( ! bulk.isGND() ) ||
                 ( t.getType() == DeviceTypes.P_TYPE ) && ( ! bulk.isVdd() ) ) {
                problems.add( new LVSProblem( "Wrong bulk for transistor: " + t ) );
            }
        }

        /**
         * Compare NetEdges by gate, type, fold, source, drain.
         *
         * minimize() depends on sorting first by gate.
         **/
        public int compareTo (Object b) {
            int c;
            NetEdge e = (NetEdge) b;
            c = this.gate.compareTo(e.gate);
            if (c!=0) return c;
            c = this.type - e.type;
            if (c!=0) return c;
            c = this.fold - e.fold;
            if (c!=0) return c;
            c = this.source.compareTo(e.source);
            if (c!=0) return c;
            c = this.drain.compareTo(e.drain);
            return c;
        }

        /**
         * For partial symmetrization.  Recursively search along a
         * series stack toward the source, looking for a
         * port/gate/rail/contacted node.  Return the node.
         **/
        public NetNode findStackSource() {
            // return source node if it is port, gate, rail, or not 2 spokes
            if (source.is_port || source.is_gate || source.isRail() ||
                (source.edges.size()!=2)) 
                return source;

            // find next edge in chain
            NetEdge e;
            if (source.edges.get(0) == this)  e = (NetEdge) source.edges.get(1);
            else                              e = (NetEdge) source.edges.get(0);

            // recurse to next edge in chain
            if (e.source != source) return e.findStackSource();
            if (e.drain  != source) return e.findStackDrain(); // swap s/d
            Debug.assertTrue(false);  // e is a looped transistor!
            return null;
        }

        /**
         * For partial symmetrization.  Recursively search along a
         * series stack toward the drain, looking for a
         * port/gate/rail/contacted node.  Return the node.
         **/
        public NetNode findStackDrain() {
            // return drain node if it is port, gate, rail, or not 2 spokes
            if (drain.is_port || drain.is_gate || drain.isRail() || 
                (drain.edges.size()!=2)) 
                return drain;

            // find next edge in chain
            NetEdge e;
            if (drain.edges.get(0) == this)  e = (NetEdge) drain.edges.get(1);
            else                             e = (NetEdge) drain.edges.get(0);

            // recurse to next edge in chain
            if (e.drain  != drain) return e.findStackDrain();
            if (e.source != drain) return e.findStackSource(); // swap s/d
            Debug.assertTrue(false);  // e is a looped transistor!
            return null;
        }

        /**
         * Return the transistor type of this transistor.
         **/
        public int getTransistorType() {
            return transistorType;
        }

        /** 
         * Returns the folding factor of this NetEdge.  Values greater than
         * 1 indicate that this transistor has been folded, and transistor
         * counts involving this transistor and others in its folding set
         * should be scaled accordingly.
         **/
        public double getFoldingFactor() {
            if (!doneFoldingFactors) setFoldingFactors();
            return foldingFactor;
        }

        /** Debugging string. */
        public String toString () {
            return "NetEdge: type=" + (type == DeviceTypes.P_TYPE ? "P" : "N")
                + " transistor=" + getTransistorType() + 
                " W=" + NumberFormatter.format(width,3) + 
                " L=" + NumberFormatter.format(length,3) + 
                " used=" + used + " usedLogic=" + usedLogic +
                "\n   S=" + source.name + " D=" + drain.name + " G=" + gate.name;
        }

        public boolean isFeedbackOnly() {
            return used && !usedLogic;
        }

        public boolean isUsedLogic() {
            return usedLogic;
        }

        public boolean isSmallInverter() {
            return gate.gatesSmallInverter() && (gate.feedbackFrom == source ||
                                                 gate.feedbackFrom == drain);
        }

        /** Transistor skill format string. */
        public String skillString(int num) {
            NetNode bulk = type == DeviceTypes.N_TYPE ? GND : Vdd;
            return "(" + (type == DeviceTypes.N_TYPE ? "NMOS" : "PMOS") +
                " \"M" + num + 
                "\" \"" + source.name +
                "\" \"" + gate.name +
                "\" \"" + drain.name +
                "\" \"" + bulk.name +
                "\" " + width + " " + length + ")\n";
        }
    }

    /** A path between NetNodes through one or more NetEdges. */
    public class NetPath implements Comparable {

        /** Ordered list of edges between from and to. */
        ArrayList edges;

        /** Starting NetNode for the path. */
        NetNode from;
        
        /** Ending NetNode for the path. */
        NetNode to;

        /** Either N_TYPE or P_TYPE of DeviceTypes. */
        int type;

        /** Is this a feedback path for a staticizer? */
        boolean feedback = false;

        /**
         * Construct a new NetPath given two NetNodes and the NetEdges
         * between them.  Sets feedbackFrom/nonFeedback as a side effect.
         */
        NetPath (NetNode from, NetNode to, int type, ArrayList edges) {
            this.edges = edges;
            this.from  = from;
            this.to    = to;
            this.type  = type;
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                NetNode node = edge.gate;
                if (node.inverseOf == from) {
                    feedback = true;
                    if (from.feedbackFrom == null) from.feedbackFrom = node;
                    else if (from.feedbackFrom != node)
                        problems.add(new LVSProblem
                                     ("more than one feedback inverter to " + from));
                }
                else node.nonFeedback = true;
            }
        }

        /** Check if this path contains exclusive gates. */
        public boolean isExcluded () {
            MultiSet gates = getUniqueGates();
            if (type == DeviceTypes.P_TYPE)
                return exclusives.areExclusive(ExclusiveNodeSet.LO,gates);
            else if (type == DeviceTypes.N_TYPE)
                return exclusives.areExclusive(ExclusiveNodeSet.HI,gates);
            return false;
        }

        /** Return the type of the transistors in the path, N_TYPE or P_TYPE. */
        public final int getType() {
            assert (type==DeviceTypes.N_TYPE) || (type==DeviceTypes.P_TYPE) 
                : "NetPath.type should be either N_TYPE or P_TYPE";
            return type;
        }

        /** Find the minimum width of the (logic) edges in this path. */
        public double getMinWidth (boolean logic) {
            double minw=0;
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                if (edge.usedLogic!=logic) continue;
                if ((minw==0)||(edge.width<minw)) minw=edge.width;
            }
            return minw;
        }

        /** Find the total length of the edges in this path. */
        public double getTotalLength () {
            double totl=0;
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                double l = ((NetEdge) t.next()).length;
                totl += l;
            }
            return totl;
        }

        /** Find the number of fet squares in the path. */
        public double getSquares() {
            double R = 0;
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                R += edge.length/edge.width;
            }
            return R;
        }

        /** Find all unique edges in all equivalent paths */
        private MultiSet findEquivalentEdges(Collection paths) {
            MultiSet edges = new MultiSet();
            for (Iterator t = paths.iterator(); t.hasNext(); ) {
                NetPath path2 = (NetPath) t.next();
                if (this.compareTo(path2)!=0) continue; // not equivalent to path
                for (Iterator s = path2.edges.iterator(); s.hasNext(); ) {
                    NetEdge edge = (NetEdge) s.next();
                    boolean unique = true;
                    for (Iterator u = edges.iterator(); u.hasNext(); )
                        if (u.next() == edge) unique=false;
                    if (unique) edges.add(edge);
                }
            }
            return edges;
        }

        /** 
         * Find aggregate squares of transistor channel for all paths
         * logically equivalent to this path.  BUG: assumes paths are
         * consistently tapered!
         */
        public double getAggregateSquares(Collection paths) {
            // find all unique edges in all equivalent paths
            MultiSet edges = findEquivalentEdges(paths);
            
            // now aggregate resistance of equivalent transistors
            double R = 0; // series resistance, in squares
            for (Iterator t = this.edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                double G = 0; // parallel conductance
                for (Iterator s = edges.iterator(); s.hasNext(); ) {
                    NetEdge edge2 = (NetEdge) s.next();
                    if (edge2.gate.compareTo(edge.gate)!=0) continue;
                    assert (edge2.type == edge.type);
                    G += edge2.width/edge2.length;
                }
                R += 1/G;
            }
            if (verbose) System.err.println("getAggregateSquares=" + R + 
                                            " path=" + this);
            return R;
        }

        /**
         * Return a folding factor for this path with potentially
         * equivalent paths.  As a side-effect, sets foldingFactors of
         * all edges in this path.  Unlike getAggregateSquares, does
         * not consider actual width and length of transistors so does
         * not have problems with tapering.
         */
        public double getFoldingFactor(Collection paths) {
            // find all unique edges in all equivalent paths
            MultiSet edges = findEquivalentEdges(paths);
            
            // now aggregate resistance of equivalent transistors
            double R = 0; // series resistance, in squares
            for (Iterator t = this.edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                double G = 0; // parallel conductance
                for (Iterator s = edges.iterator(); s.hasNext(); ) {
                    NetEdge edge2 = (NetEdge) s.next();
                    if (edge2.gate.compareTo(edge.gate)!=0) continue;
                    assert (edge2.type == edge.type);
                    G += 1;
                }
                R += 1/G;
            }

            // compute folding factor
            double foldingFactor = this.edges.size()/R;
            if (verbose) System.err.println("getFoldingFactor=" + 
                                            foldingFactor + " path=" + this);

            // set foldingFactor of edges
            for (Iterator ei=edges.iterator(); ei.hasNext();) {
                NetEdge edge = (NetEdge) ei.next();
                assert (edge.foldingFactor == -1 || 
                        edge.foldingFactor == foldingFactor);
                edge.foldingFactor = foldingFactor;
            }
            return foldingFactor;
        }

        /** Return a MultiSet of the gate names along this path. */
        public MultiSet getGates () {
            MultiSet gates = new MultiSet();
            for (Iterator t = edges.iterator(); t.hasNext(); )
                gates.add(((NetEdge) t.next()).gate.name);
            return gates;
        }

        /** Return a MultiSet of the unique gate names along this path. */
        public MultiSet getUniqueGates () {
            MultiSet gates = new MultiSet();
            for (Iterator t = edges.iterator(); t.hasNext(); )
                gates.addIfUnique(((NetEdge) t.next()).gate.name);
            return gates;
        }

        /** Return an ArrayList of the gate names along this path. */
        public ArrayList getGatesArrayList () {
            ArrayList gates = new ArrayList();
            for (Iterator t = edges.iterator(); t.hasNext(); )
                gates.add(((NetEdge) t.next()).gate.name);
            return gates;
        }

        /** Returns the set of gate NetNodes along this path. **/
        public Set getGateNodes() {
            HashSet gates = new HashSet();
            for (Iterator t = edges.iterator(); t.hasNext(); )
                gates.add(((NetEdge)t.next()).gate);
            return gates;
        }

        /** Mark all edges used/usedLogic in this path. */
        void markUsedEdges () {
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                NetEdge e = (NetEdge) t.next();
                e.used=true;
                if (!feedback) e.usedLogic=true;
            }
        }

        /**
         * Return path direction: -1 to GND via N_TYPE, 1 to Vdd via
         * P_TYPE, 0 for sneak paths or pass gate paths.  Do not use
         * this method if you aren't prepared to robustly handle pass
         * gates.  Use getType instead if you just want to know if the
         * transistors are N_TYPE or P_TYPE.
         */
        public int getDir() {
            if ((type==DeviceTypes.P_TYPE) && from.isVdd() && (!to.isRail())) return +1;
            if ((type==DeviceTypes.N_TYPE) && from.isGND() && (!to.isRail())) return -1;
            if ((type==DeviceTypes.P_TYPE) && to.isVdd() && (!from.isRail())) return +1;
            if ((type==DeviceTypes.N_TYPE) && to.isGND() && (!from.isRail())) return -1;
            return 0;
        }

        /** Return if this path is a feedback path */
        public boolean isFeedBack() {
            return feedback;
        }

        /** Is this a weak interfering feedback path? **/
        public boolean isWeakFeedBack() {
            if (!feedback) return false;
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                NetNode node = edge.gate;
                if (node.inverseOf != from) return false;
            }
            return true;
        }

        /** Size a weak feedback path. **/
        public void sizeWeakFeedBack(double width, double length, int N) {
            Debug.assertTrue(isWeakFeedBack());
            Debug.assertTrue(edges.size()>0);
            
            // get transistorType
            int transistorType = getTransistorType();
            Debug.assertTrue(transistorType!=0);

            // delete old edges, find feedback gate
            NetNode G=null;
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                edge.source.edges.remove(edge);
                edge.drain.edges.remove(edge);
                if (G==null) G=edge.gate;
                else Debug.assertTrue(G==edge.gate);
            }
            edges = new ArrayList();

            // add N edges in series
            NetNode S = from;
            NetNode D;
            for (int i=0; i<N; i++) {
                if (i+1<N) D = new NetNode();
                else D = to;
                NetEdge edge = new NetEdge(S,G,D,
                                           type,0,width,length,
                                           false,transistorType);
                edges.add(edge);
                S=D;
            }
        }

        /** Sets the width of edges in a feedback path, meant for combinational feedback **/
        public void sizeFeedback(double width) {
            Debug.assertTrue(isFeedBack());
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                if (!edge.usedLogic) edge.width=width;
            }
        }

        /** Return depth */
        public int getDepth() {
            return edges.size();
        }

        /** Return the list of edges */
        public ArrayList getEdges() {
            return edges;
        }

        /** Return the transistor type, or 0 if inconsistent **/
        public int getTransistorType() {
            int type = 0;
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                NetEdge edge = (NetEdge) t.next();
                if (type==0) type = edge.getTransistorType();
                else if (type!=edge.getTransistorType()) return 0;
            }
            return type;
        }

        /** Return starting netnode */
        public NetNode getStartNode() {
            return from;
        }
        
        /** Return ending netnode */
        public NetNode getEndNode() {
            return to;
        }

        /** Compare paths by from/to/type/feedback/gates. */
        public int compareTo(Object b) {
            int c;
            NetPath path = (NetPath) b;
            c=this.type - path.type;
            if (c!=0) return c;
            c=this.from.compareTo(path.from);
            if (c!=0) return c;
            c=this.to.compareTo(path.to);
            if (c!=0) return c;
            if      ( this.feedback && !path.feedback) return -1;
            else if (!this.feedback &&  path.feedback) return +1;
            MultiSet gates1 = this.getGates();
            MultiSet gates2 = path.getGates();
            return gates1.compareTo(gates2);
        }

        /** Debugging string. */
        public String toString () {
            String s = "NetPath: type=" + (type == DeviceTypes.P_TYPE ? "P" : "N") +
                " W=" + NumberFormatter.format(getMinWidth(true),3) + 
                " L=" + NumberFormatter.format(getTotalLength(),3) + 
                " tt=" + getTransistorType() + " fb=" + feedback +
                "\n   S=" + from.name + " D=" + to.name + " G=";
            for (Iterator t = edges.iterator(); t.hasNext(); ) {
                s += " " + ((NetEdge) t.next()).gate.name;
            }
            return s;
        }
    }
    

    /** Construct NetGraph.  Used by jflat */
    public NetGraph(final CellInterface cell, 
                    final Cadencize cadencize,
                    final CastFileParser cfp) {
        final CadenceInfo ci = cadencize.convert(cell);
        final ExclusiveNodeSets exclusiveSets = new ExclusiveNodeSets();
        exclusiveSets.merge(ci.getPortExclusiveNodeSets());
        exclusiveSets.merge(ci.getLocalExclusiveNodeSets());
        namespace = ci.getLocalNodes();
        exclusives = exclusiveSets.canonicalizeNames(namespace);
        problems = new ArrayList();       
        nodes = new MultiSet<>();
        nostaticizers = new HashSet();
        Vdd = createNetNode(HierName.makeHierName("Vdd")); Vdd.isPower=true;
        GND = createNetNode(HierName.makeHierName("GND")); GND.isGround=true;
        defaultWidth  = 0;
        defaultLength = 0;
        nextNodeNum = 1;
        try {
            this.addCellInterface(cell, new NetGraph[0], cfp, cadencize);
        } catch (com.avlsi.prs.UnimplementableProductionRuleException e)
        {
            throw new RuntimeException("Can't happen");
        }
        this.prepareForLvs();
        assert problems.isEmpty();
    }
    
    /** Construct an empty NetGraph.  Used by jauto and jlvs. */
    public NetGraph(final AliasedSet commonNamespace, 
                    final ExclusiveNodeSets exclusiveSets,
                    final List ProblemList,
                    final HierName VddName,
                    final HierName GndName,
                    final double defaultWidth,
                    final double defaultLength,
                    final Set nostaticizers) {

        if (commonNamespace != null) namespace = commonNamespace;
        else namespace = new AliasedSet(HierName.getComparator());
        if (exclusiveSets != null)
            exclusives = exclusiveSets.canonicalizeNames(namespace);
        else exclusives = new ExclusiveNodeSets();
        if (ProblemList != null) problems = ProblemList;
        else problems = new ArrayList();
        nodes = new MultiSet<>();
        this.nostaticizers = new HashSet();
        if (nostaticizers != null) 
            for (Iterator i = nostaticizers.iterator(); i.hasNext(); ) {
                HierName h = (HierName) i.next();
                HierName canon = (HierName) namespace.getCanonicalKey(h);
                this.nostaticizers.add(canon);
            }
        Vdd = createNetNode(VddName); Vdd.isPower=true;
        GND = createNetNode(GndName); GND.isGround=true;
        this.defaultWidth  = defaultWidth;
        this.defaultLength = defaultLength;
        nextNodeNum = 1;
    }

    /**
     * Construct NetGraph without defaultWidth and defaultLength for
     * old tools or those that don't create new transistors.
     **/
    public NetGraph(final AliasedSet commonNamespace, 
                    final ExclusiveNodeSets exclusiveSets,
                    final List ProblemList,
                    final HierName VddName,
                    final HierName GndName,
                    final Set nostaticizers) {
        this(commonNamespace, exclusiveSets, ProblemList,
             VddName, GndName, 0, 0, nostaticizers);
    }

    /**
     * Construct an empty NetGraph with minimal options.  Used by
     * CDL2Cast.  This is the only constructor to support multiple
     * power_nets and ground_nets so far.
     */
    public NetGraph(final String[] power_nets, final String[] ground_nets) {
        this(null,null,null,
             HierName.makeHierName(power_nets[0]),
             HierName.makeHierName(ground_nets[0]),null);
        for (String str : power_nets) {
            NetNode node = createNetNode(HierName.makeHierName(str)); node.isPower=true;
        }
        for (String str : ground_nets) {
            NetNode node = createNetNode(HierName.makeHierName(str)); node.isGround=true;
        }
        pathsToRailOnly = true; // don't stop paths until they reach rails
    }

    /** Check if this netgraph is empty (0 netedges). */
    public boolean isEmpty() {
        return nodes.stream().allMatch(x -> x.edges.isEmpty());
    }

    /** Returns set of all exclusive node sets in the NetGraph **/
    public ExclusiveNodeSets getExclusiveNodeSets() {
        return exclusives;
    }

    /** Add transistors and nodes of an Aspice netlist to this NetGraph. */
    public void addAspice(final AspiceFile aspiceCell) {
        markPortNodes(aspiceCell);
        Iterator t = aspiceCell.getTransistors();
        while ( t.hasNext() ) {
            new NetEdge((Transistor) t.next());
        }
    }

    /** Add edges and nodes of another NetGraph to this NetGraph, with name mapping. */
    public void addNetGraph(NetGraph netgraph, TreeMap map, boolean library) {
        if (map == null) map = new TreeMap(); // start with empty map
        else map = new TreeMap(map); // copy original map
        Iterator t = netgraph.getEdges().iterator();
        while (t.hasNext()) {
            NetEdge edge = (NetEdge) t.next();
            new NetEdge( mapNetNode( edge.source.name, map ),
                         mapNetNode( edge.gate.name  , map ),
                         mapNetNode( edge.drain.name , map ),
                         edge.type,
                         edge.fold,
                         edge.width,
                         edge.length,
                         library,
                         edge.getTransistorType(),
                         edge.precharge);
        }
    }

    private int getHalfOpDirective(final Map ttype, final HierName halfop,
                                   final Integer celltt) {
        final Integer val = (Integer) ttype.get(halfop);
        return (val == null ? celltt : val).intValue();
    }

    private int getTransistorType(final Map ttype, final HierName halfop,
                                  final Integer celltt) {
        return getHalfOpDirective(ttype, halfop, celltt);
    }

    /** Return a gate NetGraph with appropriate transistor types **/
    private NetGraph getMultipleTransistorGate(
            final NetGraph model,
            final Iterator/*<HierName>*/ goutputs,
            final Map ttup,
            final Map ttdn,
            final Integer ctt,
            final CastFileParser cfp,
            final Cadencize cad) {
        if (cfp == null) return model;
        if (model == null) return null;

        final String baseType = CellUtils.getBaseType(model.gateType);
        /* For backwards compatibility.  If the gate does not take
         * a meta-parameter, than do not tack on the transistor
         * types as meta-parameters.
         */
        if (baseType.equals(model.gateType)) return model;

        Integer typeup = null, typedn = null;
        if (goutputs==null) { typeup = ctt; typedn = ctt; } // smallInv
        else while (goutputs.hasNext()) {
            final HierName out = (HierName) goutputs.next();

            final int up = getTransistorType(ttup, out, ctt);
            if (typeup == null) typeup = new Integer(up);
            else if (typeup.intValue() != up) {
                throw new RuntimeException("Do not know how to handle a multiple output gate with different transistor type for the outputs.");
            }

            final int dn = getTransistorType(ttdn, out, ctt);
            if (typedn == null) typedn = new Integer(dn);
            else if (typedn.intValue() != dn) {
                throw new RuntimeException("Do not know how to handle a multiple output gate with different transistor type for the outputs.");
            }
        }
        assert typeup != null && typedn != null :
               "No output for gate " + model.gateType;

        final String newtype = baseType + "(" + typedn + "," + typeup + ")";
        try {
            return CastDesign.getGateNetGraph(cfp, newtype, Vdd.name, GND.name, cad);
        } catch (Exception e) {
            return model;
        }
    }

    /**
     * Add the production rules from a CellInterface.
     *
     * Minimizes the resulting netgraph, and obeys the the symmetric
     * and unshared directives.
     *
     * The appropriate nostaticizer directive information must be
     * passed in via the constructor.
     *
     * Exclusives from the cell are not used; they are taken from
     * those passed in to the constructor.  Similarly, when adding
     * staticizers, they must be taken from the list passed into the
     * constructor.
     *
     * Minimization is very expensive for large cells.  Most of this
     * is complication to do most of the minimization on small subsets
     * of the cells.  So, basically, we make a seperate netgraph for
     * each operator, and minimize that.  Then we merge them together
     * into sets that are mutually exclusive, and minimize them.  Then
     * we merge those sets togethether and minimize the large set.
     *
     * @param ci The CellInterface to snarf production rules and directives.
     * @param gates A list of gates to match against.
     * @param cfp The CastFileParser to get gate definitions from for multiple
     * transistor support.
     * @param cad Cadencize to use to for multiple transistor support.
     * @return none.
     */
    public void addCellInterfacePrs(final CellInterface ci,
                                    final NetGraph[] gates,
                                    final CastFileParser cfp,
                                    final Cadencize cad)
        throws UnimplementableProductionRuleException {
        markPortNodes(ci); // used to identify operators
        ProductionRuleSet prs = ci.getProductionRuleSet();
        
        // query shared directives
        Map shared = DirectiveUtils.getPrsDirective(
                                                    ci, DirectiveConstants.SHARED,
                                                    DirectiveConstants.HALFOP_TYPE);
        Map sharedup = DirectiveUtils.canonizeKey(namespace,
                                                  DirectiveUtils.getUps(shared));
        Map shareddown = DirectiveUtils.canonizeKey(namespace,
                                                    DirectiveUtils.getDowns(shared));

        // query symmetrize directives
        Map sym = DirectiveUtils.getPrsDirective(
                                                 ci, DirectiveConstants.SYMMETRIZE,
                                                 DirectiveConstants.HALFOP_TYPE);
        Map symup = DirectiveUtils.canonizeKey(namespace,
                                               DirectiveUtils.getUps(sym));
        Map symdown = DirectiveUtils.canonizeKey(namespace,
                                                 DirectiveUtils.getDowns(sym));

        // query transistor type directives
        Map ttype = DirectiveUtils.getPrsDirective(ci,
                DirectiveConstants.TRANSISTOR_TYPE,
                DirectiveConstants.HALFOP_TYPE);
        final Map ttup = DirectiveUtils.canonizeKey(namespace,
                DirectiveUtils.getUps(ttype));
        final Map ttdn = DirectiveUtils.canonizeKey(namespace,
                DirectiveUtils.getDowns(ttype));
        final Integer ctt = (Integer) DirectiveUtils.getTopLevelDirective(ci,
                DirectiveConstants.TRANSISTOR_TYPE);

        prs.canonicalizeNames(namespace);

        final Map symFullX = findXCandidates(symdown, prs);

        // Build whole network in totally unmerged DNF form
        addPrs(ci,prs.getProductionRules(),null,true,symup,sharedup,ttup,symdown,shareddown,ttdn,ctt,symFullX);

        // Substitute all gates.
        for (int i = 0; i < gates.length; i++) {
            final List/*<NetNode>*/ goutputs =
                gates[i].getUnmatchedOperatorNodes();
            final NetGraph currGate = gates[i];
            substituteGate(currGate, new UnaryFunction() {
                public Object execute(final Object o) {
                    final Map/*<HierName,HierName>*/ map = (Map) o;
                    return getMultipleTransistorGate(currGate,
                        new MappingIterator(goutputs.iterator(),
                                            new UnaryFunction() {
                            public Object execute(Object o) {
                                final HierName formal = ((NetNode) o).name;
                                return map.get(formal);
                            }
                        }),
                        ttup, ttdn, ctt, cfp, cad);
                }
            });
        }

        // Remove all transistors that aren't gates.
        Set unmatched = new HashSet();
        for (NetNode n: nodes) {
            if (!n.output || (n.gate != null)) continue;
            unmatched.add(n.name);
            n.deleteEdges();
        }

        // If we could ensure that sets of exclusives were disjoint,
        // we could improve the efficiency of Partition.  FIXME:
        // should perhaps only keep production rules with proper
        // targets, instead of filtering during adding.
        Partition part =
            new Partition (new PartitionProductionRulesByExclusives(exclusives),
                           prs.getProductionRules());
        
        for (Iterator pi = part.iterator(); pi.hasNext(); ) {
            Set pprs = (Set) pi.next();
            if (verbose) System.err.println("adding and minimizing " + pprs);
            
            // construct netgraph by building trees for prs
            NetGraph ng = new NetGraph(namespace, exclusives, new ArrayList(),
                                       Vdd.name, GND.name,
                                       defaultWidth, defaultLength, nostaticizers);
            ng.addPrs(ci,pprs.iterator(),unmatched,false,symup,sharedup,ttup,symdown,shareddown,ttdn,ctt,symFullX);
            
            // if sneak paths, construct netgraph again without trees
            if (ng.hasSneakPaths()) {
                ng = new NetGraph(namespace, exclusives, new ArrayList(),
                                  Vdd.name, GND.name, defaultWidth, defaultLength, nostaticizers);
                ng.addPrs(ci,pprs.iterator(),unmatched,true,symup,sharedup,ttup,symdown,shareddown,ttdn,ctt,symFullX);
            }
            
            // minimize and mergeFrom
            ng.minimize();
            mergeFrom(ng);
        }
    }

    private static class SymFullX {
        public static class AtomicBooleanComparator implements Comparator {
            private static AtomicBooleanComparator singleton = null;
            public int compare(final HierNameAtomicBooleanExpression e1,
                               final HierNameAtomicBooleanExpression e2) {
                return e1.getName().compareTo(e2.getName());
            }
            public int compare(final Object o1, final Object o2) {
                return compare((HierNameAtomicBooleanExpression) o1,
                               (HierNameAtomicBooleanExpression) o2);
            }
            public static AtomicBooleanComparator getInstance() {
                if (singleton == null) {
                    singleton = new AtomicBooleanComparator();
                }
                return singleton;
            }
        }

        public Set up2 = new TreeSet(AtomicBooleanComparator.getInstance());
        public Set parallel =
            new TreeSet(AtomicBooleanComparator.getInstance());
        public void reorder(final Collection conjuncts,
                            final boolean reverseOrder, final List result) {
            final List last = new ArrayList();
            for (Iterator i = conjuncts.iterator(); i.hasNext(); ) {
                HierNameAtomicBooleanExpression literal =
                    (HierNameAtomicBooleanExpression) i.next();
                if (parallel.contains(literal)) {
                    last.add(literal.getName());
                } else if (!up2.contains(literal)) {
                    if (reverseOrder) {
                        result.add(0, literal.getName());
                    } else {
                        result.add(literal.getName());
                    }
                }
            }

            final Iterator i = up2.iterator();
            final HierNameAtomicBooleanExpression termA =
                (HierNameAtomicBooleanExpression) i.next();
            final HierNameAtomicBooleanExpression termB =
                (HierNameAtomicBooleanExpression) i.next();
            assert !i.hasNext();

            if (reverseOrder) {
                result.add(termA.getName());
                result.add(0, termB.getName());
            } else {
                result.add(0, termA.getName());
                result.add(termB.getName());
            }

            result.addAll(0, last);
        }
        /**
         * Do the given conjuncts contain the 2 up terms?
         **/
        public boolean containedBy(final Collection conjuncts) {
            final Set s = new TreeSet(AtomicBooleanComparator.getInstance());
            s.addAll(conjuncts);
            return s.containsAll(up2);
        }
    }

    /**
     * Find a set of production rule targets that satisfies the requirements of
     * the SYM_FULL_X symmetrization style.
     **/
    private Map findXCandidates(final Map symdown, ProductionRuleSet rules) {
        // Get down half operators that has the SYM_FULL_X directive
        final Set fullX = (Set) CollectionUtils.addAll(new HashSet(), 
            new FilteringIterator(
                symdown.keySet().iterator(),
                new UnaryPredicate() {
                    public boolean evaluate(final Object o) {
                        return ((Integer) symdown.get(o)).intValue() == 5;
                    }
                })
            );
        if (fullX.isEmpty()) return Collections.EMPTY_MAP;

        final Set temp =
            new TreeSet(SymFullX.AtomicBooleanComparator.getInstance());

        final Map candidates = new HashMap();
        final Set disqualified = new HashSet();
        // look at all up half-operators which has SYM_FULL_X, and see which
        // ones qualifies
        for (Iterator i = rules.getProductionRules(); i.hasNext(); ) {
            final ProductionRule rule = (ProductionRule) i.next();
            final HierName target = rule.getTarget();
            final int direction = rule.getDirection();
            if (direction != ProductionRule.UP || !fullX.contains(target) ||
                disqualified.contains(target)) continue;

            if (!candidates.containsKey(target)) {
                candidates.put(target, new SymFullX());
            }
            final SymFullX sym = (SymFullX) candidates.get(target);


            final Collection disjuncts =
                rule.getGuard().DNFForm().getDisjuncts();
        DISJUNCTS:
            for (Iterator j = disjuncts.iterator(); j.hasNext(); ) {
                final Collection conjuncts = 
                    ((AndBooleanExpressionInterface) j.next()).getConjuncts();
                temp.clear();
                temp.addAll(conjuncts);
                switch (temp.size()) {
                  case 1: sym.parallel.addAll(temp); break;
                  case 2: if (sym.up2.isEmpty()) {
                              sym.up2.addAll(temp);
                              break;
                          } else if (sym.up2.containsAll(temp)) break;
                          /* fall through */
                  default: candidates.remove(target);
                           disqualified.add(target);
                           break DISJUNCTS;
                }
            }
        }

        for (Iterator i = candidates.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final SymFullX sym = (SymFullX) entry.getValue();
            if (sym.up2.size() != 2) i.remove();
        }

        // eliminate half operators without the correct down topology
        for (Iterator i = rules.getProductionRules();
             i.hasNext() && !candidates.isEmpty(); ) {
            final ProductionRule rule = (ProductionRule) i.next();

            final int direction = rule.getDirection();
            if (direction != ProductionRule.DOWN) continue;

            final HierName target = rule.getTarget();
            final SymFullX sym = (SymFullX) candidates.get(target);
            if (sym == null) continue;

            final Collection disjuncts =
                rule.getGuard().DNFForm().getDisjuncts();
            for (Iterator j = disjuncts.iterator(); j.hasNext(); ) {
                final Collection conjuncts = 
                    ((AndBooleanExpressionInterface) j.next()).getConjuncts();
                if (!sym.containedBy(conjuncts)) {
                    candidates.remove(target);
                    break;
                }
            }
        }
        return candidates;
    }

    /**
     * Add an iterator of prs to this NetGraph.  If unmatched is
     * non-null, will only add targets in the unmatched list.  Can
     * specify wheather it is safe to build shared prs trees.
     **/
    private void addPrs(CellInterface ci,
                Iterator prs, Set unmatched, boolean treeUnsafe,
                Map symup, Map sharedup, Map ttup, Map symdown, Map shareddown,
                Map ttdn, Integer ctt, Map symFullX) 
        throws UnimplementableProductionRuleException {
        this.treeUnsafe = treeUnsafe;
        while (prs.hasNext()) {
            ProductionRule p = (ProductionRule) prs.next();
            if (!p.isInverseMonotonic()) {
                throw new UnimplementableProductionRuleException(p,ci);
            }
            int direction = p.getDirection();
            HierName target = p.getTarget();
            if ((unmatched!=null) && !unmatched.contains(target)) continue;
            if (direction == ProductionRule.UP) {
                addFlavoredProductionRule(p, 
                                          (Integer) symup.get(target),
                                          (Integer) sharedup.get(target),
                                          getTransistorType(ttup, target, ctt),
                                          (SymFullX) symFullX.get(target));
            } else if (direction == ProductionRule.DOWN) {
                addFlavoredProductionRule(p, 
                                          (Integer) symdown.get(target),
                                          (Integer) shareddown.get(target),
                                          getTransistorType(ttdn, target, ctt),
                                          (SymFullX) symFullX.get(target));
            } else {
                Debug.assertTrue(false, "Illegal production rule direction");
            }
        }
        this.treeUnsafe = false;
        prepareForLvs();
    }

    /** add PRS with shared and symmetrize directives */
    private void addFlavoredProductionRule(ProductionRule p, 
                                           Integer symmetric,
                                           Integer foldnum,
                                           int transistorType,
                                           SymFullX symFullX) {
        int sym = (symmetric != null) ? symmetric.intValue() : 0;
        int fold = (foldnum != null) ? foldnum.intValue() + 1 : 0;
        double width = defaultWidth, length = defaultLength;
        if (sym==1) { // SYM_PARTIAL
            addProductionRule(p, false, false, 
                              fold_from_group(fold,NetEdge.PARTIAL_FOLD),
                              width, length,
                              transistorType, symFullX);
        } else if (sym==4) { // SYM_TRUNK
            addProductionRule(p, false, false, 
                              fold_from_group(fold,NetEdge.TRUNK_FOLD),
                              width, length,
                              transistorType, symFullX);
        } else if ((sym==2) || (sym==3) || (sym==5)) {
            // SYM_SKIP_FIRST or SYM_FULL or SYM_FULL_X
            addProductionRule(p, false, (sym==2),
                              fold_from_group(fold,NetEdge.FORWARD_FOLD),
                              width/2, length,
                              transistorType, symFullX);
            addProductionRule(p, true,  (sym==2),
                              fold_from_group(fold,NetEdge.REVERSE_FOLD),
                              width/2, length,
                              transistorType, symFullX);
        } else { // SYM_NONE
            addProductionRule(p, false, false,
                              fold_from_group(fold,NetEdge.DEFAULT_FOLD),
                              width, length,
                              transistorType, symFullX);
        }
    }

    /**
     * Helper for addCellIntefaceNetlist.  It adds transistors, and inlines
     * gates.
     **/
    private class NetlistHelper extends CDLFactoryAdaptor {
        private final CastFileParser cfp;
        private final Cadencize cad;
        private final double cdlScale;
        private final boolean removeZero;
        private final Map passgates;
        private final boolean precharge;
        private final TreeMap nameMap = new TreeMap();
        private NetNode createNetNode(HierName name) {
            if (!name.isNumeric() && !name.isGenerated()) nameMap.put(name, name);
            final NetNode node = mapNetNode(name, nameMap);
            final Boolean b = (Boolean) passgates.get(name.getAsString('.'));
            final boolean passgate = b == null ? false : b.booleanValue();
            node.passgate = passgate;
            return node;
        }
        public NetlistHelper(double cdlScale,
                             final CastFileParser cfp,
                             final Cadencize cad,
                             final boolean removeZero,
                             final Map passgates,
                             final boolean precharge) {
            this.cdlScale = cdlScale;
            this.cfp = cfp;
            this.cad = cad;
            this.removeZero = removeZero;
            this.passgates = passgates;
            this.precharge = precharge;
        }

        public void makeTransistor(final HierName name, final String type,
                                   final HierName ns, final HierName nd,
                                   final HierName ng, final HierName nb,
                                   final CDLLexer.InfoToken width,
                                   final CDLLexer.InfoToken length,
                                   final Map parameters,
                                   final Environment env) {
            final double w = CDLInterfaceSimplifier.getValue(width, env);
            if (w == 0 && removeZero) return;

            final double l = CDLInterfaceSimplifier.getValue(length, env);
            final int np =
                type.startsWith("p") || type.startsWith("P") ?
                    DeviceTypes.P_TYPE : DeviceTypes.N_TYPE;

            final Integer ttype = CellUtils.extractTransistorNumber(type);

            new NetEdge(createNetNode(ns), createNetNode(ng), createNetNode(nd),
                        np, 0, w * cdlScale, l * cdlScale, false,
                        ttype == null ? 0 : Math.abs(ttype.intValue()),
                        precharge);
        }

        public void makeCall(final HierName name, final String subName,
                             final HierName[] args, final Map parameters,
                             final Environment env) {
            final CellInterface ci;
            try {
                ci = cfp.getFullyQualifiedCell(subName);
            } catch (CastSemanticException e) {
                throw new RuntimeException("Cannot load cell " + subName, e);
            }

            final SortedSet ports = NetlistAdapter.getParameterList(ci, cad);
            if (ports.size() != args.length) {
                throw new RuntimeException("Parameter mismatch in subcircuit " + 
                                           "instantiation: " + name + 
                                           "/" + subName + 
                                           " ports = " + ports + 
                                           " args.length = " + args.length);
            }

            // Prepare the mapping from formal parameters to actual parameters
            final TreeMap map = new TreeMap();
            int i = 0;
            for (Iterator it = ports.iterator(); it.hasNext(); ++i) {
                final HierName formal = (HierName) it.next();
                map.put(formal, createNetNode(args[i]).name);
            }

            // Inline the gate NetGraph into the NetGraph.  Can't use
            // CastDesign.getGateNetGraph, because we need to pass along
            // instantiation parameters.
            final NetGraph ng = new NetGraph(null, null, null,
                                             Vdd.name, GND.name,
                                             defaultWidth, defaultLength, Collections.EMPTY_SET);
            ng.addCellInterfaceNetlist(ci, cfp, cad, parameters, true);
            ng.gateType = subName;
            final boolean stack = 
                ((Boolean) DirectiveUtils.getTopLevelDirective
                 (ci, DirectiveConstants.STACK_GATE)).booleanValue();
            if (!stack) ng.prepareForLvs();
            addNetGraph(ng, map, !stack);

            if (!stack) {
                final boolean staticizer = 
                    ((Boolean) DirectiveUtils.getTopLevelDirective
                     (ci, DirectiveConstants.STATICIZER_GATE)).booleanValue();
                final boolean precharge =
                    ((Boolean) DirectiveUtils.getTopLevelDirective
                     (ci, DirectiveConstants.PRECHARGE_PRIMITIVE)).booleanValue();
                final NetNode prechargeDrain;
                if (precharge) {
                    // For a precharge primitive, only put the gate instance on
                    // the D node.  This is only appropriate if we are using
                    // stack.PRECHARGE_[NP].0.  There is no easy way to not
                    // hardcode this, unless we introduce another directive.
                    prechargeDrain = ng.findNetNode(HierName.makeHierName("D"));
                    if (prechargeDrain == null)
                        throw new RuntimeException("Cannot find node D in " +
                                                   "precharge gate " + subName);
                } else {
                    prechargeDrain = null;
                }
                // Mark output nodes with appropriate GateInstance
                final GateInstance inst =
                    new GateInstance(subName, map, ng, precharge);
                final List goutputs = prechargeDrain == null ?
                    ng.getUnmatchedOperatorNodes() :
                    Collections.singletonList(prechargeDrain);
                for (i = 0; i < goutputs.size(); i++) {
                    final NetNode gnode = (NetNode) goutputs.get(i);
                    final NetNode node = mapNetNode(gnode.name,map);
                    if (staticizer) {
                        if (node.staticizer == null) {
                            node.staticizer = inst;
                        } else {
                            throw new RuntimeException
                                ("A staticizer of type " + node.staticizer.getType() + 
                                 " already attached to " + node.name + 
                                 ", cannot attach a staticizer of type " + subName);
                        }
                    } else {
                        if (node.gate == null) {
                            node.gate = inst;
                        } else {
                            throw new RuntimeException
                                ("A gate of type " + node.gate.getType() + 
                                 " already attached to " + node.name + 
                                 ", cannot attach a gate of type " + subName);
                        }
                    }
                }
            }
        }
    }

    /**
     * Convert the netlist block from a CellInterface.
     *
     * Construct the netgraph from the transistors in the netlist block as is.
     *
     * @param ci The CellInterface to obtain the netlist block and directives.
     * @param cfp The CastFileParser to get gate cells from.
     * @param cad The Cadencize used to get the port list.
     * @param params Instantiation parameters
     * @param removeZero Do not add zero width transistors
     * @return none.
     **/
    private void addCellInterfaceNetlist(final CellInterface ci,
                                         final CastFileParser cfp,
                                         final Cadencize cad,
                                         final Map params,
                                         final boolean removeZero) {
        markPortNodes(ci); // used to identify operators
        final BlockIterator bi =
            ci.getBlockInterface().iterator(BlockInterface.NETLIST);
        Debug.assertTrue(bi.hasNext(), "No netlist block found in " + 
                         ci.getFullyQualifiedType());
        final NetlistBlock nb = (NetlistBlock) bi.next();
        final float cdlScale =
            ((Float) DirectiveUtils.getTopLevelDirective
             (ci, DirectiveConstants.CDLSCALE)).floatValue();
        final Map passgates =
            DirectiveUtils.getTopLevelDirective
            (ci, DirectiveConstants.PASSGATE, DirectiveConstants.STRING_TYPE);
        final boolean precharge =
            ((Boolean) DirectiveUtils.getTopLevelDirective
             (ci, DirectiveConstants.PRECHARGE_PRIMITIVE)).booleanValue();
        final NetlistHelper helper = 
            new NetlistHelper(cdlScale, cfp, cad, removeZero, passgates,
                              precharge);
        final Template templ = nb.getCanonicalTemplate(cad);
        try {
            templ.execute(helper, params, NullEnvironment.getInstance(), null);
        } catch (RuntimeException e) {
            throw new RuntimeException("Cannot create netgraph for " +
                                       ci.getFullyQualifiedType(), e);
        }
    }

    private void addCellInterfaceNetlist(final CellInterface ci,
                                         final CastFileParser cfp,
                                         final Cadencize cad) {
        addCellInterfaceNetlist(ci, cfp, cad, Collections.EMPTY_MAP, false);
    }

    /**
     * Construct a netgraph from a CellInterface.  It will generate transistors
     * for cells with a PRS block or a netlist block.  Normally, the netlist
     * block is given preference over the PRS block, if it exists.
     *
     * @param ci The CellInterface to add.
     * @param gates An array of gates to match against.
     * @param cfp Where to get definitions for gates in the netlist block.  May
     * be <code>null</code> if multiple transistor support is not desired.
     * @param cad Cadencize to use to get the canonical port list for a cell.
     * May be <code>null</code> if multiple transistor support is not desired.
     * @return none.
     **/
    public void addCellInterface(final CellInterface ci,
                                 final NetGraph[] gates,
                                 final CastFileParser cfp,
                                 final Cadencize cad) 
        throws UnimplementableProductionRuleException {
        if (ci.containsNetlist()) {
            addCellInterfaceNetlist(ci, cfp, cad);
        } else {
            addCellInterfacePrs(ci, gates, cfp, cad);
        }
    }

    /** Construct a netgraph from a CellInterface with a List of gates */
    public void addCellInterface(final CellInterface ci,
                                 final List gates,
                                 final CastFileParser cfp,
                                 final Cadencize cad) 
        throws UnimplementableProductionRuleException {
        addCellInterface(ci, (NetGraph[]) gates.toArray(new NetGraph[0]), cfp, cad);
    }

    /** Group rules together by declared exclusion directives. */
    private class PartitionProductionRulesByExclusives
        implements Partition.PartitionerInterface {
        private final ExclusiveNodeSets ex;
        public PartitionProductionRulesByExclusives(ExclusiveNodeSets ex) {
            this.ex = ex;
        }
        /**
         * @bug kiniry 23 July 2002 - <a
         * href="http://internal/bugzilla/show_bug.cgi?id=1174">Bug#1174</a>
         */
        public boolean equivalent(Object o1, Object o2) {
            ProductionRule p1 = (ProductionRule) o1;
            ProductionRule p2 = (ProductionRule) o2;
            int d1 = p1.getDirection();
            int d2 = p2.getDirection();
            if (d1 != d2) { return false; }
            HierName h1 = (HierName) p1.getTarget();
            HierName h2 = (HierName) p2.getTarget();
            if (h1.equals(h2)) { return true; }
            Set s = new TreeSet();
            s.add(h1);
            s.add(h2);
            if (d1 == ProductionRule.UP) {
                return ex.areExclusive(ExclusiveNodeSet.HI, s);
            }
            if (d1 == ProductionRule.DOWN) {
                return ex.areExclusive(ExclusiveNodeSet.LO, s);
            }
            Debug.assertTrue(false, "illegal production rule direction");
            return false;
        }
    }

    /** Mark all port nodes.  Must call before adding edges to NetGraph. */
    public void markPortNodes(CellInterface ci) {
        final Map ports = CellUtils.markPorts(ci);
        Iterator i = ports.keySet().iterator();
        while (i.hasNext()) {
            String str  = (String) i.next();
            try {
                HierName name = HierName.makeHierName(str,'.');
                NetNode  node = createNetNode(name);
                node.is_port = true;
            } catch (InvalidHierNameException e) {
                throw (IllegalArgumentException)
                    new IllegalArgumentException("Bad HierName " + str).initCause(e);
            }
        }
    }

    /** Mark all port nodes.  Must call before adding edges to NetGraph. */
    public void markPortNodes(AspiceFile aspiceCell) {
        Iterator i = aspiceCell.getPortList().iterator();
        while (i.hasNext()) {
            HierName name = (HierName) i.next();
            NetNode  node = createNetNode(name);
            node.is_port = true;
        }
    }

    /** Find outputs, inverters, paths, and unused edges.  Needed for Lvs. */
    public void prepareForLvs() {
        markOutputs(); // fill in output
        findInverters(); // fill in inverseOf
        findPaths(); // make sure to find inverters first
        checkUnusedEdges(); // report any transistors that aren't used in NetPaths
        if (verbose) System.err.println(this);
    }
  
    /** Add a conjunctive production rule to this NetGraph. */
    private void addConjunctiveTerm(List gates, HierName target, int type,
                                    int fold, double width, double length,
                                    int transistorType) {
        NetNode S,D;
        if (type == DeviceTypes.P_TYPE) S = Vdd;
        else {Debug.assertTrue(type == DeviceTypes.N_TYPE); S = GND;}
        for (Iterator t = gates.iterator(); t.hasNext(); ) {
            NetNode G=createNetNode((HierName) t.next());
            if (t.hasNext()) {D = new NetNode(); nodes.add(D);}
            else {D = createNetNode(target); D.is_port = true;}
            NetEdge edge = 
                new NetEdge(S,G,D,type,fold,width,length,false,transistorType);
            S = D;
        }
    }

    /**
     * Add a conjunctive production rule to this NetGraph, sharing up
     * from the power rails.
     *
     * To be a valid graph, there must be no other targets in the graph that aren't
     * exclusive.
     **/
    private void addConjunctiveTermShared(List gates, HierName target, int type,
                                          int fold, double width, double length,
                                          int transistorType) {
        NetNode S;
        Iterator i = gates.iterator();
        if (type == DeviceTypes.P_TYPE) S = Vdd;
        else {Debug.assertTrue(type == DeviceTypes.N_TYPE); S = GND;}
        Debug.assertTrue(i.hasNext());
        addConjunctiveTermSharedRecursive(S, i, target, type, fold, width,
                                          length, transistorType);
    }

    /** Implements addConjunctiveTermShared(). */
    private void addConjunctiveTermSharedRecursive(NetNode S, Iterator gates,
                                                   HierName target,
                                                   int type, int fold,
                                                   double width, double length,
                                                   int transistorType) {
        HierName gatename = (HierName) gates.next();
        NetNode G = createNetNode(gatename);

        if (gates.hasNext()) {
            NetEdge e = S.findCompatibleNetEdge(G, true, type, fold);
            if (e != null) {
                NetNode D = e.drain;
                addConjunctiveTermSharedRecursive(D, gates, target,
                                                  type, fold, width, length,
                                                  transistorType);
            } else {
                NetNode D = new NetNode(); nodes.add(D);
                e = new NetEdge(S, G, D, type, fold, width, length, false,
                                transistorType);
                addConjunctiveTermSharedRecursive(D, gates, target, type, fold,
                                                  width, length, transistorType);
            }
        } else {
            // No more gates.  Need to stick target in.
            // Can't share with anyone.
            NetNode D = createNetNode(target); D.is_port = true;
            NetEdge e = new NetEdge(S, G, D, type, fold, width, length, false,
                                    transistorType);
        }
    }

    /**
     * Add all conjunctive terms from a production rule to this NetGraph.
     * Directly specifying a fold number other then those listed as
     * constants in NetEdge is discouraged.
     **/
    private void addProductionRule(ProductionRule prs,
                                   boolean reverseOrder, boolean skipOne,
                                   int fold, double width, double length,
                                   int transistorType, SymFullX symFullX) {
        int dir = prs.getDirection();
        HierName target = prs.getTarget();
        Collection disjuncts = prs.getGuard().DNFForm().getDisjuncts();
        for (Iterator i = disjuncts.iterator(); i.hasNext(); ) {
            List term = new ArrayList();
            Collection conjuncts = 
                ((AndBooleanExpressionInterface) i.next()).getConjuncts();
            if (symFullX == null || dir == ProductionRule.UP) {
                for (Iterator j = conjuncts.iterator(); j.hasNext(); ) {
                    HierNameAtomicBooleanExpression literal =
                        (HierNameAtomicBooleanExpression) j.next();
                    if (reverseOrder && skipOne) { // reverse all but first gate
                        if (term.size()==0) term.add(literal.getName());
                        else term.add(1,literal.getName());
                    }
                    else if (reverseOrder) term.add(0,literal.getName());
                    else term.add(literal.getName());
                }
            } else {
                symFullX.reorder(conjuncts, reverseOrder, term);
            }
            if (treeUnsafe) {
                addConjunctiveTerm(term,target,dir == ProductionRule.DOWN ? 
                                   DeviceTypes.N_TYPE : DeviceTypes.P_TYPE,
                                   fold, width, length, transistorType);
            } else {
                addConjunctiveTermShared(term,target,dir == ProductionRule.DOWN ? 
                                         DeviceTypes.N_TYPE : DeviceTypes.P_TYPE,
                                         fold, width, length, transistorType);
            }
        }
    }

    /** Simpler method to add all conjunctive terms with default parameters. */
    private void addProductionRule(ProductionRule prs, double width, double length) {
        addProductionRule(prs,false,false,0,width,length,0,null);
    }

    /** Get all nodes of this NetGraph. */
    public Collection<NetNode> getNodes() {
        return nodes;
    }

    /** Get output nodes of this NetGraph. */
    public Collection<NetNode> getOutputNodes() {
        return nodes.stream().filter(NetNode::isOutput)
                             .collect(Collectors.toList());
    }

    /** Get staticizer nodes of this NetGraph. */
    public Collection<NetNode> getStaticizerNodes() {
        return nodes.stream().filter(NetNode::isStaticizerInverter)
                             .collect(Collectors.toList());
    }

    /** Get all edges of this NetGraph. */
    public Collection<NetEdge> getEdges() {
        ArrayList<NetEdge> edges = new ArrayList<>();
        for (NetNode node : nodes) {
            for (Iterator s = node.edges.iterator(); s.hasNext(); ) {
                NetEdge edge = (NetEdge) s.next();
                if (edge.source == node) edges.add(edge); // don't double report
            }
        }
        return edges;
    }

    /** Get all paths of this NetGraph. */
    Collection<NetPath> getPaths() {
        ArrayList<NetPath> paths = new ArrayList<>();
        for (NetNode node : nodes) {
            for (Iterator s = node.paths.iterator(); s.hasNext(); ) 
                paths.add((NetPath) s.next());
        }
        return paths;
    }

    /** Return a string of CDL format edges. */
    public String cdlString() {
        int i=0;
        String s = "";
        Iterator t = getEdges().iterator();
        while (t.hasNext()) {
            NetEdge edge = (NetEdge) t.next();
            NetNode bulk = edge.type == DeviceTypes.N_TYPE ? GND : Vdd;
            s += "M" + i + 
                " " + edge.source.name +
                " " + edge.gate.name + 
                " " + edge.drain.name + 
                " " + bulk.name +
                " " + (edge.type == DeviceTypes.N_TYPE ? "N" : "P") +
                " W=" + edge.width + 
                " L=" + edge.length +
                "\n";
            i++;
        }
        return s;
    }

    /** Return a string of Skill format edges. */
    public String skillString() {
        int i = 0;
        String s = "";
        Iterator t = getEdges().iterator();
        while (t.hasNext()) {
            NetEdge edge = (NetEdge) t.next();
            if (!edge.library || edge.floating) s += "  " + edge.skillString(i);
            i++;
        }
        i = 0;
        t = getNodes().iterator();
        while (t.hasNext()) {
            NetNode node = (NetNode) t.next();
            if (node.gate != null) s += "  " + node.gate.skillString(i++);
            if (node.staticizer != null) s += "  " + node.staticizer.skillString(i++);
        }
        return s;
    }

    /** Check for new paths created by merging. */
    boolean hasMorePaths() {
        Iterator t = nodes.iterator();
        while (t.hasNext()) {
            NetNode node = (NetNode) t.next();
            if (node.output) if (node.hasMorePaths()) return true;
        }
        return false;
    }

    /** If possible, merge nodes N1 and N2 and edges E1 and E2. */
    boolean mergeNodes(NetNode N1, NetNode N2, NetEdge E1, NetEdge E2) {
        // trivial reject cases
        if (N1.output || N2.output || N1.isRail() || N2.isRail()) return false;
        if (E1.getTransistorType() != E2.getTransistorType()) return false;

        // save node.oldEdges, create OldEdges list, clear node.edges
        Collection OldEdges = getEdges();
        for (Iterator t = nodes.iterator(); t.hasNext(); ) {
            NetNode node = (NetNode) t.next();
            node.oldEdges = node.edges;
            node.edges = new MultiSet();
        }

        // construct new edges from OldEdges with merging
        for (Iterator t = OldEdges.iterator(); t.hasNext(); ) {
            NetEdge edge = (NetEdge) t.next();
            NetNode G = (edge.gate   == N2) ? N1 : edge.gate;
            NetNode S = (edge.source == N2) ? N1 : edge.source;
            NetNode D = (edge.drain  == N2) ? N1 : edge.drain;
            if (edge == E2); // omit this edge
            else if (edge == E1) { // merged version of E1 and E2
                double width = (E1.width > E2.width) ? E1.width : E2.width;
                new NetEdge(S,G,D,edge.type,edge.fold,width,edge.length, false,
                            edge.getTransistorType());
            }
            else new NetEdge(S,G,D,edge.type,edge.fold,
                             edge.width,edge.length,edge.library,
                             edge.getTransistorType());
        }
        if (N1 == N2) {
            // successful merge
            if (verbose) System.err.println("auto-merge of " + N1.name);
            return true;
        }
        
        // restore saved oldEdges of nodes if any new paths created by merge
        if (hasMorePaths()) {
            for (Iterator t = nodes.iterator(); t.hasNext(); ) {
                NetNode node = (NetNode) t.next();
                node.edges=node.oldEdges;
            }
            if (verbose) System.err.println("Failed to Merge " + N1.name + 
                                            " and " + N2.name);
            return false;
        }

        // successful merge
        if (verbose) System.err.println("Merged " + N1.name + " and " + N2.name);
        return true;
    }

    /** 
     * Merge this NetGraph into a minimal gate network. 
     *
     * @bug kiniry 23 July 2002 - <a
     * href="http://internal/bugzilla/show_bug.cgi?id=1174">Bug#1174</a>
     */
    private void minimize() {
        boolean progress;
        do {
            progress=false;
            Iterator t = nodes.iterator();
            while (t.hasNext()) {
                NetNode node = (NetNode) t.next();
                progress |= mergeEdges(node);
            }
        } while (progress);
    }

    /**
     * Helper for minimize().
     *
     * merges all compatible edges from node.
     *
     * @bug kiniry 23 July 2002 - <a
     * href="http://internal/bugzilla/show_bug.cgi?id=1174">Bug#1174</a> 
     **/
    private boolean mergeEdges(NetNode node) {
        boolean progress;
        boolean merged = false;
        do {
            progress = false;
            node.edges.sort();
            NetEdge[] edges = new NetEdge[node.edges.size()];
            Iterator s = node.edges.iterator();
            for (int i=0; i<node.edges.size(); i++) edges[i]=(NetEdge) s.next();
            // look for two edges with same gate
            out:
            for (int i=0; i<edges.length-1; i++) {
                for (int j=i+1; j<edges.length; j++) {
                    NetEdge E1=edges[i], E2=edges[j];
                    int order = E1.gate.compareTo(E2.gate);
                    // sorted list, by gate first.
                    // if E1 is less than E2
                    // It will be less than any higher E2.
                    if (order<0) break; // start over with new E1.
                    Debug.assertTrue(order==0); // E1 should not be greater than E2.
                    if (E1.fold!=E2.fold) continue;
                    if (E1.type!=E2.type) continue;
                    if (E1.library || E2.library) continue;
                    // find nodes via edges
                    NetNode N1 = (E1.source == node) ? E1.drain : E1.source;
                    NetNode N2 = (E2.source == node) ? E2.drain : E2.source;
                    // merge nodes if possible
                    if (mergeNodes(N1,N2,E1,E2)) {
                        merged = progress = true; break out; // restart do.
                    }
                }
            }
        } while (progress);
        return merged;
    }

    /** Find all inverters in this NetGraph. */
    public void findInverters() {
        Iterator t = nodes.iterator();
        while (t.hasNext()) {
            ((NetNode) t.next()).findInverter();
        }
    }

    /** Mark all operator output nodes in this NetGraph. */
    public void markOutputs() {
        Iterator t = nodes.iterator();
        while (t.hasNext()) {
            ((NetNode) t.next()).markOutput();
        }
    }

    /** 
     * Find all paths for output NetNodes and mark used edges.
     *
     * @bug kiniry 23 July 2002 - <a
     * href="http://internal/bugzilla/show_bug.cgi?id=1174">Bug#1174</a> 
     */
    public void findPaths() {
        for (Iterator s = getEdges().iterator(); s.hasNext(); ) {
            NetEdge edge = (NetEdge) s.next();
            edge.used = false;
        }
        for (Iterator t = nodes.iterator(); t.hasNext(); ) {
            NetNode node = (NetNode) t.next();
            if (node.output) {
                node.findPaths();
                for (Iterator j = node.paths.iterator(); j.hasNext(); ) {
                    NetPath path = (NetPath) j.next();
                    path.markUsedEdges();
                }
            }
        }
    }

    /** Check for any unused edges. */
    void checkUnusedEdges() {
        for (Iterator t = nodes.iterator(); t.hasNext(); ) {
            NetNode node = (NetNode) t.next();
            for (Iterator j = node.edges.iterator(); j.hasNext(); ) {
                NetEdge edge = (NetEdge) j.next();
                if ((edge.source == node)&&(!edge.used))
                    problems.add(new LVSProblem("Unused edge " + edge));
            }
        }
    }

    /** Find a NetNode by name or null if none found. */
    public NetNode findNetNode(HierName name) {
        HierName canon = null;
        if (namespace != null) canon = (HierName) namespace.getCanonicalKey(name);
        if (canon == null) canon = name;
        return (NetNode) nodes.find(new NetNode(canon));
    }

    /** Find or create a NetNode by name. */
    public NetNode createNetNode(HierName name) {
        HierName canon = null;
        if (namespace != null) canon = (HierName) namespace.getCanonicalKey(name);
        if (canon == null) canon = name;
        NetNode node = new NetNode(canon);
        NetNode oldnode = (NetNode) nodes.find(node);
        if (oldnode != null) return oldnode;
        nodes.add(node);
        return node;
    }

    /** 
     * Find or create a NetNode given another node and a name mapping.
     * Add new mappings from unmapped or unnamed nodes to unnamed
     * nodes.
     **/
    public NetNode mapNetNode(HierName name, TreeMap map) {
        NetNode node;
        if (map.get(name)!=null) // already in the map
            node = createNetNode((HierName) map.get(name));
        else { // map to a new unnamed node, add to the map
            node = new NetNode();
            nodes.add(node);
            map.put(name,node.name);
        }
        return node;
    }

    /** Get a list of operator output nodes in this NetGraph. */
    public ArrayList getUnmatchedOperatorNodes() {
        ArrayList outputs = new ArrayList();
        for (Iterator t = nodes.iterator(); t.hasNext(); ) {
            NetNode node = (NetNode) t.next();
            if (node.output && !node.isStaticizerInverter() && node.gate==null)
                outputs.add(node);
        }
        return outputs;
    }

    /** Try to map one name into another.  Update mapping if possible. */
    private static boolean tryMap(HierName from, HierName to, TreeMap map) {
        if (map.get(from) == null) map.put(from,to); // create new mapping
        else if (((HierName) map.get(from)).compareTo(to) != 0) return false;
        return true;
    }

    /** Match two paths. */
    private static TreeMap matchPath(NetPath path, NetPath gpath, TreeMap map) {
        // match type and feedback
        if (gpath.type != path.type) return null;
        if (gpath.feedback != path.feedback) return null;

        // match to and from
        map = new TreeMap(map); // copy original
        if (!tryMap(gpath.to.name,path.to.name,map)) return null;
        if (!tryMap(gpath.from.name,path.from.name,map)) return null;

        // match all gates in order
        ArrayList gates  = path.getGatesArrayList();
        ArrayList ggates = gpath.getGatesArrayList();
        if (ggates.size() != gates.size()) return null; // mismatch
        if (ggates.size() == 0) return map; // nothing left to match
        for (int i=0; i<gates.size(); i++) {
            if (!tryMap((HierName) ggates.get(i),
                        (HierName) gates.get(i),
                        map)) return null;
        }
        return map; // found complete match
    }

    /** Match lists of logic paths recursively. */
    private static TreeMap matchPaths(ArrayList paths,
                                      ArrayList gpaths,
                                      TreeMap oldmap) {
        if (gpaths.size() != paths.size()) return null; // wrong number
        if (gpaths.size() == 0) return oldmap; // nothing left to match
        NetPath gpath = (NetPath) gpaths.get(0); // get first path
        for (int i=0; i<paths.size(); i++) {
            NetPath path = (NetPath) paths.get(i);
            TreeMap map = matchPath(path,gpath,oldmap);
            if (map == null) continue; // no match yet
            ArrayList newpaths  = new ArrayList(paths);
            newpaths.remove(i); // remove matched path
            ArrayList newgpaths = new ArrayList(gpaths);
            newgpaths.remove(0); // remove first path
            map = matchPaths(newpaths,newgpaths,map);
            if (map != null) return map; // found a complete match
        }
        return null; // no match
    }

    /** Match list of operators in gateGraph recursively. */
    private static TreeMap matchOutputs(ArrayList outputs,
                                        ArrayList goutputs,
                                        TreeMap oldmap) {
        if (goutputs.size() == 0) return oldmap; // none left to match
        NetNode gnode = (NetNode) goutputs.get(0); // get first output
        for (int i=0; i<outputs.size(); i++) {
            NetNode node = (NetNode) outputs.get(i);
            TreeMap map = matchPaths(node.getLogicPaths(),
                                     gnode.getLogicPaths(),oldmap);
            if (map == null) continue; // no match yet
            ArrayList newoutputs  = new ArrayList(outputs);
            newoutputs.remove(i); // remove matched output
            ArrayList newgoutputs = new ArrayList(goutputs);
            newgoutputs.remove(0); // remove first output
            map = matchOutputs(newoutputs,newgoutputs,map);
            if (map != null) return map; // found a complete match
        }
        return null; // no match
    }
    
    /** Match a subset of the NetGraph against a library NetGraph.
     *  Return new name mapping or null.  Must findPaths first.*/
    public TreeMap matchGate(NetGraph gateGraph) {

        // initial mapping
        final TreeMap map = new TreeMap();
        initializeMatchMapping(map, gateGraph);
        
        // match output nodes gateGraph vs this NetGraph
        ArrayList outputs = getUnmatchedOperatorNodes();
        ArrayList goutputs = gateGraph.getUnmatchedOperatorNodes();
        return matchOutputs(outputs, goutputs, map);
    }

    /** 
     * Replace subsets of NetGraph which a match a library NetGraph.
     * Must findPaths first.
     */
    public void substituteGate(final NetGraph gateGraph) {
        substituteGate(gateGraph, new UnaryFunction() {
            public Object execute(final Object o) {
                return gateGraph;
            }
        });
    }

    /**
     * Match against <code>gateGraph</code>, and replace edges that matches
     * with edges from the netgraph returned by call <code>transform</code>.
     **/
    private void substituteGate(NetGraph gateGraph, UnaryFunction transform) {
        while (true) {
            final TreeMap map = matchGate(gateGraph);
            if (map == null) break;
            final NetGraph replacement = (NetGraph) transform.execute(map);
            final GateInstance inst = new GateInstance(map, replacement);
            ArrayList goutputs = gateGraph.getUnmatchedOperatorNodes();
            for (int i=0; i<goutputs.size(); i++) {
                NetNode gnode = (NetNode) goutputs.get(i);
                NetNode node = mapNetNode(gnode.name,map);
                node.deleteEdges(); // delete old edges
                node.gate = inst;
            }
            addNetGraph(replacement,map,true); // add gateGraph
        }
    }

    /**
     * Add mappings for the power rails names.  Only power rails used will be
     * included in the mapping.
     *
     * @param map mapping from names of power rails in <code>gate</code> to the
     * names of power rails in this NetGraph
     * @param gate NetGraph to establish mapping for
     **/
    private void initializeMatchMapping(final Map map, final NetGraph gate) {
        boolean n = false, p = false;
        for (Iterator i = gate.getEdges().iterator();
             i.hasNext() && !(n && p); ) {
            final NetEdge edge = (NetEdge) i.next();
            if (edge.source.isGND() || edge.gate.isGND() ||
                edge.drain.isGND() || edge.type == DeviceTypes.N_TYPE) n = true;
            if (edge.source.isVdd() || edge.gate.isVdd() ||
                edge.drain.isVdd() || edge.type == DeviceTypes.P_TYPE) p = true;
        }
        if (n) map.put(gate.GND.name, GND.name);
        if (p) map.put(gate.Vdd.name, Vdd.name);
    }

    /**
     * Attach a full staticizer to a node.
     *
     * @param node node to attached the staticizer to
     * @param staticizer netgraph of the staticizer gate
     **/
    private void addFullStaticizer(final NetNode node,
                                   final NetGraph staticizer) {
        final TreeMap map = new TreeMap();
        initializeMatchMapping(map, staticizer);
        map.put(HierName.makeHierName("out"), node.name);
        node.staticizer = new GateInstance(map, staticizer);
        addNetGraph(staticizer, map, true);
    }

    /**
     * Attach a weak inverter staticizer to a node.
     *
     * @param node node to attached the weak inverter to
     * @param weakInv netgraph of the weak inverter gate
     **/
    private void addWeakInv(final NetNode node, final NetGraph weakInv) {
        final TreeMap map = new TreeMap();
        initializeMatchMapping(map, weakInv);
        map.put(HierName.makeHierName("in"),node.inverse.name);
        map.put(HierName.makeHierName("out"),node.name);
        node.staticizer = new GateInstance(map, weakInv);
        addNetGraph(weakInv,map,true);
    }
    
    /** Add small inverter */
    private void addSmallInverter(NetNode node, NetGraph smallInv) {
        node.inverse = createNetNode(node.name.appendString("_inverse"));
        final TreeMap map = new TreeMap();
        initializeMatchMapping(map, smallInv);
        map.put(HierName.makeHierName("a"),node.name);
        map.put(HierName.makeHierName("out"),node.inverse.name);
        node.inverse.gate = new GateInstance(map, smallInv);
        addNetGraph(smallInv,map,true);
    }

    /**
     * Add a combinational staticizer, plus a small inverter if one
     * doesn't already exist.  Can do half-combinational, half-weak
     * staticizers.
     *
     * @param node node to attached the weak inverter to
     * @param smallInv netgraph of the small inverter gate
     * @param weakUp should the holdDn be weak?
     * @param weakDn should the holdUp be weak?
     * @param combUp should the holdDn be combinational?
     * @param combDn should the holdUp be combinational?
     * @param should we add smallInv even if inverter exists?
     * @param ttup transistor type for PMOS
     * @param ttdn transistor type for NMOS
     **/
    private void addCombinationalStaticizer(final NetNode node,
                                            final NetGraph smallInv,
                                            final boolean weakUp,
                                            final boolean weakDn,
                                            final boolean combUp,
                                            final boolean combDn,
                                            final boolean addInv,
                                            final int ttup,
                                            final int ttdn) {
        // skip if neither side wants feedback
        if (!weakUp && !weakDn && !combUp && !combDn) return;

        // deduce logic from NetNode
        MultiSet netUp  = new MultiSet();
        MultiSet netDn  = new MultiSet();
        MultiSet holdUp = new MultiSet();
        MultiSet holdDn = new MultiSet();
        Dnf.fromNetNode(node,netUp,netDn,holdUp,holdDn);

        // abort if feedback paths already exist
        if (holdUp.size()!=0 || holdDn.size()!=0) {
            System.err.println("ERROR: node " + node.name + " already has feedback.");
            System.exit(1);
        }
        
        // find canonical duals of netUp and netDn, or empty term for weak staticizer
        if (combDn) {
            holdDn = Dnf.dual(netUp);
            holdDn = Dnf.getCanonicalForm(holdDn,ProductionRule.DOWN,exclusives);
            holdDn = Dnf.eliminateRedundantTerms(holdDn,netDn);
        } else {
            holdDn = new MultiSet(); holdDn.add(new MultiSet());
        }
        if (combUp) {
            holdUp = Dnf.dual(netDn);
            holdUp = Dnf.getCanonicalForm(holdUp,ProductionRule.UP,exclusives);
            holdUp = Dnf.eliminateRedundantTerms(holdUp,netUp);
        } else {
            holdUp = new MultiSet(); holdUp.add(new MultiSet());
        }

        // construct temporary NetGraph
        NetGraph ng = new NetGraph(namespace, exclusives, new ArrayList(),
                                   Vdd.name, GND.name, 
                                   defaultWidth, defaultLength, Collections.EMPTY_SET);
        
        // add a small inverter if one doesn't exist
        TreeMap node_map = new TreeMap(); // needed to map inverse node
        if (addInv || node.inverse==null) addSmallInverter(node,smallInv);

        // add the holdUp and holdDn transistors
        double width  = defaultWidth;
        double length = defaultLength;
        int fold = NetEdge.DEFAULT_FOLD;
        if (weakDn || combDn) for (Iterator i = holdDn.iterator(); i.hasNext(); ) {
            List term = new ArrayList(((MultiSet) i.next()).list());
            term.add(node.inverse.name); // append the feedback transistor
            ng.addConjunctiveTermShared(term,node.name,DeviceTypes.N_TYPE,fold,
                                        width,length,ttdn);
        }
        if (weakUp || combUp) for (Iterator i = holdUp.iterator(); i.hasNext(); ) {
            List term = new ArrayList(((MultiSet) i.next()).list());
            term.add(node.inverse.name); // append the feedback transistor
            ng.addConjunctiveTermShared(term,node.name,DeviceTypes.P_TYPE,fold,
                                        width,length,ttup);
        }
        
        // minimize and merge into main NetGraph
        ng.minimize();
        mergeFrom(ng,node_map);
    }

    /** Find a vanBerkel version of a gate with a staticizer */
    private String vanBerkelGate(final NetGraph [] gates, String gateType) {
        // TODO: query van_berkel directive
        if (gateType.startsWith("gate.C2.")) {
            return gateType.replaceAll("gate\\.C2\\.","gate.C2_STAT.");
        } else if  (gateType.startsWith("gate.2AND2C2.")) {
            return gateType.replaceAll("gate\\.2AND2C2\\.","gate.2AND2C2_STAT.");
        } else if  (gateType.startsWith("gate.AND2C2.")) {
            return gateType.replaceAll("gate\\.AND2C2\\.","gate.AND2C2_STAT.");
        } else if  (gateType.startsWith("gate.AND2C2_ALT.")) {
            return gateType.replaceAll("gate\\.AND2C2_ALT\\.","gate.AND2C2_STAT.");
        } else if (gateType.startsWith("gate.C2_RHI.")) {
            return gateType.replaceAll("gate\\.C2_RHI\\.","gate.C2_RHI_STAT.");
        } else if  (gateType.startsWith("gate.2AND2C2_RHI.")) {
            return gateType.replaceAll("gate\\.2AND2C2_RHI\\.","gate.2AND2C2_RHI_STAT.");
        } else if  (gateType.startsWith("gate.AND2C2_RHI.")) {
            return gateType.replaceAll("gate\\.AND2C2_RHI\\.","gate.AND2C2_RHI_STAT.");
        } else if  (gateType.startsWith("gate.AND2C2_RHI_ALT.")) {
            return gateType.replaceAll("gate\\.AND2C2_RHI_ALT\\.","gate.AND2C2_RHI_STAT.");
        }
        return null;
    }

    /** Replace gate with vanBerkel gate if possible */
    private boolean vanBerkel(final NetGraph [] gates,
                              final NetNode node,
                              final NetGraph smallInv,
                              final boolean addInv,
                              final CastFileParser cfp,
                              final Cadencize cad) {
        if (node.gate==null) return false;
        String vb = vanBerkelGate(gates,node.gate.type);
        if (vb==null) return false;
        try {
            NetGraph vbGraph = CastDesign.getGateNetGraph(cfp,vb,Vdd.name,GND.name,cad);
            if (addInv || node.inverse==null) addSmallInverter(node,smallInv);
            TreeMap map = new TreeMap(node.gate.map);
            map.put(HierName.makeHierName("c"),node.inverse.name); // TODO: directive
            node.deleteEdges(); // delete old edges
            node.gate = new GateInstance(map,vbGraph);
            addNetGraph(vbGraph,map,true);
            return true;
        } catch (Exception e) {
            System.err.println("ERROR: unable to parse van-berkel gate " + vb);
            return false;
        }
    }

    /**
     * Attach a full staticizer, weak inverter, or combinational
     * staticizer to a node.  Chooses based on staticizer_type
     * directives and availabibility of 3 special gates.
     *
     * @param node node to attached the staticizer to
     * @param staticizerGraph netgraph of the full staticizer gate
     * @param weakInverterGraph netgraph of the weak inverter gate
     * @param stup the staticizer type declared for the pull up
     * @param stdn the staticizer type declared for the pull down
     * @param ttup the transistor type declared for the pull up
     * @param ttdn the transistor type declared for the pull down
     **/
    private void addStaticizer(final NetGraph[] gates,
                               final NetNode node,
                               final NetGraph staticizerGraph,
                               final NetGraph weakInverterGraph,
                               final NetGraph smallInverterGraph,
                               final int stup,
                               final int stdn,
                               final int ttup,
                               final int ttdn,
                               final CastFileParser cfp,
                               final Cadencize cad) {
        boolean weakUp = Staticizer.isWeakFeedback(stup);
        boolean weakDn = Staticizer.isWeakFeedback(stdn);
        boolean combUp = Staticizer.isCombinationalFeedback(stup);
        boolean combDn = Staticizer.isCombinationalFeedback(stdn);
        boolean autoUp = Staticizer.isAutomaticFeedback(stup);
        boolean autoDn = Staticizer.isAutomaticFeedback(stdn);
        boolean addInv = Staticizer.addSmallInverter(stup) || Staticizer.addSmallInverter(stdn);
        if (Staticizer.isBadStaticizerType(stup)) {
            System.err.println("ERROR: bad staticizer_type=" + stup + 
                               " on " + node.name + "+");
        }
        if (Staticizer.isBadStaticizerType(stdn)) {
            System.err.println("ERROR: bad staticizer_type=" + stdn + 
                               " on " + node.name + "-");
        }
        if (autoUp && autoDn) {
            if (vanBerkel(gates,node,smallInverterGraph,addInv,cfp,cad)) return;
        }
        if (autoUp || autoDn) {
            // deduce logic from NetNode
            MultiSet netUp  = new MultiSet();
            MultiSet netDn  = new MultiSet();
            MultiSet holdUp = new MultiSet();
            MultiSet holdDn = new MultiSet();
            Dnf.fromNetNode(node,netUp,netDn,holdUp,holdDn);
            // choose COMB_STATICIZER when opposing logic is straight chain of <=3 depth
            if (autoUp) {
                combUp = netDn.size()==1 && ((MultiSet) netDn.get(0)).size()<=3;
                weakUp = !combUp;
                if (verbose) 
                    System.err.println("NOTE: chose " +
                                       (combUp ? "COMB_STATICIZER" : "WEAK_STATICIZER") +
                                       " for " + node.name + "+");
            }
            if (autoDn) {
                combDn = netUp.size()==1 && ((MultiSet) netUp.get(0)).size()<=3;
                weakDn = !combDn;
                if (verbose)
                    System.err.println("NOTE: chose " +
                                       (combDn ? "COMB_STATICIZER" : "WEAK_STATICIZER") +
                                       " for " + node.name + "-");
            }
        }
        if (weakUp && weakDn && !addInv && (node.inverse!=null) && (weakInverterGraph!=null))
            addWeakInv(node, weakInverterGraph);
        else if (weakUp && weakDn && (staticizerGraph!=null))
            addFullStaticizer(node, staticizerGraph);
        else if ((node.inverse!=null) || (smallInverterGraph!=null))
            addCombinationalStaticizer(node,smallInverterGraph,
                                       weakUp,weakDn,combUp,combDn,addInv,ttup,ttdn);
        else {
            System.err.println("ERROR: node " + node + " needs small-inverter.");
            System.exit(1);
        }
    }

    /** 
     * Add staticizers to output nodes that need one.  If
     * <code>addToGate</code> is true, then staticizers will be added
     * to nodes even if they have been matched by a gate.  Otherwise,
     * staticizers are only added to nodes that are has not been
     * matched by a gate.  This is dependent on whether the gate
     * library includes staticizers.
     */
    public void addStaticizers(final NetGraph[] gates,
                               final NetGraph staticizerGraph,
                               final NetGraph weakInverterGraph,
                               final NetGraph smallInverterGraph,
                               final boolean addToGate,
                               final CellInterface ci,
                               final CastFileParser cfp,
                               final Cadencize cad) {

        // make list of nodes to staticize
        List opnodes = new ArrayList();
        for (Iterator i = nodes.iterator(); i.hasNext(); ) {
            NetNode node = (NetNode) i.next();
            if (!node.output) continue;
            if (!addToGate && node.gate!=null) continue;
            if (node.isCombinational()) continue;
            if (nostaticizers.contains(node.name)) continue;
            opnodes.add(node);
        }
        if (opnodes.size()==0) return; // nothing to staticize

        // query transistor type directives
        Map ttype = DirectiveUtils.getPrsDirective(ci,
                DirectiveConstants.TRANSISTOR_TYPE,
                DirectiveConstants.HALFOP_TYPE);
        final Map ttup = DirectiveUtils.canonizeKey(namespace,
                DirectiveUtils.getUps(ttype));
        final Map ttdn = DirectiveUtils.canonizeKey(namespace,
                DirectiveUtils.getDowns(ttype));
        final Integer ctt = (Integer) DirectiveUtils.getTopLevelDirective(ci,
                DirectiveConstants.TRANSISTOR_TYPE);

        // query staticizer type directives
        Map statType = DirectiveUtils.getPrsDirective(ci,
                DirectiveConstants.STATICIZER_TYPE,
                DirectiveConstants.HALFOP_TYPE);
        final Map statup = DirectiveUtils.canonizeKey(namespace,
                DirectiveUtils.getUps(statType));
        final Map statdn = DirectiveUtils.canonizeKey(namespace,
                DirectiveUtils.getDowns(statType));
        Integer cstatt = (Integer) DirectiveUtils.getTopLevelDirective(ci,
                DirectiveConstants.STATICIZER_TYPE);
        if (cstatt == null) cstatt = new Integer(0);

        for (Iterator i = opnodes.iterator(); i.hasNext(); ) {
            NetNode node = (NetNode) i.next();
            if (node.staticizer!=null) return; // already there, I wonder why?
            final List l = Collections.singletonList(node.name);
            final NetGraph staticizer =
                getMultipleTransistorGate(staticizerGraph, l.iterator(),
                                          ttup, ttdn, ctt, cfp, cad);
            final NetGraph weakInv =
                getMultipleTransistorGate(weakInverterGraph, l.iterator(),
                                          ttup, ttdn, ctt, cfp, cad);
            final NetGraph smallInv = 
                getMultipleTransistorGate(smallInverterGraph, null,
                                          null, null, ctt, cfp, cad);
            addStaticizer(gates, node, staticizer, weakInv, smallInv,
                          getHalfOpDirective(statup, node.name, cstatt),
                          getHalfOpDirective(statdn, node.name, cstatt),
                          getHalfOpDirective(ttup, node.name, ctt),
                          getHalfOpDirective(ttdn, node.name, ctt),
                          cfp,cad);
        }
    }

    /**
     * Imply exlchi/excllo statements for every inverter in/out.  Used
     * by CDL2Cast to eliminate transient interfering paths.
     **/
    private void implyExclusiveInverters() {
        for (Iterator t = nodes.iterator(); t.hasNext(); ) {
            NetNode node = (NetNode) t.next();
            if (node.inverseOf==null) continue;
            Collection n = new ArrayList();
            n.add(node.name);
            n.add(node.inverseOf.name);
            ExclusiveNodeSet exclhi = 
                new ExclusiveNodeSet(ExclusiveNodeSet.HI, n);
            ExclusiveNodeSet excllo =
                new ExclusiveNodeSet(ExclusiveNodeSet.LO, n);
            exclusives.addExclusiveNodeSet(exclhi);
            exclusives.addExclusiveNodeSet(excllo);
        }
    }
    
    /**
     * Return a ProductionRuleSet generated from the NetGraph.  Used
     * by CDL2Cast to generate simulatable PRS for standard gates or
     * licensed cores.  Handles pass gates, as long as you pull to
     * correct rail through proper transistor type.
     *
     * Non-combinational gates are automatically strengthened to avoid
     * tranisent interference, such as an XOR gate using a and _a.
     **/
    public ProductionRuleSet getProductionRuleSet() {
        assumeStrongInverters = true;
        implyExclusiveInverters(); // imply that inverters are exclusive
        prepareForLvs(); // find paths again with new exclusions
        assumeStrongInverters = false;
        ProductionRuleSet prs = new ProductionRuleSet();

        // map nodes to the root of an a driving inverter chain, if any
        TreeMap inverter_map = new TreeMap(); 
        for (Iterator t = nodes.iterator(); t.hasNext(); ) {
            NetNode node = (NetNode) t.next();
            if (node.inverseOf!=null) {
                inverter_map.put(node.name,node.inverseOf.name);
            } else if (node.interferingInverseOf!=null) {
                inverter_map.put(node.name,node.interferingInverseOf.name);
            }
        }

        // translate each output node into PRS
        for (Iterator t = nodes.iterator(); t.hasNext(); ) {
            NetNode node = (NetNode) t.next();
            if (!node.output) continue;
            if (node.isStaticizerInverter()) continue;
            
            // get dnf for this node
            MultiSet netUp = new MultiSet();
            MultiSet netDn = new MultiSet();
            MultiSet holdUp = new MultiSet();
            MultiSet holdDn = new MultiSet();
            Dnf.fromNetNode(node,netUp,netDn,holdUp,holdDn);

            // default prs parameters
            BooleanExpressionInterface guard;
            HierName target       = node.name;
            boolean isIsochronic  = false;
            boolean isUnstableP   = true; // assume its nasty stuff
            boolean isMetastableP = false;
            boolean isTimedP      = false;
            int after             = -1;
            boolean absolute      = false;
            HierName powerSupply;
            int direction;
            ProductionRule rule;
            
            // get DNF's (excluding paths which include both a and _a)
            netDn = Dnf.getCanonicalForm(netDn,ProductionRule.UP,exclusives);
            netUp = Dnf.getCanonicalForm(netUp,ProductionRule.DOWN,exclusives);

            // check for combinational logic
            boolean combinational = node.isCombinational();

            // remove potential interference: dn & ~up -> x-    up & ~dn -> x+
            if (!combinational) {
                MultiSet dualDn = Dnf.dual(netDn);
                MultiSet dualUp = Dnf.dual(netUp);
                netDn = Dnf.conjunct(netDn,dualUp);
                netUp = Dnf.conjunct(netUp,dualDn);
                netDn = Dnf.getCanonicalForm(netDn,ProductionRule.UP,exclusives);
                netUp = Dnf.getCanonicalForm(netUp,ProductionRule.DOWN,exclusives);
            }

            // add dn production rule
            guard = Dnf.getBooleanExpression(netDn,true,combinational ? null : inverter_map);
            powerSupply = GND.name;
            direction   = ProductionRule.DOWN;
            rule = new ProductionRule(guard,target,powerSupply,direction,
                                      isIsochronic,isUnstableP,isMetastableP,isTimedP,
                                      after,absolute);
            if (netDn.size()>0) prs.addProductionRule(rule);

            // add up production rule
            guard = Dnf.getBooleanExpression(netUp,false,combinational ? null : inverter_map);
            powerSupply = Vdd.name;
            direction   = ProductionRule.UP;
            rule = new ProductionRule(guard,target,powerSupply,direction,
                                      isIsochronic,isUnstableP,isMetastableP,isTimedP,
                                      after,absolute);
            if (netUp.size()>0) prs.addProductionRule(rule);
        }
        return prs;
    }

    /** Debugging string. */
    public String toString () {
        String s;
        s = "Nodes(" + nodes.size() + ")\n";
        for (Iterator t = nodes.iterator(); t.hasNext(); ) s += " " + t.next() + "\n";
        Collection edges = getEdges();
        s += "Edges(" + edges.size() + ")\n";
        for (Iterator t = edges.iterator(); t.hasNext(); ) s += " " + t.next() + "\n";
        Collection paths = getPaths();
        s += "Paths(" + paths.size() + ")\n";
        for (Iterator t = paths.iterator(); t.hasNext(); ) s += " " + t.next() + "\n";
        return s;
    }

    /**
     * Merges in another NetGraph.  Auto-generated nodes are renamed, and
     * unshared edges are reassigned fold numbers.
     *
     * Also handles partial symmetrization for any edges with PARTIAL_FOLD
     * fold_type.
     *
     * For this to make sense, both this netgraph and the other should
     * have the same exclusion rules and namespaces.
     **/
    public void mergeFrom(NetGraph ng, TreeMap node_map) {
        TreeMap sym_map = new TreeMap(); // map for partial symmetrization

        // copy nodes, renaming anonymous ones, and merging non-anonymous
        for (Iterator i = ng.nodes.iterator(); i.hasNext(); ) {
            NetNode n = (NetNode) i.next();
            if (!n.named && !n.isRail() && node_map.get(n.name)==null) {
                NetNode new_node = new NetNode();
                nodes.add(new_node);
                node_map.put(n.name, new_node.name);
                if (n.edges.size()==2) { // non-contacted internal node
                    new_node = new NetNode();
                    nodes.add(new_node);
                }
                sym_map.put(n.name,new_node.name);
            } else {
                node_map.put(n.name, n.name);
                sym_map.put(n.name, n.name);
                NetNode new_node = createNetNode(n.name);
                if (n.gate != null) {
                    Debug.assertTrue(new_node.gate == null,
                                     "two gates matched when merging.");
                    GateInstance gi = n.gate;
                    new_node.gate = 
                        new GateInstance(gi.type, gi.map, gi.gateGraph);
                }
            }
        }

        // copy edges, optionally symmetrizing
        for (Iterator i = ng.getEdges().iterator(); i.hasNext(); ) {
            NetEdge e = (NetEdge) i.next();
            boolean partialSymmetrize = (fold_type(e.fold) == NetEdge.PARTIAL_FOLD);
            boolean trunkSymmetrize   = (fold_type(e.fold) == NetEdge.TRUNK_FOLD);
            if (partialSymmetrize || trunkSymmetrize) { // create two symmetrized edges
                // create normal edge
                new NetEdge(mapNetNode(e.source.name, node_map),
                            mapNetNode(e.gate.name, node_map),
                            mapNetNode(e.drain.name, node_map),
                            e.type, e.fold, e.width/2, e.length,
                            e.library, e.getTransistorType());
                // s/d of symmetrized transistor
                NetNode s = e.source;
                NetNode d = e.drain;
                // identify s/d of the stack
                NetNode stackS = e.findStackSource();
                NetNode stackD = e.findStackDrain();
                // swap s/d if it matches the stack's s/d
                if (partialSymmetrize || stackS.isRail() || stackD.isRail()) {
                    if (s == stackS) s = stackD;
                    if (d == stackD) d = stackS;
                }
                // create symmetrized edge (reverse S/D order)
                new NetEdge(mapNetNode(d.name,sym_map),
                            mapNetNode(e.gate.name, sym_map),
                            mapNetNode(s.name,sym_map),
                            e.type, e.fold, e.width/2, e.length,
                            e.library, e.getTransistorType());
            } else { // create one edge
                new NetEdge(mapNetNode(e.source.name, node_map),
                            mapNetNode(e.gate.name, node_map),
                            mapNetNode(e.drain.name, node_map),
                            e.type, e.fold, e.width, e.length,
                            e.library, e.getTransistorType());
            }
        }
    }

    /** Normal usage of mergeFrom with an empty node_map **/
    public void mergeFrom(NetGraph ng) {
        TreeMap node_map = new TreeMap(); // HierName -> HierName
        mergeFrom(ng,node_map);
    }

    /**
     * Builds a map suitable for addNetGraph(); that can be used to add this
     * graph to another with a prefix of prefix.
     *
     * @param prefix The prefix to use.
     * @return The TreeMap built.
     *
     * @see #addNetGraph(NetGraph, TreeMap, boolean)
     **/
    public TreeMap constructPrefixMap(HierName prefix) {
        TreeMap rv = new TreeMap();

        for (Iterator i = nodes.iterator(); i.hasNext(); ) {
            NetNode n = (NetNode) i.next();
            if (n.name.isGenerated()) continue;  // they get remapped automatically.
            if (n.name.isGlobal()) {
                // Handled specially in LVS, so handled specially here.
                rv.put(n.name, n.name);
            } else {
                rv.put(n.name, HierName.append(prefix, n.name));
            }
        }
        return rv;
    }

    /** Are there any sneakpaths? */
    private boolean hasSneakPaths() {
        Iterator t = nodes.iterator();
        while (t.hasNext()) {
            NetNode node = (NetNode) t.next();
            if (node.output) if (node.hasSneakPaths()) return true;
        }
        return false;
    }

    /** 
     * Sets foldingFactors of all edges in the NetGraph.
     * As a side-effect, also sets aveDriveStrength[], edgeFanout[],
     * and gateLoad[] of all nodes.
     **/
    private void setFoldingFactors() {
        if (doneFoldingFactors == true) return;
        for (Iterator ni = nodes.iterator(); ni.hasNext();) {
            NetNode node = (NetNode)ni.next();
            Set pathsToProcess = new HashSet(node.paths);
            node.aveDriveStrength = new double[] { 0.0F, 0.0F };
            int[] numUniquePaths = { 0, 0 };
            while (pathsToProcess.size() > 0) {
                NetPath path1 = (NetPath) pathsToProcess.iterator().next();
                List equivalentPaths = new ArrayList();
                // Find all unique edges in equivalent paths
                Set uniqueEdges = new HashSet();
                for (Iterator pi=pathsToProcess.iterator(); pi.hasNext();) {
                    NetPath path2 = (NetPath) pi.next();
                    if (path1.compareTo(path2)==0) {
                        equivalentPaths.add(path2);
                        uniqueEdges.addAll(path2.edges);
                        pi.remove();
                    }
                }
                // Aggregate resistance (squares) of equivalent transistors
                double R = 0.0;                         // series resistance
                while (uniqueEdges.size() > 0) {
                    Iterator ei=uniqueEdges.iterator();
                    NetEdge edge1 = (NetEdge) ei.next();
                    ei.remove();                        // parallel conductance
                    double G = edge1.width / edge1.length;
                    edge1.gate.edgeFanout[edge1.type]++;
                    edge1.gate.gateLoad[edge1.type] += edge1.width*edge1.length;
                    while (ei.hasNext()) {
                        NetEdge edge2 = (NetEdge) ei.next();
                        if (edge2.gate.compareTo(edge1.gate)==0) {
                            G += edge2.width / edge2.length;
                            ei.remove();
                            edge2.gate.edgeFanout[edge2.type]++;
                            edge2.gate.gateLoad[edge2.type] +=
                                edge2.width * edge2.length;
                        }
                    }
                    R += 1/G;
                }
                node.aveDriveStrength[path1.type] += 1/R;
                numUniquePaths[path1.type]++;

                // Determine folding factors of all paths
                for (Iterator pi=equivalentPaths.iterator(); pi.hasNext();) {
                    NetPath path = (NetPath) pi.next();
                    double foldingFactor = path.getSquares() / R;
                    for (Iterator ei=path.edges.iterator(); ei.hasNext();) {
                        NetEdge edge = (NetEdge) ei.next();
                        assert (edge.foldingFactor == -1.0 ||
                                edge.foldingFactor == foldingFactor);
                        edge.foldingFactor = foldingFactor;
                    }
                }
            }
            for (int i=0; i<1; i++) {
                if (numUniquePaths[i] > 0) 
                    node.aveDriveStrength[i] /= numUniquePaths[i];
            }
        }
        doneFoldingFactors = true;
    }

    /** Returns the "unfolded" transistor count of the NetGraph **/
    public int getUnfoldedTransistorCount() {
        if (!doneFoldingFactors) setFoldingFactors();
        double count = 0.0;
        for (Iterator ei=getEdges().iterator(); ei.hasNext();) {
            NetEdge edge = (NetEdge)ei.next();
            assert edge.foldingFactor != -1.0F;
            count += 1.0/edge.foldingFactor;
        }
        return (int)count;
    }

    /** pick fold number from shared group */
    private static int fold_from_group(int group, int type) {
        return (group<<3) | type;
    }

    /** return the fold type given a fold number */
    private static int fold_type(int fold) {
        return fold & 7;
    }

}
