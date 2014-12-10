/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cadalyze;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.StringTokenizer;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.ExclusiveNodeSet;
import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.prs.UnimplementableProductionRuleException;
import com.avlsi.io.FileSearchPath;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.tools.cadalyze.LeafStats;
import com.avlsi.tools.lvs.PrsToNet;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.lvs.NetGraph.NetNode;
import com.avlsi.tools.lvs.NetGraph.NetEdge;
import com.avlsi.tools.lvs.NetGraph.NetPath;


/**
 * Gleans interesting statistics from a NetGraph
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **/
public final class NetGraphAnalyzer {
    /** Print lots of debugging information? Used by debugPrintln() **/
    public static boolean debug = false;

    /** The top-level cast cell to analyze **/
    private final CellInterface cell;

    /** Net graph representation of the cell **/
    private NetGraph netgraph;

    /** Map of NetEdge to EdgeType **/
    private HashMap edgeMap;

    /** Map of NedNode to NodeType **/
    private HashMap nodeMap;

    /** Set of all combinational output nodes in the cell **/
    private HashSet combinationalOutputs;

    /** Set of all unmarked nodes in the cell **/
    private HashSet unmarkedNodes;

    /** Statistics **/
    private LeafStats stats;

    private abstract static class NetGraphElementType {

        public static final int UNCLASSIFIED = 0;

        public abstract int       getNumTypes();
        public abstract String[]  getTypeStringArray();
        abstract List             getLogicTypes();

        public Integer type;

        /** Sub-category types **/
        public static final int PRIMARY = 0;
        public static final int STATICIZER = 1;
        public static final int INTERNAL = 2;

        public NetGraphElementType(int t, int subType) {
            type = new Integer(t + subType * getNumTypes());
        }
        public NetGraphElementType() {
            type = new Integer(UNCLASSIFIED);
        }

        public boolean equals(final Object t) {
            return t instanceof NetGraphElementType &&
                (((NetGraphElementType)t).type.intValue() == type.intValue());
        }

        public int hashCode() { return type.intValue(); }

        public void setType(int t) { type = new Integer(t); }
        public void setStaticizerType(int t) { 
            type = new Integer(t + getNumTypes()); 
        }
        
        public int getType() { return type.intValue(); }
        public final Integer getInteger() { return type; }
        public boolean isLogicType() { 
            int t = type.intValue();
            Integer baseType = new Integer(t % getNumTypes());
            return getLogicTypes().contains(baseType);
        }
        public boolean isStaticizerType() {
            return type.intValue() >= getNumTypes() && 
                   type.intValue() < 2 * getNumTypes();
        }
        public boolean isInternal() {
            return type.intValue() >= 2 * getNumTypes();
        }

        public final String toString() {
            final String tstr[] = getTypeStringArray();
            int t = type.intValue();
            String s = new String(tstr[t % getNumTypes()]);
            if (t >= getNumTypes() && t < 2*getNumTypes()) s += " staticizer";
            if (t >= 2*getNumTypes()) s += " internal";
            return s;
        }

        public final String toPaddedString() {
            int max_length = 0;
            final String tstr[] = getTypeStringArray();
            for (int i=0; i<tstr.length; i++) 
                if (tstr[i].length() > max_length)
                    max_length = tstr[i].length();
            max_length += (new String(" staticizer")).length();
            String s = toString();
            for (int i=s.length(); i<max_length; i++) s += " ";
            return s;
        }
    }
        

    final static class EdgeType extends NetGraphElementType {
        static final int DATA_DYNAMIC_LOGIC             = 1;
        static final int DATA_DYNAMIC_LOGIC_OUTPUT      = 2;
        static final int DATA_DYNAMIC_LOGIC_PRECHARGE   = 3;
        static final int CONDITIONAL_OUTPUT_LOGIC       = 4;
        static final int CONDITIONAL_OUTPUT_PRECHARGE   = 5;
        static final int INPUT_COMPLETION               = 6;
        static final int OUTPUT_COMPLETION              = 7;
        static final int MISC_COMPLETION                = 8;
        static final int ACK_LOGIC                      = 9;
        static final int GO                             = 10;
        static final int RESET                          = 11;
        static final int OTHER                          = 12;

        static final String typeStringArray[] = {
            "Unclassified", 
            "Data logic", 
            "Data logic output inverters",
            "Data logic precharge",
            "Conditional output logic",
            "Conditional output precharge",
            "Input completion logic",
            "Output completion logic",
            "Misc completion logic",
            "Ack logic",
            "Go generation",
            "Reset transistors",
            "Other"
        };

        public int getNumTypes() { return typeStringArray.length; }

        static final List logicTypes = new ArrayList();
        static {
            logicTypes.add(new Integer(DATA_DYNAMIC_LOGIC));
            logicTypes.add(new Integer(DATA_DYNAMIC_LOGIC_OUTPUT));
            logicTypes.add(new Integer(DATA_DYNAMIC_LOGIC_PRECHARGE));
        }
        static final List nonStaticizerTypes = new ArrayList();
        static {
            for (int i=0; i<typeStringArray.length; i++)
                nonStaticizerTypes.add(new EdgeType(i));
        }

        public String[] getTypeStringArray() { return typeStringArray; }
        List getLogicTypes() { return logicTypes; }
        public static List getNonStaticizerTypes() {return nonStaticizerTypes;}


        public EdgeType(int t, boolean isStaticizer) { 
            super(t,isStaticizer ? STATICIZER : PRIMARY); 
        }
        public EdgeType(int t) { super(t,PRIMARY); }
        public EdgeType() { super(); }
    }

    final static class NodeType extends NetGraphElementType {
        static final int INPUT_DATA_RAIL                = 1;
        static final int INPUT_ENABLE                   = 2;    // (an output)
        static final int LOGIC_RAIL                     = 3;
        static final int CONDITIONAL_RAIL               = 4;
        static final int OUTPUT_DATA_RAIL               = 5;
        static final int OUTPUT_ENABLE                  = 6;    // (an input)
        static final int GO                             = 7;
        static final int _GO                            = 8;
        static final int INPUT_COMPLETION               = 9;
        static final int OUTPUT_COMPLETION              = 10;
        static final int MISC_COMPLETION                = 11;
        static final int INTERMEDIATE_ACK               = 12;
        static final int INTERNAL_ENABLE                = 13;
        static final int RESET                          = 14;
        static final int SUPPLY                         = 15;
        static final int OTHER                          = 16;

        static final String typeStringArray[] = {
            "Unclassified",
            "Input data rail", "Input enable",
            "Dynamic data logic rail",
            "Dynamic conditional logic rail", 
            "Ouput data rail", "Output enable",
            "Go", "_Go",
            "Input completion", "Output completion", "Misc completion",
            "Intermediate ack",
            "Internal enable", "Reset",
            "Supply",
            "Other"
        };

        public int getNumTypes() { return typeStringArray.length; }

        static final List logicTypes = new ArrayList();
        static {
            logicTypes.add(new Integer(LOGIC_RAIL));
            logicTypes.add(new Integer(OUTPUT_DATA_RAIL));
        }

        public String[] getTypeStringArray() { return typeStringArray; }
        List getLogicTypes() { return logicTypes; }

        public NodeType(int t, int subType) { super(t,subType); }
        public NodeType(int t) { super(t,PRIMARY); }
        public NodeType() { super(); }

        public boolean isGo() { return type.intValue() == GO; }

        public static Collection inputRailTypes() {
            ArrayList types = new ArrayList();
            types.add(new NodeType(INPUT_DATA_RAIL));
            return types;
        }

        public static Collection outputRailTypes() {
            ArrayList types = new ArrayList();
            types.add(new NodeType(LOGIC_RAIL));
            types.add(new NodeType(CONDITIONAL_RAIL));
            return types;
        }

        public static Collection completionTypes() {
            ArrayList types = new ArrayList();
            types.add(new NodeType(INPUT_COMPLETION));
            types.add(new NodeType(OUTPUT_COMPLETION));
            types.add(new NodeType(MISC_COMPLETION));
            return types;
        }

        /** All types that can possibly be involved in ack/enable generation **/
        public static Collection ackFaninTypes() {
            Collection types = completionTypes();
            types.add(new NodeType(INTERMEDIATE_ACK));
            types.add(new NodeType(INPUT_ENABLE));
            types.add(new NodeType(INPUT_DATA_RAIL));
            types.add(new NodeType(INTERNAL_ENABLE));
            types.add(new NodeType(CONDITIONAL_RAIL));
            return types;
        }
    }

    public final static class CellType {

        public static final int UNKNOWN = 0;
        public static final int WCHB    = 1;
        public static final int PCHB    = 2;
        public static final int PCFB    = 3;
        public static final int ACK     = 4;   // (INVC2/CTREE2 from copies)
        public static final int RESET   = 5;   // (RAMP_RESET)
        public static final int OTHER   = 6;

        static final String typeString[] = {
            "Unknown", "WCHB", "PCHB", "PCFB", "ACK", "RESET", "OTHER"
        };

        private int     type = UNKNOWN;
        private boolean has_statebit;
        private boolean has_go;
        private boolean has_conditional_inputs;
        private boolean has_conditional_outputs;
        private boolean has_anomalies;

        private ArrayList anomalyComments = new ArrayList();

        public CellType() {}

        /** 
         * Used when NetGraphAnalyzer user code knows what type of
         * cell it's going to analyze.
         **/
        public CellType(int type) {
            this.type = type;
        }

        public CellType(final CellType from) {
            this.type = from.type;
            this.has_statebit = from.has_statebit;
            this.has_go = from.has_go;
            this.has_conditional_inputs = from.has_conditional_inputs;
            this.has_conditional_outputs = from.has_conditional_outputs;
            this.has_anomalies = from.has_anomalies;
            this.anomalyComments = new ArrayList(from.anomalyComments);
        }
        
        public void setType(int newType) { 
            if (type != UNKNOWN && newType != type) 
                registerAnomaly("Cell type changed from "+ typeString[type] +
                                " to "+ typeString[newType]);
            type = newType; 
        }

        void setStatebit()    { has_statebit = true; }
        void setGo()          { has_go = true; }
        void setCondInputs()  { has_conditional_inputs = true; }
        void setCondOutputs() { has_conditional_outputs = true; }

        void registerAnomaly(String comment) {
            has_anomalies = true;
            anomalyComments.add(comment);
        }

        public int     getType()        { return type; }
        public boolean hasStatebit()    { return has_statebit; }
        public boolean hasGo()          { return has_go; }
        public boolean hasCondInputs()  { return has_conditional_inputs; }
        public boolean hasCondOutputs() { return has_conditional_outputs; }

        public final String toString() {
            String s = " Cell Type: " + typeString[type];
            if (has_go) { s += " w/ go"; } 
            s += "\n Conditional channels:";
            if (has_conditional_inputs) { s += " inputs"; }
            if (has_conditional_outputs) { s += " outputs"; }
            if (!has_conditional_inputs && !has_conditional_outputs) {
                s += " none"; }
            s += "\n Statebit: ";
            if (has_statebit) { s += "yes\n"; }
            else { s += "no"; }
            if (has_anomalies) {
                for (int i=0; i<anomalyComments.size(); i++)
                    s += "\n " + anomalyComments.get(i);
            }
            return s;
        }

        /** 
         * Writes cell data to an open HtmlPage. Note: expected to follow
         * summaryTable().
         **/
        public void toHtml(HtmlPage page) {
            // Type
            String val = new String(typeString[type]);
            if (has_go) val += "w/ go";
            page.summaryTableLine("Cell type:",val);

            // Conditional channels
            val = "";
            if (has_conditional_inputs) val += "Inputs ";
            if (has_conditional_outputs) val += "Outputs";
            if (!has_conditional_inputs && !has_conditional_outputs)
                val = "None";
            page.summaryTableLine("Conditional channels:",val);
            
            // statebit (currently inaccurate)
            if (has_statebit) val = "Yes";
            else val = "No";
            page.summaryTableLine("Includes statebit:",val);
        }

        /** Writes anomaly list in html form **/
        public void printHtmlAnomalyList(HtmlPage page) {
            if (has_anomalies) {
                page.writer.ul();
                for (Iterator ai=anomalyComments.iterator(); ai.hasNext();) {
                    page.writer.li();
                    page.writer.println((String)ai.next());
                }
                page.writer.ulEnd();
            }
            else page.writer.println("None.");
        }
    }

    /** Cell type **/
    private CellType cellType;

    private static void debugPrintln(final String s) {
        if (debug) {
            System.err.println(s);
        }
    }

    private static void debugPrint(final String s) {
        if (debug) {
            System.err.print(s);
        }
    }

    public NetGraphAnalyzer(final CellInterface cell, 
                            final NetGraph netgraph,
                            final CellType type) {
        this.cell = cell;
        this.netgraph = netgraph;
        if (type != null) this.cellType = new CellType(type);
        else this.cellType = new CellType();
        edgeMap = new HashMap();
        nodeMap = new HashMap();
        combinationalOutputs = new HashSet();

        // Mark Vdd & GND nodes
        markNode(netgraph.Vdd,NodeType.SUPPLY);
        markNode(netgraph.GND,NodeType.SUPPLY);

        // Find combinational output nodes
        Iterator nt = netgraph.getNodes().iterator();
        while (nt.hasNext()) {
            NetNode node = (NetNode)nt.next();
            if (node.isCombinational() && node.isPort()) 
                combinationalOutputs.add(node);
        }
        // Initialize unmarkedNodes
        unmarkedNodes = new HashSet(netgraph.getNodes());
        updateUnmarkedNodes();
    }

    private void markNode(final NetNode node, final NodeType type) {
        if (nodeMap.containsKey(node) && 
            !((NodeType)nodeMap.get(node)).equals(type)) {
            System.err.println("Warning: Reclassifying " + node.name + 
                " from " + (NodeType)nodeMap.get(node) + 
                " to " + type);
        }
        nodeMap.put(node,type);
    }

    /** Can be used for non-staticizer types **/
    private void markNode(final NetNode node, int type) {
        markNode(node,new NodeType(type));
    }

    private void markEdge(final NetEdge edge, final EdgeType type) {
        if (edgeMap.containsKey(edge) && 
            !((EdgeType)edgeMap.get(edge)).equals(type)) {
            System.err.println("Warning: Reclassifying " + edge + 
                " from " + (EdgeType)edgeMap.get(edge) + 
                " to " + type);
        }
        edgeMap.put(edge,type);
    }

    /** Can be used for non-staticizer types **/
    private void markEdge(final NetEdge edge, int type) {
        markEdge(edge,new EdgeType(type,false));
    }

    /**
     * Marks all edges in the specified path as type
     **/
    private void markPath(final NetPath path, final EdgeType type) {
        Iterator et = path.getEdges().iterator();
        while (et.hasNext()) {
            markEdge((NetEdge)et.next(),type);
        }
    }

    /** For non-staticizer types **/
    private void markPath(final NetPath path, int type) {
        markPath(path, new EdgeType(type,false));
    }

    /** 
     * Marks 'node, all edges and internal nodes driving 'node', and
     * staticizer edges/nodes driving 'node'.  If overridePriorMark is
     * set, then 'node' and its staticizers are marked regardless of 
     * whether 'node' was already marked.  (Edges are always re-marked.)
     **/
    private void markOperator(final NetNode node, int ntype, int etype,
                              boolean overridePriorMark) {
        EdgeType edgeType = new EdgeType(etype);
        for (Iterator pi = node.getLogicPaths().iterator(); pi.hasNext();) {
            NetPath path = (NetPath) pi.next();
            for (Iterator ei = stripReset(path).iterator(); ei.hasNext();) {
                NetEdge edge = (NetEdge) ei.next();
                markEdge(edge,edgeType);
                markEdgeInternals(edge,path,ntype);
            }
        }
        if (!nodeMap.containsKey(node) || overridePriorMark) {
            markNode(node,ntype);
            markStaticizer(node,ntype,etype);
        }
    }

    /**
     * Is this a reset node?  Marks it if so.
     **/
    private boolean isResetNode(final NetNode node) {
        if (node.name.equals(HierName.makeHierName("Reset")) ||
            node.name.equals(HierName.makeHierName("_Reset")) ||
            node.name.equals(HierName.makeHierName("_RESET"))) {
            markNode(node,new NodeType(NodeType.RESET));
            return true;
        }
        else return false;
    }

    /**
     * Is this edge a reset transistor?  Mark it if so.
     **/
    private boolean isResetEdge(final NetEdge edge) {
        if (isResetNode(edge.gate)) {
            markEdge(edge,new EdgeType(EdgeType.RESET,false));
            return true;
        }
        else return false;
    }

    private boolean isParallelReset(final NetPath path) {
        boolean resetPath = false;
        if (path.getEdges().size() == 1) {
            NetEdge edge = (NetEdge)path.getEdges().iterator().next();
            resetPath = isResetEdge(edge);
        }
        return resetPath;
    }

    /**
     * Strips out reset edges (and marks them), returns the collection of
     * remaining NetEdges.  Returns null if the path is a parallel reset path.
     **/
    private List stripReset(final NetPath path) {
        ArrayList nonResetEdges = new ArrayList();
        if (!isParallelReset(path)) {
            for (int i=0; i<path.getEdges().size(); i++) {
                NetEdge e = (NetEdge)path.getEdges().get(i);
                if (!isResetEdge(e)) nonResetEdges.add(e);
            }
        }
        return nonResetEdges;
    }

    /** Returns non-reset gate nodes of 'path' **/
    private Collection getNonResetGateNodes(final NetPath path) {
        Collection gates = new ArrayList();
        for (Iterator ei = stripReset(path).iterator(); ei.hasNext();) {
            NetEdge edge = (NetEdge) ei.next();
            gates.add(edge.gate);
        }
        return gates;
    }

    /** Returns non-reset (logic) fanin nodes of 'node' **/
    private Set getNonResetFaninNodes(final NetNode node) {
        Set faninNodes = node.getLogicFaninNodes();
        for (Iterator nt=faninNodes.iterator(); nt.hasNext();) {
            NetNode n = (NetNode) nt.next();
            if (isResetNode(n)) nt.remove();
        }
        return faninNodes;
    }

    /**
     * Returns a set of all NetNodes driving gates of the set 'edges'.
     **/
    private Set getGatesOfEdges(final Collection edges) {
        Iterator ei = edges.iterator();
        HashSet gateNodes = new HashSet();
        while (ei.hasNext()) gateNodes.add(((NetEdge)ei.next()).gate);
        return gateNodes;
    }

    /** 
     * Returns all internal nodes of the path.  The returned collection
     * definitely includes redundancies.
     **/
    private Collection getInternalNodes(final NetPath path) {
        Iterator ei = path.getEdges().iterator();
        ArrayList internalNodes = new ArrayList();
        while (ei.hasNext()) {
            NetEdge edge = (NetEdge) ei.next();
            if (edge.source != path.getStartNode() &&
                edge.source != path.getEndNode()) 
                internalNodes.add(edge.source);
            if (edge.drain != path.getStartNode() &&
                edge.drain != path.getEndNode())
                internalNodes.add(edge.drain);
        }
        return internalNodes;
    }

    /** Checks if node is part of some exclusive HI or LO set (not CC) **/
    private boolean isInExclusiveSet(final NetNode node) {
        Iterator it = netgraph.getExclusiveNodeSets().getIterator();
        while (it.hasNext()) {
            ExclusiveNodeSet exset = (ExclusiveNodeSet)it.next();
            if (exset.getHiLo()==ExclusiveNodeSet.HI ||
                exset.getHiLo()==ExclusiveNodeSet.LO) {
                Iterator jt = exset.getNodes();
                while (jt.hasNext())
                    if (((HierName)jt.next()).equals(node.name)) return true;
            }
        }
        return false;
    }

    /**
     * Marks the source and drain nodes of the specified edge as type
     * internalNodeType if they aren't the path's start/end nodes.
     **/
    private void markEdgeInternals(final NetEdge edge, final NetPath path,
                                   int internalNodeType) {
        NodeType type = new NodeType(internalNodeType,NodeType.INTERNAL);
        if (path.getStartNode() != edge.source &&
            path.getEndNode() != edge.source)
            markNode(edge.source,type);
        if (path.getStartNode() != edge.drain &&
            path.getEndNode() != edge.drain)
            markNode(edge.drain,type);
    }

    /**
     * Marks all edges of all specified paths that have 'gate' as their
     * gate node.  Marks internal source/drain nodes of such edges
     * as type internalNodeType.
     **/
    private void markEdgesByGate(final List paths, final NetNode gate, 
                                 int edgeType, int internalNodeType) {
        Iterator pt = paths.iterator();
        while (pt.hasNext()) {
            NetPath path = (NetPath)pt.next();
            Iterator et = path.getEdges().iterator();
            while (et.hasNext()) {
                NetEdge edge = (NetEdge)et.next();
                if (edge.gate.equals(gate)) {
                    markEdge(edge,new EdgeType(edgeType));
                    markEdgeInternals(edge,path,internalNodeType);
                }
            }
        }
    }

    /**
     * Marks all edges of all specified paths that have gate nodes included
     * in the 'gates' set.  Marks internal source/drain nodes of all such
     * edges as type internalNodeType.
     **/
    private void markEdgesByGates(final List paths, final Collection gates, 
                                  int edgeType, int internalNodeType) {
        Iterator pt = paths.iterator();
        while (pt.hasNext()) {
            NetPath path = (NetPath)pt.next();
            Iterator et = path.getEdges().iterator();
            while (et.hasNext()) {
                NetEdge edge = (NetEdge)et.next();
                if (gates.contains(edge.gate)) {
                    markEdge(edge,new EdgeType(edgeType));
                    markEdgeInternals(edge,path,internalNodeType);
                }
            }
        }
    }

    /**
     * Marks an _R.d[i] node, as well as its staticizer edges/nodes,
     * and its output combinational logic edges/nodes (to R.d[i], usually
     * an inverter.)
     **/
    private void markOutputLogicNode(final NetNode node) {
        markNode(node,new NodeType(NodeType.LOGIC_RAIL));
        Iterator nt = combinationalOutputs.iterator();
        while (nt.hasNext()) {
            NetNode outnode = (NetNode)nt.next();
            Iterator pt = outnode.getPaths().iterator();
            while (pt.hasNext()) {
                NetPath path = (NetPath)pt.next();
                if (nodeDrivesPath(node,path)) {
                    markNode(outnode,NodeType.OUTPUT_DATA_RAIL);
                    // assert outnode belongs to an exclusive set?
                    markPath(path,EdgeType.DATA_DYNAMIC_LOGIC_OUTPUT);
                }
            }
        }
        // Mark the staticizer
        // NOTE: Checking for node.isStaticized() Doesn't return true 
        // for byte cells (i.e. cells that have flattened NetGraph subcells)
        // for some reason...
        markStaticizer(node,NodeType.LOGIC_RAIL, EdgeType.DATA_DYNAMIC_LOGIC);
    }

    /**
     * Marks the staticizer edges/nodes attached to 'node'.  Understands
     * both full staticizers and weak inverter staticizers.  Doesn't
     * correctly handle combinational staticizers (assumes the feedback
     * operator is an inverter).
     **/
    private void markStaticizer(final NetNode node, int ntype, int etype) {
        Iterator st = node.getFeedbackEdges().iterator();
        if (!st.hasNext()) debugPrintln("No feedback edges for "+node.name);
        while (st.hasNext()) {
            NetEdge statedge = (NetEdge)st.next();
            markEdge(statedge,new EdgeType(etype,true));
            NetNode statnode = (NetNode)statedge.gate;
            if (statnode.isStaticizerInverter()) {
                markNode(statnode,new NodeType(ntype,NodeType.STATICIZER));
                Iterator pt = statnode.getPaths().iterator();
                while (pt.hasNext()) {
                    markPath((NetPath)pt.next(),new EdgeType(etype,true));
                }
            }
        }
    }

    /**
     * Marks the C-element edges driving 'node', as well as 'node' itself, 
     * and the nodes & edges associated with the C-element's staticizer.
     * Also marks all internal nodes of the C-element.
     * Returns a Pair of NetNodes driving the C-element.  Returns null
     * if a C-element does not drive 'node', in which case none of the
     * nodes & edges are marked.
     **/
    private Set markCElement(final NetNode node, int ntype, int etype) {
        HashSet edges = new HashSet();
        ArrayList internalNodes = new ArrayList();
        HashSet fanin = null;
        Iterator pi = node.getLogicPaths().iterator();
        while (pi.hasNext()) {
            NetPath path = (NetPath) pi.next();
            Collection pathInternalNodes = getInternalNodes(path);
            Collection pathEdges = stripReset(path);
            Set pathFanin = getGatesOfEdges(pathEdges);
            if (fanin != null && (!pathFanin.containsAll(fanin) ||
                                  !fanin.containsAll(pathFanin))) {
                // Not a C-element
                return null;
            }
            else if (fanin == null) {
                fanin = new HashSet(pathFanin);
            }
            edges.addAll(pathEdges);
            internalNodes.addAll(pathInternalNodes);
        }
        Iterator ei = edges.iterator();
        while (ei.hasNext()) {
            NetEdge e = (NetEdge)ei.next();
            markEdge(e,etype);
        }
        markNode(node,ntype);
        Iterator ni = internalNodes.iterator();
        while (ni.hasNext()) markNode((NetNode)ni.next(),
            new NodeType(ntype,NodeType.INTERNAL));
        // Deal with staticizer
        markStaticizer(node,ntype,etype);
        return fanin;
    }

    /**
     * Marks edges of an inverter driving 'node'.  Returns the NetNode
     * driving the inverter, or null if 'node' is not driven by an inverter
     * (and only an inverter).  If stripReset==true, reset transistors 
     * attached to the inverter are allowed and, if found, are marked.
     **/
    private NetNode markInverterEdges(final NetNode node, int type, 
                                      boolean stripReset) {
        NetNode fanin = null;
        HashSet edgesToMark = new HashSet();
        boolean up=false, dn=false;
        Iterator pi = node.getPaths().iterator();
        while (pi.hasNext()) {
            NetPath p = (NetPath)pi.next();
            List pathEdges;
            if (stripReset) pathEdges = stripReset(p);
            else pathEdges = p.getEdges();
            if (pathEdges.size()!=1 || fanin != null &&
                ((NetEdge)pathEdges.get(0)).gate != fanin) return null;
            else if (fanin == null) {
                fanin = ((NetEdge)pathEdges.get(0)).gate;
            }
            edgesToMark.add((NetEdge)pathEdges.get(0));
            if (p.getDir() == 1) up=true;
            else if (p.getDir() == -1) dn=true;
        }
        if (!up || !dn) return null;
        Iterator ei = edgesToMark.iterator();
        while (ei.hasNext()) {
            markEdge((NetEdge)ei.next(),type);
        }
        return fanin;
    }

    /** Normal usage: always strip reset transistors **/
    private NetNode markInverterEdges(final NetNode node, int type) {
        return markInverterEdges(node,type,true);
    }

    /** 
     * Returns true if the specified node drives some gate in the
     * specified path.
     **/
    private boolean nodeDrivesPath(final NetNode node, final NetPath path) {
        Collection gatenames = path.getGates();
        return gatenames.contains(node.name);
    }

    /** 
     * Returns true if the specified node drives a combinational
     * operator to a cell output node.
     **/
    private boolean hasCombinationalPathToOutput(final NetNode node) {
        Iterator nt = combinationalOutputs.iterator();
        while (nt.hasNext()) {
            NetNode outnode = (NetNode)nt.next();
            Iterator pt = outnode.getPaths().iterator();
            while (pt.hasNext()) {
                NetPath path = (NetPath)pt.next();
                if (nodeDrivesPath(node,path)) return true;
            }
        }
        return false;
    }

    /**
     * Determines if 'node' has prs of the following form:
     *
     *  p1 & p2 & .. & pN & (logic terms) -> node-
     *  ~p1 & ~p2 & .. & ~pN -> node+
     *
     * if so, returns a pair of two sets of NetNodes:
     *   (1) nonempty set of {p1,p2,..pN} precharge nodes
     *   (2) Possibly empty set of (logic terms) nodes
     * 
     * If 'node' is not driven in the manner described above, then
     * the returned pair will be null.
     * Note: reset nodes are ignored (relies on proper naming).
     **/
    private Pair isPrechargedLogicNode(final NetNode node) {
        if (!node.isOutput() || node.isCombinational()) return null;
        List paths = node.getLogicPaths();
        Iterator pt = paths.iterator();
        debugPrintln("Examining logic paths of output " +
                     node.getName().toString());
        Set up_terms = new HashSet();
        Set dn_terms = new HashSet();
        Set common_up_terms = new HashSet();
        Set common_dn_terms = new HashSet();
        boolean up_first = true;
        boolean dn_first = true;
        while (pt.hasNext()) {
            NetPath p = (NetPath) pt.next();
            debugPrintln(p.toString());
            if (p.getDir()==0)
                cellType.registerAnomaly("Illegal path found on output "+
                                         node.name);
            else if (!isParallelReset(p)) {
                Set tset,ctset;
                boolean first = false;
                if (p.getDir()==-1) {   // to GND
                    tset = dn_terms;
                    ctset = common_dn_terms;
                    if (dn_first) first = true;
                    dn_first = false;
                }
                else {                  // to Vdd
                    tset = up_terms;
                    ctset = common_up_terms;
                    if (up_first) first = true;
                    up_first = false;
                }
                Iterator gt = p.getEdges().iterator();
                HashSet gnodes = new HashSet();
                while (gt.hasNext()) {
                    NetNode g = ((NetEdge)gt.next()).gate;
                    // Special-case Reset/_Reset: ignore them
                    if (!isResetNode(g)) {
                        tset.add(g);
                        if (first) {
                            ctset.add(g);
                        }
                        else {
                            gnodes.add(g);
                        }
                    }
                }
                if (!first) {
                    ctset.retainAll(gnodes);
                }
            }
        }
        if (debug) {
            debugPrintln("Non-reset terms of output " + node.name + ":");
            debugPrint(" -(");
            pt = dn_terms.iterator();
            while (pt.hasNext()) {
                NetGraph.NetNode term = (NetGraph.NetNode)pt.next();
                debugPrint(" " + term.getName().toString());
                if (common_dn_terms.contains(term)) debugPrint("*");
            }
            debugPrint(" )  +(");
            pt = up_terms.iterator();
            while (pt.hasNext()) {
                NetGraph.NetNode term = (NetGraph.NetNode)pt.next();
                debugPrint(" " + term.getName().toString());
                if (common_up_terms.contains(term)) debugPrint("*");
            }
            debugPrint(" )\n");
        }
        // Now determine if this a precharged logic node.
        // Rule: If common_up_terms==up_terms and 
        //       If common_up_terms<=common_dn_terms and
        //       If dn_term-common_up_terms is non-empty.
        Set logic_terms = new HashSet();
        if (common_up_terms.containsAll(up_terms) &&
            common_dn_terms.containsAll(common_up_terms)) {
            logic_terms.addAll(dn_terms);
            logic_terms.removeAll(common_up_terms);
        }
        else {
            return null;
        }
        return new Pair(common_up_terms,logic_terms);
    }

    /**
     * Identifies _R.d[i] logic nodes and associated transistor edges.
     * Also identifies en/go/L.e, R.e, and L.d[i].
     **/
    private void identifyLogic() {
        Iterator t = netgraph.getNodes().iterator();
        while (t.hasNext()) {
            NetNode n = (NetNode) t.next();
            Pair logicPair = isPrechargedLogicNode(n);
            if (logicPair != null) {
                // A candidate logic node.  Now Need to additionally check that
                //    logic_terms are all cell inputs 
                //    and is non-empty unless precharge_terms includes
                //      data rail cell inputs (WCHB buffer case)
                Set precharge_terms = (Set)logicPair.getFirst();
                Set logic_terms = (Set)logicPair.getSecond();
                Iterator gt = logic_terms.iterator();
                List paths = n.getLogicPaths();
                boolean all_inputs = true;
                while (gt.hasNext()) {
                    NetNode node = (NetNode)gt.next();
                    if (!node.isOutput() && node.isPort() &&
                        isInExclusiveSet(node))
                        markNode(node,NodeType.INPUT_DATA_RAIL);
                    else all_inputs = false;
                }
                debugPrintln("all_inputs = "+all_inputs);
                if (all_inputs && logic_terms.size()>0) {
                    // This is a logic node... but may be a conditional
                    // output rail.
                    debugPrintln("Is a dynamic logic output");
                    if (hasCombinationalPathToOutput(n)) {
                        markOutputLogicNode(n);
                        markEdgesByGates(paths,logic_terms,
                            EdgeType.DATA_DYNAMIC_LOGIC,
                            NodeType.LOGIC_RAIL);
                        // Learn what we can from the precharge_terms
                        gt = precharge_terms.iterator();
                        boolean has_re = false;
                        boolean has_le = false;
                        boolean has_en = false;
                        boolean has_go = false;
                        boolean wchb = false;
                        while (gt.hasNext()) {
                            NetNode node = (NetNode)gt.next();
                            if (node.isPort() && !node.isOutput()) {
                                // L.d[i] or R.e
                                if (isInExclusiveSet(node)) {
                                    markNode(node,NodeType.INPUT_DATA_RAIL);
                                    markEdgesByGate(paths,node,
                                        EdgeType.DATA_DYNAMIC_LOGIC_PRECHARGE,
                                        NodeType.LOGIC_RAIL);
                                    wchb = true;
                                }
                                else {
                                    has_re = true;
                                    markNode(node,NodeType.OUTPUT_ENABLE);
                                    markEdgesByGate(paths,node,
                                        EdgeType.DATA_DYNAMIC_LOGIC_PRECHARGE,
                                        NodeType.LOGIC_RAIL);
                                }
                            }
                            else if (node.isPort() && node.isOutput()) {
                                // L.e
                                markOperator(node,NodeType.INPUT_ENABLE,
                                             EdgeType.ACK_LOGIC,false);
                                markEdgesByGate(paths,node,
                                    EdgeType.DATA_DYNAMIC_LOGIC_PRECHARGE,
                                    NodeType.LOGIC_RAIL);
                                has_le = true;
                            }
                            else if (!node.isPort()) {
                                // en or go, depending on number
                                if (precharge_terms.size()==1) {
                                    has_go = true;
                                    markNode(node,NodeType.GO);
                                    markEdgesByGate(paths,node,
                                        EdgeType.DATA_DYNAMIC_LOGIC_PRECHARGE,
                                        NodeType.LOGIC_RAIL);
                                }
                                else {
                                    has_en = true;
                                    markOperator(node,NodeType.INTERNAL_ENABLE,
                                                 EdgeType.ACK_LOGIC,false);
                                    markEdgesByGate(paths,node,
                                        EdgeType.DATA_DYNAMIC_LOGIC_PRECHARGE,
                                        NodeType.LOGIC_RAIL);
                                }
                            }
                        }
                        debugPrintln("has_re="+has_re+" "+
                                           "has_le="+has_le+" "+
                                           "has_en="+has_en+" "+
                                           "has_go="+has_go+" "+
                                           "wchb="+wchb);
                        // Consistency check, set template type
                        if      ( has_le &&  has_re && !has_en && 
                                 !has_go && !wchb ||
                                 !has_le &&  has_re &&  has_en &&
                                 !has_go && !wchb ||
                                 !has_le && !has_re && !has_en &&
                                  has_go && !wchb)
                            cellType.setType(CellType.PCHB);
                        else if (!has_le &&  has_re && !has_en &&
                                 !has_go &&  wchb)
                            cellType.setType(CellType.WCHB);
                        else {
                            System.err.println("Warning: Unknown template type.");
                            cellType.setType(CellType.OTHER);
                        }
                        if (has_go) cellType.setGo();
                    }
                    else {
                        // Conditional output logic rail
                        markNode(n,NodeType.CONDITIONAL_RAIL);
                        markEdgesByGates(paths,logic_terms,
                            EdgeType.CONDITIONAL_OUTPUT_LOGIC,
                            NodeType.CONDITIONAL_RAIL);
                        // Mark precharge gates & edges
                        gt = precharge_terms.iterator();
                        while (gt.hasNext()) {
                            NetNode node = (NetNode)gt.next();
                            markEdgesByGate(paths,node,
                                EdgeType.CONDITIONAL_OUTPUT_PRECHARGE,
                                NodeType.CONDITIONAL_RAIL);
                        }
                        // Mark staticizer
                        markStaticizer(n,NodeType.CONDITIONAL_RAIL,
                            EdgeType.CONDITIONAL_OUTPUT_PRECHARGE);
                    }
                }
                else if (logic_terms.size()==0 && 
                         hasCombinationalPathToOutput(n) &&
                         precharge_terms.size()==2) {
                    // Annoying WCHB buffer case...
                    gt = precharge_terms.iterator();
                    NetNode node1 = (NetNode)gt.next();
                    NetNode node2 = (NetNode)gt.next();
                    boolean excl1 = isInExclusiveSet(node1);
                    boolean excl2 = isInExclusiveSet(node2);
                    if (node1.isPort() && !node1.isOutput() &&
                        node2.isPort() && !node2.isOutput() &&
                        (excl1 && !excl2 || !excl1 && excl2)) {
                        NetNode ldnode,renode;
                        if (excl1) {
                            ldnode = node1; renode = node2;
                        }
                        else {
                            ldnode = node2; renode = node1;
                        }
                        markNode(ldnode,NodeType.INPUT_DATA_RAIL);
                        markEdgesByGate(paths,ldnode,
                                EdgeType.DATA_DYNAMIC_LOGIC,
                                NodeType.LOGIC_RAIL);
                        markNode(renode,NodeType.OUTPUT_ENABLE);
                        markEdgesByGate(paths,renode,
                                EdgeType.DATA_DYNAMIC_LOGIC_PRECHARGE,
                                NodeType.LOGIC_RAIL);
                        markOutputLogicNode(n);
                        cellType.setType(CellType.WCHB);
                        debugPrintln("WCHB: node="+n.name);
                    }
                }
            }
        }
    }

    /**
     * Call after identifyLogic if the cell has any go signals.
     * This will identify the _go inverters, marking the associated
     * _go nodes as "_GO" and edges as "GO".  Will also mark the output 
     * enable node and the internal enable.
     *
     * After this call, all port nodes should be marked.
     **/
    private void identifyGo() {
        if (cellType.hasGo()) {
            // First identify all go nodes
            ArrayList goNodes = new ArrayList();
            Iterator nt = nodeMap.keySet().iterator();
            while (nt.hasNext()) {
                NetNode go = (NetNode)nt.next();
                if (((NodeType)nodeMap.get(go)).isGo()) goNodes.add(go);
            }
            for (int i=0; i<goNodes.size(); i++) {
                NetNode go = (NetNode)goNodes.get(i);
                NetNode _go = markInverterEdges(go,EdgeType.GO);
                if (_go == null) {
                    cellType.registerAnomaly("Go node "+go.name+
                        " not driven by an inverter.");
                    return;
                }
                Set _goFanin = markCElement(_go,NodeType._GO,EdgeType.GO);
                if (_goFanin == null || _goFanin.size()!=2) {
                    cellType.registerAnomaly("2-input C-element does " +
                        "not drive " + _go.name + " _go signal.");
                    return;
                }
                else {
                    Iterator ni = _goFanin.iterator();
                    for (int j=0; j<2; j++) {
                        NetNode node = (NetNode)ni.next();
                        if (node.isPort() && !node.isOutput()) {
                            // R.e
                            markNode(node,NodeType.OUTPUT_ENABLE);
                        }
                        else if (node.isPort() && node.isOutput()) {
                            // L.e = en
                            markOperator(node,NodeType.INPUT_ENABLE,
                                         EdgeType.ACK_LOGIC,false);
                        }
                        else if (!node.isPort() && node.isOutput()) {
                            // en
                            markOperator(node,NodeType.INTERNAL_ENABLE,
                                         EdgeType.ACK_LOGIC,false);
                        }
                        else {
                            cellType.registerAnomaly("Unknown _go fan-in node "+
                                node.name);
                        }
                    }
                }
            }
        }
    }

    /**
     * Identifies all remaining reset edges and nodes.  Assumes that, at
     * minimum, _Reset has been identified by now.  Also assumes that all
     * reset nodes are driven by inverters.
     **/
    private void identifyReset() {
        // Identify known reset nodes
        ArrayList resetNodes = new ArrayList();
        Iterator ei = nodeMap.entrySet().iterator();
        NodeType resetType = new NodeType(NodeType.RESET);
        while (ei.hasNext()) {
            Entry entry = (Entry) ei.next();
            if (((NodeType)entry.getValue()).equals(resetType))
                resetNodes.add((NetNode)entry.getKey());
        }
        // Mark all inverters + fanin nodes driving reset nodes
        for (Iterator ni = resetNodes.iterator(); ni.hasNext();) {
            NetNode reset = (NetNode) ni.next();
            do {
                markNode(reset,NodeType.RESET);
                reset = markInverterEdges(reset,EdgeType.RESET,false);
            } while (reset != null && !nodeMap.keySet().contains(reset));
        }
    }

    /** 
     * Checks if all port nodes are marked.  If an unmarked port named
     * "_RESET" is encountered, it's marked.  Any other unmarked input 
     * ports are assumed to be input data rails.  Rationale:
     *
     *   1. All input enables should be involved in an output domino 
     *      logic stage -- either directly or via a go signal.  We
     *      should have covered both cases by now.
     *   2. Input data rails sometimes feed directly into ack logic
     *      (usually for conditional inputs, but sometimes, e.g.
     *      FILTER_8_1of2, for conditional outputs).
     *   3. The standard leaf cells for which this code is designed
     *      have no input node types other than enables and data rails.
     *
     * This function does not attempt to identify unmarked output ports.
     * It is assumed taht these are input channel enables that will be
     * marked by identifyAck().
     **/
    private void identifyPorts() {
        updateUnmarkedNodes();
        for (Iterator nt=unmarkedNodes.iterator(); nt.hasNext();) {
            NetNode port = (NetNode) nt.next();
            if (port.isPort()) {
                if (!port.isOutput()) {
                    if (port.name.equals(HierName.makeHierName("_RESET")))
                        markNode(port,NodeType.RESET);
                    else
                        markNode(port,NodeType.INPUT_DATA_RAIL);
                }
            }
        }
    }

    /**
     * Updates the unmarkedNodes set.  Assumes that the set is initialized
     * to include all nodes and that the set monotonically decreases from
     * invocation to invocation of this method.
     **/
    private void updateUnmarkedNodes() {
        unmarkedNodes.removeAll(nodeMap.keySet());
        if (debug) {
            System.out.println("Unmarked nodes:");
            for (Iterator it=unmarkedNodes.iterator(); it.hasNext();) {
                NetNode node = (NetNode) it.next();
                System.out.print(" "+node.name);
            }
            System.out.print("\n");
        }
    }

    /**
     * Returns the set of all unidentified output nodes that the specified 
     * set of input nodes exclusively drives.  Marks all edges involved
     * in the fanout, as well as associated internal nodes.  Note that it
     * is not required that _all_ of the completionNodes input set be 
     * covered by the returned set of nodes.
     * New: Verifies that for each node, pull-down fanin == pull-up fanin.
     **/
    private Set identifyCompletionFanout(final Collection completionNodes,
                                         int ntype, int etype, 
                                         boolean stopAtUnmarked,
                                         int outputPortType) {
        HashSet fanoutSet = new HashSet();
        Collection fanoutCandidates;
        if (stopAtUnmarked) fanoutCandidates = unmarkedNodes;
        else fanoutCandidates = netgraph.getNodes();
        for (Iterator ni = fanoutCandidates.iterator(); ni.hasNext();) {
            NetNode unmarked = (NetNode) ni.next();
            if (unmarked.isOutput() && !unmarked.isStaticizerInverter()) {
                //boolean isCompletion = true;
                //boolean nonEmpty = false;
                HashSet pulldnTerms = new HashSet();
                HashSet pullupTerms = new HashSet();
                for (Iterator pi = unmarked.getLogicPaths().iterator(); 
                     pi.hasNext();) {
                    NetPath path = (NetPath) pi.next();
                    if (path.getDir() == -1)
                        pulldnTerms.addAll(getNonResetGateNodes(path));
                    else
                        pullupTerms.addAll(getNonResetGateNodes(path));
                    //Collection faninNodes = getNonResetGateNodes(path);
                    //if (!completionNodes.containsAll(faninNodes))
                    //    isCompletion = false;
                    //else if (!faninNodes.isEmpty()) nonEmpty = true;
                }
                //if (isCompletion && nonEmpty) {
                if (pulldnTerms.equals(pullupTerms) && !pulldnTerms.isEmpty()
                    && completionNodes.containsAll(pullupTerms)) {
                    debugPrintln("Identified completion node "+unmarked.name);
                    fanoutSet.add(unmarked);
                    // Mark node, fanin edges, internal nodes, and staticizer
                    if (!unmarked.isPort())
                        markOperator(unmarked,ntype,etype,false);
                    else
                        markOperator(unmarked,outputPortType,etype,false);
                }
            }
        }
        return fanoutSet;
    }

    /**
     * Reverse lookup of nodeMap; given a 'type' nodeType, returns the set
     * of NetNodes that have been marked with that type.
     **/
    private Collection getNodesByType(final NodeType type) {
        Collection nodeSet = new ArrayList();
        Iterator ei = nodeMap.entrySet().iterator();
        while (ei.hasNext()) {
            Entry entry = (Entry) ei.next();
            if (((NodeType)entry.getValue()).equals(type))
                nodeSet.add(entry.getKey());
        }
        return nodeSet;
    }

    /**
     * Reverse set lookup of nodeMap; given a set of nodeTypes, returns
     * the set of NetNodes that have been marked with any of those types.
     **/
    private Collection getNodesByTypeSet(final Collection typeSet) {
        Collection nodeSet = new ArrayList();
        Iterator ei = nodeMap.entrySet().iterator();
        while (ei.hasNext()) {
            Entry entry = (Entry) ei.next();
            if (typeSet.contains(entry.getValue()))
                nodeSet.add(entry.getKey());
        }
        return nodeSet;
    }

    private void identifyCompletionOfNodes(Collection completionNodes,
                                    int completionNodeType,
                                    int completionEdgeType,
                                    boolean stopAtUnmarked,
                                    int outputPortType) {
        do {
            completionNodes = identifyCompletionFanout(completionNodes,
                                completionNodeType,completionEdgeType,
                                stopAtUnmarked,outputPortType);
        } while (!completionNodes.isEmpty());
    }

    /**
     * Identifies completion logic of a particular class of nodes
     * (identified by the 'type' NodeType).  Completion logic is defined
     * as a tree of logic compressing an input set of nodes down to a 
     * smaller set of output nodes.  Identification stops at the first 
     * stage where completion nodes are combined with unrelated nodes.
     * Any node in the completion tree that isn't a port is marked as
     * type 'completionNodeType'; any node that is a port is marked as
     * 'outputPortType'.
     **/
    private void identifyCompletion(final Collection inputTypes,
                                    int completionNodeType,
                                    int completionEdgeType,
                                    boolean stopAtUnmarked,
                                    int outputPortType) {
        updateUnmarkedNodes();
        Collection completionSet = getNodesByTypeSet(inputTypes);
        /*
        Collection completionSet = new ArrayList();
        Iterator ei = nodeMap.entrySet().iterator();
        while (ei.hasNext()) {
            Entry entry = (Entry) ei.next();
            if (inputTypes.contains(entry.getValue()))
                completionSet.add(entry.getKey());
        }
        */
        if (debug) {
            System.out.println("Completion input nodes:");
            for (Iterator ni = completionSet.iterator(); ni.hasNext();)
                System.out.print(" "+((NetNode)ni.next()).name);
            System.out.print("\n");
        }
        identifyCompletionOfNodes(completionSet,completionNodeType,
                                  completionEdgeType,stopAtUnmarked,
                                  outputPortType);
    }

    private void identifyAck() {
        updateUnmarkedNodes();
        Collection ackFaninNodes = getNodesByTypeSet(NodeType.ackFaninTypes());
        // Construct sets of ack node candidates and input data rail candidates
        Set ackCandidateNodes = new HashSet();
        Set inputCandidateNodes = new HashSet();
        for (Iterator ni=unmarkedNodes.iterator(); ni.hasNext();) {
            NetNode n =  (NetNode) ni.next();
            if (n.isPort() && !n.isOutput()) inputCandidateNodes.add(n);
            else if (n.isOutput() && !n.isStaticizerInverter()) 
                ackCandidateNodes.add(n);
        }
        Set newAckNodes = new HashSet();
        Set newInputNodes = new HashSet();
        boolean progress;
        do {
            progress = false;
            for (Iterator ni=ackCandidateNodes.iterator(); ni.hasNext();) {
                NetNode candidate = (NetNode)ni.next();
                Set faninNodes = getNonResetFaninNodes(candidate);
                // Check if all fanin nodes are either known ack fanin
                // nodes or unidentified port inputs
                boolean isAck = !faninNodes.isEmpty();
                boolean mightBeAck = !faninNodes.isEmpty();
                for (Iterator nj=faninNodes.iterator(); 
                     mightBeAck && nj.hasNext();) {
                    NetNode n = (NetNode) nj.next();
                    boolean goodFanin = ackFaninNodes.contains(n) ||
                                        inputCandidateNodes.contains(n) ||
                                        newAckNodes.contains(n) ||
                                        newInputNodes.contains(n);
                    isAck = isAck && goodFanin;
                    mightBeAck = mightBeAck && 
                        (goodFanin || ackCandidateNodes.contains(n));
                }
                if (isAck) {
                    debugPrintln("Identified ack candidate node "+candidate);
                    faninNodes.retainAll(inputCandidateNodes);
                    newInputNodes.addAll(faninNodes);
                    inputCandidateNodes.removeAll(faninNodes);
                    newAckNodes.add(candidate);
                    ni.remove();
                    progress = true;
                }
                else if (!mightBeAck) {
                    ni.remove();
                    progress = true;
                }
                else debugPrintln("Retaining node "+candidate+
                                  " as possible ack node");
            } 
        } while (progress);
        // Ideally, would verify that each identified input enable is 
        // dependent on some Rv node, and prune out those that don't (as
        // well as associated newAckNodes.)  For now, let's just assume 
        // everything's fine.
        for (Iterator ni=newAckNodes.iterator(); ni.hasNext();) {
            NetNode ackNode = (NetNode) ni.next();
            if (ackNode.isPort())
                markOperator(ackNode,NodeType.INPUT_ENABLE,
                             EdgeType.ACK_LOGIC,false);
            else
                markOperator(ackNode,NodeType.INTERMEDIATE_ACK,
                             EdgeType.ACK_LOGIC,false);
        }
        for (Iterator ni=newInputNodes.iterator(); ni.hasNext();)
            markNode((NetNode)ni.next(),NodeType.INPUT_DATA_RAIL);
    }

    /** Registers remaining unmarked nodes & edges with cellType **/
    private void registerUnmarked() {
        updateUnmarkedNodes();
        for (Iterator ni = unmarkedNodes.iterator(); ni.hasNext();) {
            cellType.registerAnomaly("Couldn't identify node "+
                                     ((NetNode)ni.next()).name);
        }
        HashSet unmarkedEdges = new HashSet(netgraph.getEdges());
        unmarkedEdges.removeAll(edgeMap.keySet());
        for (Iterator ei = unmarkedEdges.iterator(); ei.hasNext();) {
            cellType.registerAnomaly("Couldn't identify edge "+ 
                                     (NetEdge)ei.next());
        }
    }

    /** 
     * Marks all nodes and edges in the NetGraph to be the specified
     * type. (Except reset and Vdd/GND nodes.)
     **/
    private void markAll(int ntype, int etype) {
        for (Iterator ni = netgraph.getNodes().iterator(); ni.hasNext();) {
            NetNode node = (NetNode) ni.next();
            if (node.isPort() && !node.isOutput() && !node.isRail() && 
                !isResetNode(node))
                markNode(node,ntype);
            else if (node.isOutput() && !node.isStaticizerInverter()) 
                markOperator(node,ntype,etype,false);
            // else assumed to be covered by markOperator of some other node
        }
        updateUnmarkedNodes();
        //assert unmarkedNodes.isEmpty();
        if (!unmarkedNodes.isEmpty()) {
            System.err.println(cell.getFullyQualifiedType()+": "+
                               "markAll() didn't mark all nodes!");
            registerUnmarked();
        }
    }

    /**
     * Analyzes the NetGraph, categorizing all nodes and transistors.
     * Returns a mapping of all HierNames (i.e. all output and port
     * NetNodes) to NodeProps objects.
     **/
    public Map analyzeCell() {

        // Analyze the NetGraph appropriately, depending on type info
        if (cellType.getType() != CellType.ACK &&
            cellType.getType() != CellType.RESET &&
            cellType.getType() != CellType.OTHER) {
            // Assumed to be a proper non-fragment asynchronous leaf cell
            identifyLogic();
            identifyGo();
            identifyPorts();
            identifyCompletion(NodeType.inputRailTypes(),
                               NodeType.INPUT_COMPLETION,
                               EdgeType.INPUT_COMPLETION,true,
                               NodeType.INPUT_ENABLE);
            identifyCompletion(NodeType.outputRailTypes(),
                               NodeType.OUTPUT_COMPLETION,
                               EdgeType.OUTPUT_COMPLETION,true,
                               NodeType.INPUT_ENABLE);
            identifyAck();
            identifyReset();
            registerUnmarked();
        }
        else if (cellType.getType() == CellType.ACK) {
            markAll(NodeType.INTERMEDIATE_ACK,EdgeType.ACK_LOGIC);
        }
        else if (cellType.getType() == CellType.RESET) {
            markAll(NodeType.RESET,EdgeType.RESET);
        }
        else markAll(NodeType.OTHER,EdgeType.OTHER);

        // Tally statistics
        tallyLeafStats();

        // Generate and return the String-to-NodeProps map
        return mapNodeProps();
    }

    /** Tallies marks and packages info into a LeafStats **/
    private void tallyLeafStats() {
        HashMap nodeTypeCounts = new HashMap();
        HashMap edgeTypeCounts = new HashMap();
        HashMap edgeTypeCountsUnfolded = new HashMap();
        HashMap edgeTypeAreas = new HashMap();
        HashMap edgeTypeWidths = new HashMap();

        // Tally counts of node categories
        Iterator nt = netgraph.getNodes().iterator();
        while (nt.hasNext()) {
            NetNode node = (NetNode)nt.next();
            NodeType type = new NodeType();
            if (nodeMap.containsKey(node)) {
                type = (NodeType)nodeMap.get(node);
                debugPrintln("NodeType("+node.name+")= "+type);
            }
            int num = 0;
            if (nodeTypeCounts.containsKey(type)) {
                num = ((Integer)nodeTypeCounts.get(type)).intValue();
            }
            num++;
            nodeTypeCounts.put(type,new Integer(num));
        }

        // Tally counts, areas, and widths of transistor categories
        Iterator et = netgraph.getEdges().iterator();
        while (et.hasNext()) {
            NetEdge edge = (NetEdge)et.next();
            EdgeType type = new EdgeType();
            if (edgeMap.containsKey(edge)) {
                type = (EdgeType)edgeMap.get(edge);
                debugPrintln("EdgeType("+edge+")= "+type);
            }
            int num = 0; 
            double uNum = 0.0, area = 0, width = 0;
            if (edgeTypeCounts.containsKey(type)) {
                num = ((Integer)edgeTypeCounts.get(type)).intValue();
                uNum = ((Double)edgeTypeCountsUnfolded.get(type)).doubleValue();
                area = ((Double)edgeTypeAreas.get(type)).doubleValue();
                width = ((Double)edgeTypeWidths.get(type)).doubleValue();
            }
            num++;
            uNum += 1.0/edge.getFoldingFactor();
            area += edge.width * edge.length;
            width += edge.width;
            edgeTypeCounts.put(type,new Integer(num));
            edgeTypeCountsUnfolded.put(type,new Double(uNum));
            edgeTypeAreas.put(type,new Double(area));
            edgeTypeWidths.put(type,new Double(width));
        }
        stats = new LeafStats(netgraph,nodeTypeCounts,edgeTypeCounts,
                              edgeTypeCountsUnfolded,edgeTypeAreas,
                              edgeTypeWidths);

        // Check for percentage of uncalssified nodes, warn if too high,
        // recategorize them as OTHER.
        float uf = stats.getUnclassifiedNodeFraction();
        if (uf > 0.01) {
            DecimalFormat formPercent = 
                (DecimalFormat)DecimalFormat.getInstance();
            formPercent.applyPattern("#0.0%");
            String warnStr = "Could not classify " + formPercent.format(uf) +
                " of nodes";
            cellType.registerAnomaly(warnStr+".");
            System.err.println(warnStr+" in "+ cell.getFullyQualifiedType());
        }
        stats.convertUnclassifiedToOther();
    }

    /** Generates String-to-NodeProps map for all output and port nodes **/
    private Map mapNodeProps() {
        Map nodePropsMap = new HashMap();
        for (Iterator ni=netgraph.getNodes().iterator(); ni.hasNext();) {
            NetNode node = (NetNode) ni.next();
            if ((node.isPort() || node.isOutput()) && !node.isRail()) {
                nodePropsMap.put(node.getName(),new NodeProps(node));
            }
        }
        return nodePropsMap;
    }

    public CellType getCellType() { return cellType; }

    public LeafStats getLeafStats() { return stats; }

    public void printStatistics(boolean verbose) {
        System.out.println(cellType);
        assert stats!=null;
        stats.printStats(verbose);
    }

    /**************************** STATIC METHODS ****************************/

    private static void usage(final int exitStatus) {
        System.err.println("Usage: "+NetGraphAnalyzer.class.getName()+"\n" +
                           " cast_cell \n" +
                           " [--verbose=0|1] [--debug=0|1]" +
                           " [--gates=<cell:...>]\n" +
                           " [--cast-path=castpath]\n" );
        System.exit(exitStatus);
    }

    public static void main(String[] args) {
        
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;
        if (theArgs.argExists("version")) {
            System.out.println(com.avlsi.util.debug.VersionInfo
                .getVersionString(NetGraphAnalyzer.class));
        }
        boolean verbose = false;
        if (theArgs.getArgValue("verbose","").equals("1")) {
            verbose = true;
        }
        if (theArgs.getArgValue("debug","").equals("1")) {
            debug = true;
        }

        final FileSearchPath castPath = 
            new FileSearchPath( theArgs.getArgValue( "cast-path", "." ) );
        
        // check number of arguments
        if (args.length < 2)
          usage(1);

        final String gateString = theArgs.getArgValue("gates", null);
        final List gateList = new ArrayList();
        final StringTokenizer tok = new StringTokenizer(gateString,":");
        while (tok.hasMoreTokens()) {
            gateList.add(tok.nextToken());
        }

        final StringContainerIterator strIter = 
            theArgs.nonParsedArgumentsIterator();
        
        try {
            final String castCellName = strIter.next();
           
            // get the cast body
            final CastFileParser cfp = new CastFileParser(castPath,"2");
            
            final CellInterface cell = 
                cfp.getFullyQualifiedCell(castCellName);

            final NetGraph netgraph = 
                PrsToNet.getNetGraph(cell,cfp,null,null,null,null);

            final NetGraphAnalyzer analyzer = 
                new NetGraphAnalyzer(cell,netgraph,null);
            //stats.printOutputDataPorts();

            if (verbose) System.err.println("Analyzing "+castCellName);
            analyzer.analyzeCell();
            analyzer.printStatistics(verbose);
        }
        catch (Exception e) {
            e.printStackTrace();
            usage(1);
        }
    }
}
