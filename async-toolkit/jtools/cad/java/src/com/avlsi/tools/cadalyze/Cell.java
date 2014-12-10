/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cadalyze;

import java.io.IOException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.SortedSet;
import java.util.StringTokenizer;
import java.util.TreeSet;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
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
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.tools.cadalyze.NetGraphAnalyzer.CellType;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.lvs.NetGraph.NetNode;
import com.avlsi.tools.lvs.NetGraph.NetEdge;
import com.avlsi.tools.lvs.NetGraph.NetPath;
import com.avlsi.tools.lvs.PrsToNet;


/**
 * Class to contain Cadalyze cell statistics & other data.
 * @author Mike Davies
 * @version $Revision$ $Date$
 **/
public final class Cell {

    /*************************** STATIC DATA MEMBERS **************************/

    /** Print lots of debugging information? Used by debugPrintln() **/
    public static boolean debug = false;

    private static String stdAsyncChannelType = 
        "standard.channel.asynchronous_channel";

    private static String dpuFragmentTypes[] = {
        "standard.base.DPU_DYB", "standard.base.DPU_BIT",
        "standard.base.DPU_CTRL"
    };

    /** Cached standard.e1of CellInterface **/
    private static CellInterface stdAsyncChannel = null;

    /** Cached standard.base.DPU_DYB etc. CellInterfaces **/
    private static CellInterface[] dpuFragmentCells = null;

    /****************************** DATA MEMBERS ******************************/

    /** Constants to identify cell design style type **/
    public static final int UNKNOWN      = -1;
    public static final int ASYNCHRONOUS = 0;
    public static final int SYNCHRONOUS  = 1;
    public static final int OTHER        = 2;

    /** Helper class to encapsulate cell category type constants **/
    public static class Category {
        public static final int UNKNOWN = -1;

        /** Asynchronous cell types **/
        public static final int ASYNC   = 0;        // (unspecified/logic)
        public static final int BUFFER  = 1;
        public static final int SMRBUF  = 2;
        public static final int SLACK   = 3;
        public static final int RESET   = 4;
        public static final int ACK     = 5;
        public static final int SRAM10E = 6;
        public static final int SRAM10B = 7;
        public static final int SRAM6E  = 8;
        public static final int SRAM6B  = 9;
        public static final int SRAM    = 10;
        public static final int XBAR    = 11;
        public static final int CSR     = 12;
        public static final int DFT     = 13;
        public static final int SERIAL  = 14;       // (non-CSR, non-DFT)

        /** Synchronous cell types **/
        public static final int SYNC    = 20;       // (unspecified/logic)
        public static final int SYNCBUF = 21;
        public static final int DFF     = 22;
        public static final int LATCH   = 23;

        private static final int syncOffset = SYNC;

        /** Other, whatever that may be **/
        public static final int OTHER   = 30;

        /** Cell type **/
        int index;

        static final int highLevelFragmentTypes[] = { RESET };
        static final String asyncCategoryDescriptions[] = {
            "Logic", "Buffer", "S/M/R Buffer", "Slack", "Reset",
            "Ack", "SRAM 10T", "SRAM 10T Bit", 
            "SRAM 6T", "SRAM 6T Bit", "SRAM (Other)", "Crossbar", 
            "CSR", "DFT", "Serial (Other)"
        };
        static final String syncCategoryDescriptions[] = {
            "Logic", "Buffer", "Flip-Flop", "Latch"
        };

        /** Uninitialized constructor only for use within class **/
        Category() { index = UNKNOWN; }

        /** General use constructor **/
        public Category(int c) { index = c; }

        public boolean isAsynchronous() { 
            return index != UNKNOWN && index != OTHER && index < syncOffset; 
        }
        public boolean isSynchronous() { 
            return index != UNKNOWN && index != OTHER && index >= syncOffset; 
        }
        public boolean isHighLevelFragment() {
            for (int i=0; i<highLevelFragmentTypes.length; i++)
                if (index == highLevelFragmentTypes[i]) return true;
            return false;
        }
        public boolean isLogic() { return index == ASYNC || index == SYNC; }

        public boolean isUncategorized() { 
            return index == UNKNOWN || index == ASYNC || index == SYNC ||
                   index == OTHER;
        }
        public boolean equals(int cat) { return index == cat; }

        /** List of major categories (Pair<String,Category[]>) **/
        public static List getMajorAsyncCategories() {
            List catList = new ArrayList();

            catList.add(new Pair("Logic", new Category[] { 
                        new Category(ASYNC) }));

            catList.add(new Pair("Buffering", new Category[] {
                        new Category(BUFFER), 
                        new Category(SMRBUF),
                        new Category(SLACK) }));

            catList.add(new Pair("SRAM", new Category[] { 
                        new Category(SRAM10E), 
                        new Category(SRAM10B), 
                        new Category(SRAM6E),
                        new Category(SRAM6B),
                        new Category(SRAM) }));

            catList.add(new Pair("Crossbar", new Category[] { 
                        new Category(XBAR) }));

            catList.add(new Pair("Serial", new Category[] {
                        new Category(CSR), 
                        new Category(DFT), 
                        new Category(SERIAL) }));

            catList.add(new Pair("Other", new Category[] {
                        new Category(ACK),
                        new Category(RESET) }));

            return catList;
        }

        public static List getMajorSyncCategories() {
            List catList = new ArrayList();

            catList.add(new Pair("Logic", new Category[] { 
                       new Category(SYNC) }));

            catList.add(new Pair("Buffering", new Category[] {
                       new Category(SYNCBUF) }));

            catList.add(new Pair("Registers", new Category[] { 
                       new Category(DFF), 
                       new Category(LATCH) }));

            return catList;
        }

        public static List getOtherCategories() {
            List catList = new ArrayList();
            catList.add(new Pair("Other", new Category[] {
                        new Category(OTHER) }));
            return catList;
        }

        /** Buffer, slack, etc. categories **/
        public static Collection getSyncBufferCategories() {
            ArrayList list = new ArrayList();
            list.add(new Category(SYNCBUF));
            return list;
        }

        /** Non-logic and non-buffer categories **/
        public static Collection getFlopCategories() {
            ArrayList list = new ArrayList();
            list.add(new Category(DFF));
            list.add(new Category(LATCH));
            return list;
        }

        public static Collection getSyncLogicCategories() {
            ArrayList list = new ArrayList();
            list.add(new Category(SYNC));
            return list;
        }

        public int hashCode() { return index; }
        public boolean equals(Object c) { 
            return c instanceof Category && 
                index == ((Category)c).index;
        }
        public String toString() { 
            if (isAsynchronous()) return asyncCategoryDescriptions[index]; 
            if (isSynchronous()) 
                return syncCategoryDescriptions[index-syncOffset];
            if (index == OTHER) return "Other";
            return "Unknown";
        }

        public String styleString() {
            if (isAsynchronous()) return "Asynchronous";
            if (isSynchronous()) return "Synchronous";
            if (index == OTHER) return "Other";
            return "Unknown";
        }
    }

    /** Cell type **/
    private Category category;

    /** CellInterface for this cell **/
    private final CellInterface ci;

    /** CadenceInfo for this cell **/
    private final CadenceInfo cinfo;

    /** 
     * Map of parent cell category to instance count.  This is 
     * maintained as a map since, for example, buffers instantiated 
     * within slack cells or srams need to be accounted for differently 
     * from isolated buffers.
     **/
    private Map instCountByCategory = null;

    /**
     * Is this cell a leaf?  For purposes of Cadalyze, a leaf is defined
     * as follows:
     *   If asynchronous: All port nodes belong to channel defchans AND
     *      the cell contains no subcells whose port nodes all belong to
     *      channel defchans; OR has bare nodes or nodes belonging to 
     *      non-channel defchans in its port list AND has no subcells.
     *      In the latter case, isFragment will be true.
     *
     *   If synchronous: The cell has no subcells.
     **/
    private boolean leaf;

    /** Is this cell a fragment? (See leaf for definition) **/
    private boolean fragment;

    /** Is this cell a datapath unit cell constituent (e.g. DPU_DYB) **/
    private boolean dpuFragment = false;
    
    /** 
     * Is this an inlined cell?  The same cell that is instantiated in
     * the design in both inlined and non-inlined forms will have two 
     * distinct Cell objects.
     **/
    private boolean inlined;

    /** This cell's LeafStats, obtained from NetGraphAnalyzer **/
    private LeafStats leafStats = null;

    /** This cell's CellType, as reported by NetGraphAnalyzer **/
    private CellType cellType = null;

    /** This cell's local node count **/
    private int localNodeCount;

    /** Subcell Cell-to-Set<HierName> instance map **/
    private final Map subcellInstNameMap;

    /** Subcell instance HierName-to-Cell map (subcellInstNameMap^-1) **/
    private final Map instNameToCellMap;

    /** HierName-to-NodeProps map **/
    private Map nodePropsMap = null;

    /** Accumulated local node properties of this cell **/
    private NodeProps localNodeProps = null;

    /***************************** CLASS METHODS *****************************/

    /** Constructor **/
    public Cell(final CellInterface ci, final CadenceInfo cinfo,
                final Map subcellInstNameMap,
                final Map instNameToCellMap) {
        this.ci = ci;
        this.cinfo = cinfo;
        this.subcellInstNameMap = subcellInstNameMap;
        this.instNameToCellMap = instNameToCellMap;
        this.instCountByCategory = new HashMap();
        setProperties();
    }

    /** 
     * Constructor helper; Sets the cell's design style type, fragment &
     * dpuFragment properties, and category type.  
     **/
    private void setProperties() {

        String fqcn = ci.getFullyQualifiedType();

        // Asynchronous or synchronous? (rely on synchronous directive)
        int designStyleType = ASYNCHRONOUS;
        if (((Boolean)DirectiveUtils.getTopLevelDirective(ci,
                        DirectiveConstants.SYNCHRONOUS)).booleanValue())
            designStyleType = SYNCHRONOUS;
        if (fqcn.startsWith("vendor.") && designStyleType != SYNCHRONOUS)
            designStyleType = OTHER;

        // Are all port types asynchronous e1ofN channels?
        fragment = (designStyleType == ASYNCHRONOUS ?
                   !ci.allPortSubcellsRefineFrom(stdAsyncChannel) : false);

        // Set dpuFragment (check if the cell refines from
        // DPU_DYB, DPU_BIT, or DPU_CTRL)
        for (int i=0; i<dpuFragmentCells.length; i++) {
            if (ci.eventuallyRefinesFrom(dpuFragmentCells[i])) {
                dpuFragment = true;
                break;
            }
        }

        debugPrintln("    setProperties(): designStyleType="+designStyleType+
                     " fragment="+fragment);
        
        // Set cell Category (this should eventually be made more
        // general and moved to a configuration file).
        category = new Category();
        if (designStyleType == ASYNCHRONOUS) {
            if (fqcn.startsWith("lib.buffer.") || 
                fqcn.matches(".+\\.BUF_1of\\d+\\..+")) {
                if (fqcn.startsWith("lib.buffer.slack.")) 
                    category.index = Category.SLACK;
                else if (fqcn.matches(".+\\.RBUF_1of\\d+\\..+") ||
                         fqcn.matches(".+\\.SBUF_1of\\d+\\..+") ||
                         fqcn.matches(".+\\.MBUF_1of\\d+\\..+"))
                    category.index = Category.SMRBUF;
                else category.index = Category.BUFFER;
            }
            else if (fqcn.startsWith("standard.reset.RAMP_RESET.")) {
                category.index = Category.RESET;
            }
            else if (fqcn.startsWith("lib.sram.")) {
                category.index = Category.SRAM;
                if (fqcn.startsWith("lib.sram.10T.BIT"))
                    category.index = Category.SRAM10B;
                else if (fqcn.startsWith("lib.sram.6T.BIT"))
                    category.index = Category.SRAM6B;
                else if (fqcn.startsWith("lib.sram.dualsram16.") ||
                         fqcn.startsWith("lib.sram.dualsram32.") ||
                         fqcn.startsWith("lib.sram.masksram32.") ||
                         fqcn.startsWith("lib.sram.dualmasksram32.") ||
                         fqcn.startsWith("lib.sram.sram16.") ||
                         fqcn.startsWith("lib.sram.sram32.") ||
                         fqcn.startsWith("lib.sram.10T."))
                    category.index = Category.SRAM10E;
                else if (fqcn.startsWith("lib.sram.masksram128x4.") ||
                         fqcn.startsWith("lib.sram.masksram64x4.") ||
                         fqcn.startsWith("lib.sram.sram128.") ||
                         fqcn.startsWith("lib.sram.sram128x4.") ||
                         fqcn.startsWith("lib.sram.sram128x8.") ||
                         fqcn.startsWith("lib.sram.sram64x4.") ||
                         fqcn.startsWith("lib.sram.6T."))
                    category.index = Category.SRAM6E;
            }
            else if (fqcn.startsWith("lib.router.")) {
                category.index = Category.XBAR;
            }
            else if (fqcn.startsWith("lib.serial.") ||
                     fqcn.startsWith("core.spi4.shared.serial_helpers.")) {
                category.index = Category.SERIAL;
                if (fqcn.startsWith("lib.serial.csr."))
                    category.index = Category.CSR;
                else if (fqcn.startsWith("lib.serial.scan.") ||
                         fqcn.startsWith("lib.serial.s2p.") ||
                         fqcn.startsWith("lib.serial.p2s."))
                    category.index = Category.DFT;
                else if (fqcn.startsWith("lib.serial.state.") ||
                         fqcn.startsWith("lib.serial.sram."))
                    category.index = Category.ASYNC;
            }
            else if (fragment) {
                if (fqcn.startsWith("lib.")) {
                    if (fqcn.startsWith("lib.util.")) {
                        // Assume INVC2/CTREE2 etc. ack logic
                        category.index = Category.ACK;
                    }
                    else {
                        // ARBITER, SAMPLING_BB, etc.
                        category.index = Category.ASYNC;
                    }
                }
                else {
                    // Weeeeird... possibly not even asynchronous
                    category.index = Category.OTHER;
                }
            }
            else {
                // General async (logic)
                category.index = Category.ASYNC;
            }
        }
        else if (designStyleType == SYNCHRONOUS) {
            if (fqcn.startsWith("vendor.artisan.std.gates")) {
                if (fqcn.matches(".+\\.[EMS]?DFF.+") ||
                    fqcn.matches(".+\\.S[EM]DFF.+"))
                    category.index = Category.DFF;
                else if (fqcn.matches(".+\\.TLAT.+"))
                    category.index = Category.LATCH;
                else if (fqcn.matches(".+\\.CLKBUF.+") ||
                         fqcn.matches(".+\\.CLKINV.+") ||
                         fqcn.matches(".+\\.INV.+") ||
                         fqcn.matches(".+\\.BUF.+") ||
                         fqcn.matches(".+\\.TBUF.+") ||
                         fqcn.matches(".+\\.DLY.+"))
                    category.index = Category.SYNCBUF;
                else category.index = Category.SYNC;
            }
            else {
                // General sync (mid-level block)
                category.index = Category.SYNC;
            }
        }
        else category.index = Category.OTHER;
    }

    private void setLeaf(boolean newLeaf) { leaf = newLeaf; }

    private void addToInstCount(final Category cat, int cnt) {
        Integer val = (Integer)instCountByCategory.get(cat);
        if (val==null) instCountByCategory.put(cat,new Integer(cnt));
        else instCountByCategory.put(cat,new Integer(val.intValue()+cnt));
    }

    public boolean isLeaf() { return leaf; }
    public boolean isFragment() { return fragment; }
    public boolean isDpuFragment() { return dpuFragment; }
    public boolean isSynchronous() { return category.isSynchronous(); }
    public final String getName() { return ci.getFullyQualifiedType(); }
    public Category getCategory() { return category; }

    /** Returns instance count of this cell, in all instantiation contexts **/
    public int getInstanceCount() { 
        int cnt = 0;
        for (Iterator ci=instCountByCategory.values().iterator(); ci.hasNext();)
            cnt += ((Integer)ci.next()).intValue();
        return cnt;
    }

    /** 
     * Returns the number of times this cell is instantiated beneath a
     * cell of category 'cat'.  Cell type assignments are made "bottom-up";
     * for example, in the following instantiation hierarchy:
     *
     *   A (type1) -> B (type2) -> C
     *             -> C
     *   D         -> B (type2) -> C
     *
     * (A instantiates a B which instantiates a C, and D instnatiates a 
     * B which instantiates a C), "C" will be counted twice under type1
     * and only once under type2. (In the first instantiation of C, the 
     * type1 of A overrides the type2 classification of B.)
     **/
    public int getInstanceCountInCategory(final Category cat) {
        Integer cnt = ((Integer)instCountByCategory.get(cat));
        return cnt == null ? 0 : cnt.intValue();
    }

    /** catSet==null causes total instance count to be returned **/
    public int getInstanceCountInCategories(final Collection catSet) {
        int sum = 0;
        for (Iterator ci=instCountByCategory.keySet().iterator(); 
             ci.hasNext();) {
            Category c = (Category)ci.next();
            if (catSet == null || catSet.contains(c))
                sum += ((Integer)instCountByCategory.get(c)).intValue();
        }
        return sum;
    }

    /** Returns leaf cell statistics, if they exist **/
    public LeafStats getLeafStats() { 
        return leafStats;
        //if (netgraphAnalyzer != null) return netgraphAnalyzer.getLeafStats();
        //else return null;
    }

    /** Checks if this is a high-level fragment type (e.g. RAMP_RESET) **/
    public boolean isHighLevelFragment() {
        return fragment && category.isHighLevelFragment();
    }

    /** For debug **/
    public Iterator getSubcellPairs() { return ci.getSubcellPairs(); }

    /** Returns the set of all of this cell's subcells **/
    public final Set getSubcellSet() { return subcellInstNameMap.keySet(); }

    /** Returns local instance count of specified subcell **/
    public int getSubcellInstanceCount(final Cell cell) {
        Set instNameSet = (Set)subcellInstNameMap.get(cell);
        return instNameSet == null ? 0 : instNameSet.size();
    }

    /** 
     * Construct the cell's netgraph (if possible), study it with
     * NetGraphAnalyzer, extract relevant info, and then free
     * the associated memory.
     **/
    public void analyzeNetGraph(final CastFileParser cfp, final LayoutInfo li) {
        // Construct NetGraph
        NetGraph netgraph = null;
        if (leaf && ci.hasNetlistBody()) {
            try {
                netgraph = PrsToNet.getNetGraph(ci,cfp,null,null,null,null);
            }
            catch (Exception e) {
                System.err.println("Warning: Couldn't build NetGraph "+
                    "for cell "+getName());
                System.err.println(e);
                netgraph = null;
            }
        }
        if (netgraph == null) return;

        // Set input CellType to include what information we have
        cellType = new CellType();
        if (category.isAsynchronous()) {
            if (fragment) {
                if (category.index == Category.RESET)
                    cellType.setType(CellType.RESET);
                else if (category.index == Category.ACK)
                    cellType.setType(CellType.ACK);
                else {
                    // XBAR, SRAM, etc.
                    cellType.setType(CellType.OTHER);
                }
            }
        }
        else cellType.setType(CellType.OTHER);

        // Have NetGraphAnalyzer take a look
        NetGraphAnalyzer netgraphAnalyzer = 
            new NetGraphAnalyzer(ci,netgraph,cellType);
        nodePropsMap = netgraphAnalyzer.analyzeCell();
        leafStats = netgraphAnalyzer.getLeafStats();
        leafStats.setLayoutProperties(li,getName().toString());
        cellType = netgraphAnalyzer.getCellType();
    }

    /**
     * Sets node properties of all local nodes of all cells in the
     * hierarchy, including this one.  This cell's port nodes are treated
     * specially; they are set to be non-local if they only connect to
     * a single leaf cell or if they are either undriven or unloaded.  
     * (All other nodes in a particular subcell type are set to be local 
     * iff the node is not connected to any parent node in any instantiation 
     * of that cell.)
     **/
    public void setNodeProperties() {
        // Set nodePropsMap in all subcells
        setNodePropsRecursively();

        // For the top level cell ONLY, explicitly make single-connection
        // nodes non-local.
        for (Iterator ni=nodePropsMap.entrySet().iterator(); ni.hasNext();) {
            Entry entry = (Entry) ni.next();
            HierName name = (HierName) entry.getKey();
            NodeProps props = (NodeProps) entry.getValue();
            debugPrintln("Port node "+name+": "+props.getNumConnections()+
                         " connections");
            if (((AliasedMap) cinfo.getPortNodes()).contains(name) &&
                (props.getNumConnections() <= 1 ||
                 !props.isDriven() || !props.isLoaded())) {
                props.setNonLocal();
            }
        }
    }

    /** Private recursive workhorse function for setNodeProps() **/
    private void setNodePropsRecursively() {
        if (nodePropsMap != null) return;

        debugPrintln("Setting node properties of "+getName());
        nodePropsMap = new HashMap();

        // Descend down to subcells -- build nodePropsMap bottom-up
        for (Iterator sci=subcellInstNameMap.keySet().iterator();
             sci.hasNext();) {
            
            // Set the subcell's NodeProps
            Cell subcell = (Cell) sci.next();
            subcell.setNodePropsRecursively();

            // Get all instance names of this subcell type
            Set instNameSet = (Set) subcellInstNameMap.get(subcell);

            // Connect all ports of this subcell type
            AliasedMap ports = subcell.cinfo.getPortNodes();
            for (Iterator pi=ports.getCanonicalKeys(); pi.hasNext();) {
                HierName portName = (HierName) pi.next();
                // Skip if port not used by the subcell
                if (Boolean.FALSE.equals(ports.getValue(portName))) continue;

                // Look up the NodeProps of this port of the subcell
                NodeProps portProps = 
                    (NodeProps) subcell.nodePropsMap.get(portName);
                if (portProps == null) continue;    // Vdd, GND

                // Connect this port of all instances of this subcell type
                for (Iterator ni=instNameSet.iterator(); ni.hasNext();) {
                    HierName instName = (HierName) ni.next();
                    HierName nodeName = HierName.append(instName,portName);
                    HierName cnodeName= (HierName)
                        cinfo.getLocalNodes().getCanonicalKey(nodeName);
                    if (cnodeName == null) {
                        debugPrintln("  Couldn't find canonical alias of "+portName+" of "+
                                     "instance "+instName+" ("+nodeName+") "+
                                     "in type "+subcell.getName()+")");
                        continue;
                    }
                    NodeProps nodeProps = 
                        (NodeProps) nodePropsMap.get(cnodeName);
                    if (nodeProps == null) {
                        nodeProps = new NodeProps();
                        nodePropsMap.put(cnodeName,nodeProps);
                    }
                    nodeProps.connectNode(portProps);
                    //debugPrintln(getName()+": Connecting "+cnodeName+" to "+
                    //    nodeName+" ("+portName+") of cell type "+
                    //    subcell.getName());
                }
            }
        }
    }

    /**
     * To be called after setNodeProps has ben called on the top-level cell.  
     * (Warning: will call setNodeProps() if it hasn't already been called.)
     * This method tallies properties of the cell's local nodes.
     **/
    public NodeProps getLocalNodeProps() {
        if (nodePropsMap == null) setNodeProperties();
        if (localNodeProps != null) return localNodeProps;

        localNodeProps = new NodeProps();

        for (Iterator npi=nodePropsMap.entrySet().iterator(); npi.hasNext();) {
            Entry entry = (Entry) npi.next();
            NodeProps node = (NodeProps) entry.getValue();
            if (node.isLocal()) localNodeProps.accumulateNode(node);
            else debugPrintln(" Not local: "+(HierName)entry.getKey());
        }
        debugPrintln("Cell "+getName()+": "+localNodeProps.getNumNodes()+
                     " local nodes accumulated.");
        debugPrintln("  aveDriveStrength="+localNodeProps.getAverageDriveStrength());
        debugPrintln("  numConnections="+localNodeProps.getNumConnections()+
                     " ("+localNodeProps.numLocalLeafNodes+" local nodes)");
        return localNodeProps;
    }


    /** Generates an HTML leaf cell report page for this cell **/
    public void printHtmlReport(String designName, String outputDir,
                                float medGateLength) {
        HtmlPage page;
        try {
            page = new HtmlPage(designName,outputDir,getName()+".html",
                                getName());
        }
        catch (IOException e) {
            System.err.println("Couldn't create HTML page "+outputDir+"/"+
                               getName());
            System.err.println(e);
            return;
        }
        // General info table
        page.summaryTable();
        page.summaryTableLine("Design style:",category.styleString());
        page.summaryTableLine("Cell category:",category.toString());
        page.summaryTableLine("Instance count:",getInstanceCount());
        page.summaryTableLine("Fragment:", fragment ? "Yes" : "No");
        if (cellType != null) cellType.toHtml(page);
        page.summaryTableEnd();

        // Standard LeafStats info
        if (leafStats != null) leafStats.printHtmlStats(page);

        // Output any cell anomalies
        if (cellType != null) {
            page.section("Analysis Warnings");
            cellType.printHtmlAnomalyList(page);
        }

        if (localNodeProps != null) {
            page.section("Local Node Statistics");
            localNodeProps.printHtmlStats(page,medGateLength);
        }

        // Print subcell list
        if (subcellInstNameMap.keySet().size() > 0) {
            page.section("Subcells");
            List cols = new ArrayList();
            cols.add(new Pair("Subcell Type",Boolean.FALSE));
            cols.add(new Pair("Instance count",Boolean.TRUE));
            page.reportTable(cols);
            for (Iterator pi=getSortedSubcellSet().iterator(); pi.hasNext();) {
                Pair p = (Pair) pi.next();
                Cell subcell = (Cell) p.getFirst();
                int subcellInstCnt = ((Integer) p.getSecond()).intValue();
                cols = new ArrayList();
                cols.add("<A HREF="+subcell.getName()+".html>"+
                         subcell.getName()+"</A>");
                cols.add(((Integer) p.getSecond()).toString());
                page.reportTableLine(cols);
            }
            page.reportTableEnd();
        }

        page.close();
    }

    /** 
     * Returns a list of subcells w/ instance counts (Pair<Cell,Integer>),
     * sorted by instance count 
     **/
    public SortedSet getSortedSubcellSet() {
        class SubcellCompare implements Comparator {
            public int compare(Object o1, Object o2) {
                Pair p1 = (Pair) o1, p2 = (Pair) o2;
                if (p1.getFirst() == p2.getFirst()) return 0;
                if (((Integer)p2.getSecond()).intValue() >
                    ((Integer)p1.getSecond()).intValue())
                    return 1;
                else return -1;
            }
        }
        TreeSet set = new TreeSet(new SubcellCompare());
        for (Iterator ei=subcellInstNameMap.entrySet().iterator(); 
             ei.hasNext();) {
            Entry entry = (Entry) ei.next();
            set.add(new Pair(entry.getKey(),
                             new Integer(((Set) entry.getValue()).size())));
        }
        return set;
    }



    /***************************** STATIC METHODS *****************************/

    private static void debugPrintln(final String s) {
        if (debug) System.err.println(s);
    }

    private static void debugPrint(final String s) {
        if (debug) System.err.print(s);
    }

    /**
     * Adds this cell and its entire subcell tree to fqcnToCellMap.
     * Guaranteed to not return null.  If parentIsInlined, then
     * this cell won't be added 
     **/
    private static Cell addCellToMap(final Cadencize cadencize,
                                     final CellInterface ci, 
                                     final Map fqcnToCellMap) {

        String fqcn = ci.getFullyQualifiedType();
        debugPrintln("Adding "+fqcn);

        HashMap subcellInstNameMap = new HashMap();
        HashMap instNameToCellMap = new HashMap();

        Cell cell = new Cell(ci,cadencize.convert(ci),subcellInstNameMap,
                             instNameToCellMap);

        debugPrintln("    category="+cell.getCategory()+" fragment="+
                     cell.isFragment());

        boolean containsSubcells = false;
        boolean containsAllDpuFragmentSubcells = true;

        List dpuFragmentCells = new ArrayList();
        if (!ci.containsCompletePrs()) {
            for (Iterator scit=ci.getAllSubcellPairs(); scit.hasNext();) {
                Pair subcellPair = (Pair)scit.next();
                HierName name = (HierName) subcellPair.getFirst();
                CellInterface subci = (CellInterface) subcellPair.getSecond();

                // Look up inline info
                boolean subIsInlined = ci.isInlinedSubcell(name);
                boolean subFromInlinedParent = ci.parentIsInlinedSubcell(name);

                // Skip non-prs cells and any inlined subcells
                if (!subci.hasRealProductionRule() && !subci.hasNetlistBody() 
                    || subIsInlined) continue;

                // Look up/create this subcell's Cell; this will probably
                // be thrown out if it's inlined (or if this cell was inlined)
                String subFqcn = subci.getFullyQualifiedType();
                Cell subCell = (Cell)fqcnToCellMap.get(subFqcn);
                if (subCell == null) {
                    subCell = addCellToMap(cadencize, subci, fqcnToCellMap);
                }

                containsSubcells = true;
                containsAllDpuFragmentSubcells &= subCell.dpuFragment;
                if (subCell.dpuFragment) dpuFragmentCells.add(subCell);

                // Update subcell instance count map
                Set instNameSet = (Set) subcellInstNameMap.get(subCell);
                if (instNameSet == null) {
                    instNameSet = new HashSet();
                    instNameSet.add(name);
                    subcellInstNameMap.put(subCell,instNameSet);
                }
                else if (instNameSet != null) {
                    instNameSet.add(name);
                }
                instNameToCellMap.put(name,subCell);
            }
        }
        cell.setLeaf(!cell.dpuFragment &&
                     (!containsSubcells || 
                      containsSubcells && containsAllDpuFragmentSubcells));

        if (containsSubcells && !containsAllDpuFragmentSubcells &&
            dpuFragmentCells.size() > 0) {
            // Corner case: a DPU fragment cell was used in a non-DPU 
            // environment (e.g. lib.sram.6T.ENV_4x4)
            for (Iterator sci=dpuFragmentCells.iterator(); sci.hasNext();) {
                ((Cell)sci.next()).setLeaf(true);
            }
        }

        debugPrintln("    leaf="+cell.isLeaf());

        // Add the cell to fqcnToCell map
        /*if (!skipAddToMap)*/fqcnToCellMap.put(fqcn,cell);
        return cell;
    }

    /** 
     * Recursively tallies instance counts of 'cell' and all of its subcells.
     * The Category is "sticky" in the sense that as soon as it becomes
     * non-"UNSPEC" it never changes again.  Thus the highest level cell with
     * an assigned Category overrides the categorization of any of its
     * subcells.
     **/
    private static void tallyInstanceCounts(final Cell cell, int cnt,
                                            Category cat) {
        if (cat.isUncategorized()) cat = cell.category;
        cell.addToInstCount(cat,cnt);
        for (Iterator scit=cell.subcellInstNameMap.entrySet().iterator(); 
             scit.hasNext();) {
            Entry subcellEntry = (Entry) scit.next();
            tallyInstanceCounts((Cell)subcellEntry.getKey(),
                cnt * ((Set)subcellEntry.getValue()).size(), cat);
        }
    }

    /** 
     * Recursively processes this cell and all of its subcells 
     * Returns a FQCN-to-Cell Map<String,Cell> of all cells in
     * the design.
     **/
    public static Map processHierarchy(final CellInterface ci,
                                       final CastFileParser cfp,
                                       boolean verbose)
                                       throws Exception {
        Cadencize cadencize = new Cadencize(false);
        HashMap fqcnToCellMap = new HashMap();
        if (verbose) System.err.println("Building cell hierarchy...");
        lookupBaseCells(cfp);
        Cell cell = addCellToMap(cadencize,ci,fqcnToCellMap);
        tallyInstanceCounts(cell,1,new Category());
        return fqcnToCellMap;
    }

    /**
     * Look up CellInterfaces of standard base cells that are needed
     * during cell processing.
     **/
    private static void lookupBaseCells(final CastFileParser cfp) 
        throws Exception {
        if (stdAsyncChannel == null)
            stdAsyncChannel = cfp.getFullyQualifiedCell(stdAsyncChannelType);
        if (dpuFragmentCells == null) {
            dpuFragmentCells = new CellInterface[dpuFragmentTypes.length];
            for (int i=0; i<dpuFragmentTypes.length; i++)
                dpuFragmentCells[i] = 
                    cfp.getFullyQualifiedCell(dpuFragmentTypes[i]);
        }
    }
}
