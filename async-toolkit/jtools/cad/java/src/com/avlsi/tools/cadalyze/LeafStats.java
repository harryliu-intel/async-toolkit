/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.SortedMap;
import java.util.StringTokenizer;
import java.util.TreeMap;

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
import com.avlsi.util.htmlWriter.HtmlWriter;

import com.avlsi.tools.cadalyze.NetGraphAnalyzer.EdgeType;
import com.avlsi.tools.cadalyze.NetGraphAnalyzer.NodeType;
import com.avlsi.tools.lvs.NetGraph;

public final class LeafStats {

    int numNMOS;            // Number of NMOS transistors not in gates
    int numPMOS;            // Number of PMOS transistors not in gates
    int numGateNMOS;        // Number of NMOS transistors in gates
    int numGatePMOS;        // Number of PMOS transistors in gates
    int numGates;           // Number of gates in the cell
    int numStaticizers;     // Number of staticizers in the cell
    int numUnfoldedEdges;   // Transistor count adjusted for folding

    float areaNMOS;         // Gate area of NMOS transistors not in gates
    float areaPMOS;         // Gate area of PMOS transistors not in gates
    float areaGateNMOS;     // Gate area of NMOS transistors in gates
    float areaGatePMOS;     // Gate area of PMOS transistors in gates

    Map nodeTypeCounts;
    Map edgeTypeCounts;
    Map edgeTypeCountsUnfolded;
    Map edgeTypeAreas;
    Map edgeTypeWidths;

    SortedMap lengthCounts; // transistor length [nm] -> count map

    float unclassifiedFraction;

    float logicEfficiency;  // Set by printHtmlStats (should be elsewhere)

    /** !=1 only when the LeafStats represents a leaf cell group **/
    private int numCells;

    /** Layout bounding box area **/
    double layoutArea = -1.0F;

    /** Layout density factor **/
    float densityFactor = -1.0F;

    /** Accumulated transistor area of cells with layout info **/
    double layoutTransistorArea = 0.0;

    /** Number of cells with layout info **/
    int cellsWithLayoutInfo = 0;

    /** True if this LeafStats represents an aggregation of cell statistics **/
    private boolean aggegatedStats;

    /** Returns the total number of transistors in this leaf cell **/
    public int getTransistorCount() {
        return numNMOS + numPMOS + numGateNMOS + numGatePMOS;
    }

    /** Returns the total number of transistors, adjusted for folding **/
    public int getUnfoldedTransistorCount() { return numUnfoldedEdges; }

    /** Returns the total transistor area in this leaf cell **/
    public float getTransistorArea() {
        return areaNMOS + areaPMOS + areaGateNMOS + areaGatePMOS;
    }

    public int getNumNMOS() { return numNMOS+numGateNMOS; }
    public int getNumPMOS() { return numPMOS+numGatePMOS; }
    public int getNumStaticizers() { return numStaticizers; }
    public float getAreaNMOS() { return areaNMOS+areaGateNMOS; }
    public float getAreaPMOS() { return areaPMOS+areaGatePMOS; }

    /** Returns total node count **/
    public int getNodeCount() {
        int total = 0;
        for (Iterator ci=nodeTypeCounts.values().iterator(); ci.hasNext();) {
            Integer cnt = (Integer)ci.next();
            total += (cnt==null ? 0 : cnt.intValue());
        }
        return total;
    }

    /** Returns node count of the given NodeType **/
    public int getNodeTypeCount(final NodeType type) {
        Integer cnt = (Integer)nodeTypeCounts.get(type);
        return cnt==null ? 0 : cnt.intValue();
    }

    /** Returns edge count of the given EdgeType **/
    public int getEdgeTypeCount(final EdgeType type) {
        Integer cnt = (Integer)edgeTypeCounts.get(type);
        return cnt==null ? 0 : cnt.intValue();
    }

    /** Returns unfolded edge count of the given EdgeType **/
    public int getEdgeTypeCountUnfolded(final EdgeType type) {
        Double cnt = (Double)edgeTypeCountsUnfolded.get(type);
        return cnt==null ? 0 : cnt.intValue();
    }

    /** Returns total area of the given EdgeType **/
    public float getEdgeTypeArea(final EdgeType type) {
        Double area = (Double)edgeTypeAreas.get(type);
        return area==null ? 0 : area.floatValue();
    }

    /** Returns total transistor width of the given EdgeType **/
    public float getEdgeTypeWidth(final EdgeType type) {
        Double width = (Double)edgeTypeWidths.get(type);
        return width==null ? 0 : width.floatValue();
    }

    /** Returns the average (unfolded) transistor width of all edges **/
    public float getAverageTransistorWidth() {
        double totalWidth = 0.0;
        for (Iterator wi=edgeTypeWidths.values().iterator(); wi.hasNext();) {
            totalWidth += ((Double)wi.next()).doubleValue();
        }
        return (float)totalWidth/getUnfoldedTransistorCount();
    }

    /** 
     * Returns the average (unfolded) transistor width of edges belonging
     * to any of the types in 'edgeTypes'.
     **/
    public float getAverageTransistorWidth(final Collection edgeTypes) {
        double totalWidth = 0.0;
        double totalCnt = 0.0;
        for (Iterator ti=edgeTypes.iterator(); ti.hasNext();) {
            EdgeType type = (EdgeType) ti.next();
            Double cnt = (Double) edgeTypeCountsUnfolded.get(type);
            if (cnt != null) {
                totalCnt += cnt.doubleValue();
                totalWidth += ((Double)edgeTypeWidths.get(type)).doubleValue();
            }
        }
        return totalCnt == 0.0 ? 0.0F : (float)(totalWidth / totalCnt);
    }

    /** Returns the fraction of nodes that couldn't be classified **/
    public float getUnclassifiedNodeFraction() {
        Integer cnt = 
            (Integer)nodeTypeCounts.get(new NodeType(NodeType.UNCLASSIFIED));
        if (cnt != null && (cnt.intValue() > 0) 
            || unclassifiedFraction == -1.0F) {
            int total = getNodeCount();
            unclassifiedFraction =
                cnt==null || total==0 ? 0.0F : cnt.floatValue() / total;
        }
        return unclassifiedFraction;
    }

    /** Returns the median transistor legnth, in meters. **/
    public float getMedianTransistorLength() {
        int medianCount = getTransistorCount() / 2;
        for (Iterator li=lengthCounts.entrySet().iterator(); li.hasNext();) {
            Entry entry = (Entry) li.next();
            medianCount -= ((Integer) entry.getValue()).intValue();
            if (medianCount <= 0) 
                return ((Integer) entry.getKey()).floatValue() * 1e-9F;
        }
        // Should never get this case! (Unless no transistors)
        return -1.0e-6F;
    }

    /** Reclassify unclassified node and edge counts to "OTHER" **/
    public void convertUnclassifiedToOther() {
        recategorizeCountsOfType(new NodeType(NodeType.UNCLASSIFIED),
                                 new NodeType(NodeType.OTHER),
                                 nodeTypeCounts);
        recategorizeCountsOfType(new EdgeType(EdgeType.UNCLASSIFIED),
                                 new EdgeType(EdgeType.OTHER),
                                 edgeTypeCounts);
        recategorizeDoublesOfType(new EdgeType(EdgeType.UNCLASSIFIED),
                                 new EdgeType(EdgeType.OTHER),
                                 edgeTypeCountsUnfolded);
        recategorizeDoublesOfType(new EdgeType(EdgeType.UNCLASSIFIED),
                                 new EdgeType(EdgeType.OTHER),
                                 edgeTypeAreas);
        recategorizeDoublesOfType(new EdgeType(EdgeType.UNCLASSIFIED),
                                 new EdgeType(EdgeType.OTHER),
                                 edgeTypeWidths);
    }

    /** Number of cells this LeafStats represents **/
    public int getNumCells() { return numCells; }

    /** Average transistor area per cell **/
    public float getAverageTransistorAreaPerCell() { 
        return (float) getTransistorArea() / numCells;
    }

    /** Average transistor count per cell **/
    public float getAverageTransistorCountPerCell(boolean folded) {
        if (folded)
            return (float) getTransistorCount() / numCells;
        else
            return (float) getUnfoldedTransistorCount() / numCells;
    }

    /** Average number of specified node type per cell **/
    public float getAverageNodeCountPerCell(final NodeType type) {
        Integer cnt = (Integer) nodeTypeCounts.get(type);
        return cnt == null ? 0.0F : cnt.floatValue()/numCells;
    }

    /** Set layout area and density factor **/
    public void setLayoutProperties(final LayoutInfo li, 
                                    final String cellName) {
        layoutArea = li.getLayoutArea(cellName);
        layoutTransistorArea = getTransistorArea();
        densityFactor = li.getTransistorDensityFactor(cellName, (float)
                                                      layoutTransistorArea);
        cellsWithLayoutInfo = 1;
    }

    /** 
     * Initializes all counts to 0. Call tallyMarks() to calculate
     * statistics
     **/
    LeafStats() {
        nodeTypeCounts = new HashMap();
        edgeTypeCounts = new HashMap();
        edgeTypeCountsUnfolded = new HashMap();
        edgeTypeAreas  = new HashMap();
        edgeTypeWidths = new HashMap();
        lengthCounts   = new TreeMap();
        layoutArea = 0.0;
        layoutTransistorArea = 0.0;
        cellsWithLayoutInfo = 0;
        numCells = 0;
        aggegatedStats = false;
    }

    /**
     * Calculates basic statistics from the NetGraph, sets
     * node type and edge type maps from those passed.
     **/
    LeafStats(final NetGraph netgraph,
               final Map nodeTypeCounts,
               final Map edgeTypeCounts,
               final Map edgeTypeCountsUnfolded,
               final Map edgeTypeAreas,
               final Map edgeTypeWidths) {
        lengthCounts = new TreeMap();
        countDevices(netgraph);
        this.nodeTypeCounts = new HashMap(nodeTypeCounts);
        this.edgeTypeCounts = new HashMap(edgeTypeCounts);
        this.edgeTypeCountsUnfolded = new HashMap(edgeTypeCountsUnfolded);
        this.edgeTypeAreas = new HashMap(edgeTypeAreas);
        this.edgeTypeWidths = new HashMap(edgeTypeWidths);
        numCells = 1;
        aggegatedStats = false;
    }

    /**
     * Adds statistics from the passed LeafStats, weighted by
     * instance count
     **/
    void addStatistics(int instCount, final LeafStats stats) {
        numNMOS         += instCount * stats.numNMOS;
        numPMOS         += instCount * stats.numPMOS;
        numGateNMOS     += instCount * stats.numGateNMOS;
        numGatePMOS     += instCount * stats.numGatePMOS;
        numGates        += instCount * stats.numGates;
        numStaticizers  += instCount * stats.numStaticizers;
        areaNMOS        += instCount * stats.areaNMOS;
        areaPMOS        += instCount * stats.areaPMOS;
        areaGateNMOS    += instCount * stats.areaGateNMOS;
        areaGatePMOS    += instCount * stats.areaGatePMOS;
        numUnfoldedEdges += instCount * stats.numUnfoldedEdges;

        addWeightedIntegersByType(instCount,stats.nodeTypeCounts,
                                  nodeTypeCounts);
        addWeightedIntegersByType(instCount,stats.edgeTypeCounts,
                                  edgeTypeCounts);
        addWeightedDoublesByType(instCount,stats.edgeTypeCountsUnfolded,
                                  edgeTypeCountsUnfolded);
        addWeightedDoublesByType(instCount,stats.edgeTypeAreas,edgeTypeAreas);
        addWeightedDoublesByType(instCount,stats.edgeTypeWidths,edgeTypeWidths);

        // Add transistor length counts
        for (Iterator li=stats.lengthCounts.entrySet().iterator(); 
             li.hasNext();) {
            Entry entry = (Entry) li.next();
            int len = ((Integer) entry.getKey()).intValue();
            int cnt = ((Integer) entry.getValue()).intValue();
            addGateLength(instCount*cnt,len);
        }

        numCells += instCount * stats.numCells;
        if (stats.cellsWithLayoutInfo > 0) {
            layoutArea += instCount * stats.layoutArea;
            layoutTransistorArea += instCount * stats.layoutTransistorArea;
            cellsWithLayoutInfo += instCount * stats.cellsWithLayoutInfo;
        }
        aggegatedStats = true;
    }

    /** Preliminary statistics that are tallied upfront **/
    private void countDevices(final NetGraph netgraph) {
        for (Iterator t=netgraph.getEdges().iterator(); t.hasNext();) {
            NetGraph.NetEdge e = (NetGraph.NetEdge) t.next();
            if (e.type == DeviceTypes.N_TYPE) {
                if (e.library && !e.floating) {
                    numGateNMOS++;
                    areaGateNMOS += e.width * e.length;
                }
                else {
                    numNMOS++;
                    areaNMOS += e.width * e.length;
                }
            }
            else if (e.type == DeviceTypes.P_TYPE) {
                if (e.library && !e.floating) {
                    numGatePMOS++;
                    areaGatePMOS += e.width * e.length;
                }
                else {
                    numPMOS++;
                    areaPMOS += e.width * e.length;
                }
            }
            addGateLength(1,(int)(e.length*1e9));
        }
        for (Iterator t = netgraph.getNodes().iterator(); t.hasNext();) {
            NetGraph.NetNode n = (NetGraph.NetNode) t.next();
            if (n.getGate() != null) numGates++;
            if (n.getStaticizer() != null) numStaticizers++;
        }
        numUnfoldedEdges = netgraph.getUnfoldedTransistorCount();
    }

    /** Adds 'cnt' transistors of length 'nanometerLength' (in nm obviously) **/
    private void addGateLength(int cnt, int nanometerLength) {
        Integer intLen = new Integer(nanometerLength);
        Integer intCnt = (Integer) lengthCounts.get(intLen);
        if (intCnt == null) lengthCounts.put(intLen,new Integer(cnt));
        else lengthCounts.put(intLen,new Integer(intCnt.intValue()+cnt));
    }

    private static void recategorizeCountsOfType(final Object fromType,
                                                 final Object toType,
                                                 final Map map) {
        Integer fromCntInt = (Integer)map.get(fromType);
        Integer toCntInt   = (Integer)map.get(toType);
        int fromCnt = (fromCntInt == null ? 0 : fromCntInt.intValue());
        int toCnt   = (toCntInt == null ? 0 : toCntInt.intValue());
        map.put(toType,new Integer(fromCnt + toCnt));
        map.remove(fromType);
    }

    private static void recategorizeDoublesOfType(final Object fromType,
                                                  final Object toType,
                                                  final Map map) {
        Double fromValDouble = (Double)map.get(fromType);
        Double toValDouble   = (Double)map.get(toType);
        double fromCnt = (fromValDouble == null ? 0 : 
                          fromValDouble.doubleValue());
        double toCnt   = (toValDouble == null ? 0 : 
                          toValDouble.doubleValue());
        map.put(toType,new Double(fromCnt + toCnt));
        map.remove(fromType);
    }

    private static void addWeightedIntegersByType(int weight, 
                                                  final Map from, 
                                                  final Map to) {
        for (Iterator ki=from.entrySet().iterator(); ki.hasNext();) {
            Entry entry = (Entry) ki.next();
            int inc = weight * ((Integer)entry.getValue()).intValue();
            Integer toVal = (Integer) to.get(entry.getKey());
            if (toVal == null) to.put(entry.getKey(),new Integer(inc));
            else to.put(entry.getKey(),new Integer(inc+toVal.intValue()));
        }
    }

    private static void addWeightedFloatsByType(int weight, 
                                                final Map from, 
                                                final Map to) {
        for (Iterator ki=from.entrySet().iterator(); ki.hasNext();) {
            Entry entry = (Entry) ki.next();
            float inc = weight * ((Float)entry.getValue()).floatValue();
            Float toVal = (Float) to.get(entry.getKey());
            if (toVal == null) to.put(entry.getKey(),new Float(inc));
            else to.put(entry.getKey(),new Float(inc+toVal.floatValue()));
        }
    }

    private static void addWeightedDoublesByType(int weight, 
                                                 final Map from, 
                                                 final Map to) {
        for (Iterator ki=from.entrySet().iterator(); ki.hasNext();) {
            Entry entry = (Entry) ki.next();
            double inc = weight * ((Double)entry.getValue()).doubleValue();
            Double toVal = (Double) to.get(entry.getKey());
            if (toVal == null) to.put(entry.getKey(),new Double(inc));
            else to.put(entry.getKey(),new Double(inc+toVal.doubleValue()));
        }
    }

    /** Statistics Report Summary **/
    void printSummary(boolean verbose) {
        // Set up numeric formats
        DecimalFormat formWidth = (DecimalFormat)DecimalFormat.getInstance();
        DecimalFormat formPercent = (DecimalFormat)DecimalFormat.getInstance();
        formWidth.applyPattern("0.00");
        formPercent.applyPattern("#0.00%");

        // Report node count distribution
        Iterator nt;
        if (verbose) {
            System.out.println("Node type distribution:");
            nt = nodeTypeCounts.keySet().iterator();
            while (nt.hasNext()) {
                NodeType type = (NodeType)nt.next();
                int cnt = ((Integer)nodeTypeCounts.get(type)).intValue();
                System.out.println(" "+type.toPaddedString()+": "+cnt);
            }
        }
        // Report edge count, ave width, and area percentage distributions
        if (verbose) System.out.println(
            "Edge type count distribution & total area (unfolded):");
        nt = edgeTypeCounts.keySet().iterator();
        int logicCount = 0;
        float logicArea = 0.0F;
        while (nt.hasNext()) {
            EdgeType type = (EdgeType)nt.next();
            int cnt = ((Integer)edgeTypeCounts.get(type)).intValue();
            int ucnt = ((Double)edgeTypeCountsUnfolded.get(type)).intValue();
            float area = ((Double)edgeTypeAreas.get(type)).floatValue();
            float width = ((Double)edgeTypeWidths.get(type)).floatValue();
            if (type.isLogicType()) {
                logicCount += cnt;
                logicArea += area;
            }
            if (verbose) {
                System.out.println(" " +type.toPaddedString()+": " + 
                    cnt + "(" + ucnt + ") " +
                    formWidth.format(width/cnt*1e6) + "(" +
                    formWidth.format(width/ucnt*1e6) + ") um / " + 
                    formPercent.format((float)cnt/getTransistorCount()) + " " +
                    formPercent.format(area/getTransistorArea()));
            }
        }
        if (verbose) 
            System.out.println("Transistor Count and Area Summary:");
        System.out.println(" Logic Transistor Count: " +
            (float)logicCount/getTransistorCount()*100 + "%");
        System.out.println(" Logic Transistor Area:  " +
            logicArea/getTransistorArea()*100 + "%");
    }

    void printStats(boolean verbose) {
        if (verbose) {
            System.out.println(
                "Transistor counts:\n" +
                " Gates           = " + numGates + "\n" +
                " Staticizers     = " + numStaticizers + "\n" +
                " NMOS (Floating) = " + numNMOS + "\n" +
                " PMOS (Floating) = " + numPMOS + "\n" +
                " NMOS (Gates)    = " + numGateNMOS + "\n" +
                " PMOS (Gates)    = " + numGatePMOS + "\n" +
                " Total           = " + getTransistorCount() + "\n" +
                " Total Unfolded  = " + getUnfoldedTransistorCount() + "\n" +
                " TotalArea       = " + getTransistorArea());
        }
        printSummary(verbose);
    }

    /** 
     * Writes node, transistor, and summary sections to the specified
     * HtmlPage.  As a side effect, sets logicEfficiency for use by
     * getLogicEfficiency().  (Should be generated more cleanly.)
     **/
    void printHtmlStats(final HtmlPage page) {

        // Set up numeric formats
        DecimalFormat formFloat = 
            (DecimalFormat)DecimalFormat.getInstance();
        DecimalFormat formPercent = 
            (DecimalFormat)DecimalFormat.getInstance();
        formFloat.applyPattern("#0.00");
        formPercent.applyPattern("#0.00%");

        // Report node count distribution
        page.section("Node Distribution");
        ArrayList cols = new ArrayList();
        cols.add(new Pair("Category",Boolean.FALSE));
        cols.add(new Pair("Count",Boolean.TRUE));
        cols.add(new Pair("Percentage",Boolean.TRUE));
        page.reportTable(cols);

        // Output each node type
        Iterator nt;
        for (nt = nodeTypeCounts.keySet().iterator(); nt.hasNext();) {
            NodeType type = (NodeType)nt.next();
            int cnt = ((Integer)nodeTypeCounts.get(type)).intValue();
            if (cnt == 0) continue;
            cols = new ArrayList();
            cols.add(type.toString());
            cols.add(String.valueOf(cnt));
            cols.add(formPercent.format((float)cnt/getNodeCount()));
            page.reportTableLine(cols);
        }
        page.reportTableEnd();

        // Report edge count, ave width, and area percentage distributions
        page.section("Transistor Distribution");
        cols = new ArrayList();
        cols.add(new Pair("Category",Boolean.FALSE));
        cols.add(new Pair("Count (folded)",Boolean.TRUE));
        cols.add(new Pair("Average width (um)",Boolean.TRUE));
        cols.add(new Pair("Percentage by count",Boolean.TRUE));
        cols.add(new Pair("Percentage by area",Boolean.TRUE));
        page.reportTable(cols);

        // Output each edge type
        int logicCount = 0, logicUnfoldedCount = 0;
        float logicArea = 0.0F;
        for (nt = edgeTypeCounts.keySet().iterator(); nt.hasNext();) {
            EdgeType type = (EdgeType)nt.next();
            int cnt = ((Integer)edgeTypeCounts.get(type)).intValue();
            int ucnt = ((Double)edgeTypeCountsUnfolded.get(type)).intValue();
            if (cnt == 0 && ucnt == 0) continue;
            float area = ((Double)edgeTypeAreas.get(type)).floatValue();
            float width = ((Double)edgeTypeWidths.get(type)).floatValue();
            if (type.isLogicType()) {
                logicCount += cnt;
                logicUnfoldedCount += ucnt;
                logicArea += area;
            }
            cols = new ArrayList();
            cols.add(type.toString());
            cols.add(String.valueOf(ucnt)+" ("+String.valueOf(cnt)+")"); 
            cols.add(formFloat.format(width/ucnt*1e6));
            cols.add(formPercent.format((float)ucnt/
                                        getUnfoldedTransistorCount()));
            cols.add(formPercent.format((float)area/getTransistorArea()));
            page.reportTableLine(cols);
        }
        page.reportTableEnd();

        // Summary
        page.section("Summary");
        page.summaryTable();
        if (aggegatedStats) {
            page.summaryTableLine("Number of cells:",numCells);
            page.summaryTableLine("Average transistor count per cell (folded):",
                formFloat.format(getAverageTransistorCountPerCell(false))+" ("+
                formFloat.format(getAverageTransistorCountPerCell(true))+")");
            page.summaryTableLine("Average transistor area per cell:",
                formFloat.format(getAverageTransistorAreaPerCell()*1e12));
            float numInputRails = getAverageNodeCountPerCell(
                    new NodeType(NodeType.INPUT_DATA_RAIL));
            if (numInputRails > 0.0F) {
                float numInputEnables = getAverageNodeCountPerCell(
                        new NodeType(NodeType.INPUT_ENABLE));
                float numOutputRails = getAverageNodeCountPerCell(
                        new NodeType(NodeType.OUTPUT_DATA_RAIL));
                float numOutputEnables = getAverageNodeCountPerCell(
                        new NodeType(NodeType.OUTPUT_ENABLE));
                page.summaryTableLine(
                        "Average number of input data rails per cell:",
                        formFloat.format(numInputRails));
                page.summaryTableLine(
                        "Average number of input enables per cell:",
                        formFloat.format(numInputEnables));
                page.summaryTableLine(
                        "Average number of output data rails per cell:",
                        formFloat.format(numOutputRails));
                page.summaryTableLine(
                        "Average number of output enables per cell:",
                        formFloat.format(numOutputEnables));
            }
        }
        page.summaryTableLine("Total transistor count:",getTransistorCount());
        page.summaryTableLine("Total transistor area (um^2):",
                              formFloat.format(getTransistorArea()*1e12));
        page.summaryTableLine("Layout area (um^2):",
                              layoutArea == -1.0F ? "N/A" : 
                              formFloat.format(layoutArea));
        page.summaryTableLine("Layout density factor:",
                              layoutArea == -1.0F ? "N/A" : 
                              formFloat.format(densityFactor));
        page.summaryTableLine("Logic transistor count percentage (unfolded):",
            formPercent.format((float)logicUnfoldedCount/
                               getUnfoldedTransistorCount()));
        page.summaryTableLine("Logic transistor area percentage:",
            formPercent.format(logicArea/getTransistorArea()));

        page.summaryTableEnd();

        // Save this for future use
        logicEfficiency = (float) logicArea / getTransistorArea();
    }

    public float getLogicEfficiency() { return logicEfficiency; }

    /** Generates HTML Statistics Report Page **/
    void printHtmlStatsReport(String designName, String outputDir, 
                              String pageName, String title) {
        // Start page
        if (title == null) title = "Leaf Cell Statistics";
        HtmlPage page;
        try {
            page = new HtmlPage(designName,outputDir,pageName,title);
        }
        catch (IOException e) {
            System.err.println("Couldn't create HTML page "+pageName);
            System.err.println(e);
            return;
        }
        printHtmlStats(page);
        page.close();
    }
}
