/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cadalyze;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.StringTokenizer;
import java.util.TreeSet;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cell.CellInterface;
import com.avlsi.io.FileSearchPath;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.htmlWriter.HtmlWriter;

import com.avlsi.tools.cadalyze.Cell.Category;
import com.avlsi.tools.cadalyze.NetGraphAnalyzer.NodeType;


/**
 * CAST Design Analyzer
 *
 * Gathers and reports interesting information about a CAST design.  Supports
 * both asynchronous and synchronous circuits.  
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **/
public final class Cadalyze {

    /***************************** INNER CLASSES *****************************/
    
    static class AreaCategory {

        String description = "";
        LeafStats leafStats = new LeafStats();
        NodeProps nodeProps = new NodeProps();
        Collection cellCategorySet;             // null for all categories
        AreaCategory parentAreaCategory = null;
        int indent = 0;

        AreaCategory(String description) {
            this.description = description;
            cellCategorySet = null;
        }

        AreaCategory(String description, Collection categories) {
            this.description = description;
            cellCategorySet = categories;
        }

        AreaCategory(String description, Category category) {
            this.description = description;
            cellCategorySet = new ArrayList();
            cellCategorySet.add(category);
        }

        AreaCategory(String description, Category[] categories,
                     AreaCategory parentAreaCategory, int indent) {
            this.description = description;
            this.parentAreaCategory = parentAreaCategory;
            this.indent = indent;
            cellCategorySet = new ArrayList();
            for (int i=0; i<categories.length; i++) 
                cellCategorySet.add(categories[i]);
        }

        AreaCategory(String description, Category category, 
                     AreaCategory parentAreaCategory, int indent) {
            this.description = description;
            this.parentAreaCategory = parentAreaCategory;
            this.indent = indent;
            cellCategorySet = new ArrayList();
            cellCategorySet.add(category);
        }

        void addLeafStats(Cell leaf) {
            //int cnt = leaf.getInstanceCount();
            //if (cellCategorySet != null)
            int cnt = leaf.getInstanceCountInCategories(cellCategorySet);
            leafStats.addStatistics(cnt,leaf.getLeafStats());
        }

        void addNodeProps(Cell cell) {
            //int cnt = cell.getInstanceCount();
            //if (cellCategorySet != null)
            int cnt = cell.getInstanceCountInCategories(cellCategorySet);
            nodeProps.addNodeProps(cnt,cell.getLocalNodeProps());
        }

        List reportTableLineStart() {
            List cols = new ArrayList();
            if (indent > 0) {
                cols.add(Boolean.TRUE);
                // Make this variably indent sometime later...
                cols.add("&nbsp;&nbsp;&nbsp;&nbsp;"+description);
            }
            else {
                cols.add(Boolean.FALSE);
                cols.add(description);
            }
            return cols;
        }

        String htmlFileName() {
            return description.replace(' ','_').replace('/','_')
                .toLowerCase() + ".html";
        }
    }


    /***************************** DATA MEMBERS ******************************/

    /** Print lots of debugging information? Used by debugPrintln() **/
    private boolean debug;

    /** The top-level cast cell to analyze **/
    private final CellInterface ci;

    /** Cast File Parser to use to parse new cells **/
    private final CastFileParser cfp;

    /** List of gates, in case we need to generate NetGraphs from prs blocks **/
    private final List gateList;

    /** FQCN-to-Cell map of all cells in the design **/
    private Map fqcnToCellMap = null;

    /** Top-level Cell **/
    private Cell topCell = null;

    /** Set of all leaf cells (impled from fqcnToCellMap + Cell lookups) **/
    private Set leafSet = null;

    /** Number of non-fragment leaf cells in the design **/
    private int nonFragLeafCnt = 0;

    /** Number of fragment leaf cells in the design **/
    private int fragLeafCnt = 0;

    /** Design style type (-1: unknown, 0: async, 1: sync, 2: mixed) **/
    private int style = -1;

    private static final int UNKNOWN      = -1;
    private static final int ASYNCHRONOUS = 0;
    private static final int SYNCHRONOUS  = 1;
    private static final int MIXED        = 2;

    /** List of all area categories (AreaCategory) **/
    private List areaCategories = null;

    /** Stats accumulated over all leaf cells (index 0 of areaCategories) **/
    //private LeafStats totalStats = null;
    private AreaCategory allCategories = null;

    /** Directory to which all output report files are written **/
    private String outputDir;

    /** Report generation start time **/
    private Date startDate;

    /** Median transistor length in the design **/
    private float medGateLength;

    /** Layout information **/
    private final LayoutInfo layoutInfo;

    /***************************** CLASS METHODS *****************************/

    /** Constructor **/
    public Cadalyze(final CellInterface ci, 
                    final CastFileParser cfp,
                    final List gateList,
                    final List layoutInfoFiles,
                    final String outputDir,
                    boolean debug) {
        this.ci        = ci;
        this.cfp       = cfp;
        this.gateList  = gateList;
        this.outputDir = outputDir + "/" + ci.getFullyQualifiedType();
        this.debug     = debug;
        Cell.debug     = debug;
        startDate      = new Date();

        // Read layout info files
        layoutInfo = new LayoutInfo(layoutInfoFiles);

        // Create results output directory if it doesn't exist
        File outputDirFile = new File(this.outputDir);
        if (!outputDirFile.exists()) outputDirFile.mkdir();
    }

    /** Analyze the design **/
    public void analyze(boolean verbose) throws Exception {
        if (verbose) System.out.println("Processing hierarchy...");
        fqcnToCellMap = Cell.processHierarchy(ci,cfp,verbose);
        topCell = (Cell)fqcnToCellMap.get(ci.getFullyQualifiedType());
        assert topCell != null;
        leafSet = lookupLeafCells();

        // Analyze all leaf cell netlists, tally results across entire design
        if (verbose) System.out.println("Analyzing leaf cell NetGraphs...");
        for (Iterator cit=fqcnToCellMap.values().iterator(); cit.hasNext();) {
            Cell cell = (Cell) cit.next();
            if (cell.isLeaf()) cell.analyzeNetGraph(cfp,layoutInfo);
        }

        /*
        for (Iterator lcit=leafSet.iterator(); lcit.hasNext();) {
            Cell leaf = (Cell)lcit.next();
            leaf.buildNetGraph(cfp);
            leaf.analyzeNetGraph();
        }
        */

        // Perform bottom-up propagation of node properties
        if (verbose) System.out.println("Accumulating node properties...");
        topCell.setNodeProperties();

        tallyLeafStatistics();
    }

    /** 
     * Returns all leaf cells (set of Cell types) 
     * As a side effect, calculates fragLeafCnt/nonFragLeafCnt and sets style.
     **/
    private Set lookupLeafCells() {
        HashSet leafSet = new HashSet();
        for (Iterator cit=fqcnToCellMap.values().iterator(); cit.hasNext();) {
            Cell cell = (Cell) cit.next();
            if (cell.isLeaf()) {
                leafSet.add(cell);
                if (cell.isFragment()) fragLeafCnt++;
                else nonFragLeafCnt++;
            }

            // Set high-level design style
            if (style == -1) {
                if (!cell.isSynchronous()) style = 0;
                else style = 1;
            }
            else if (style == 0 && cell.isSynchronous() ||
                     style == 1 && !cell.isSynchronous())
                style = 2;
        }
        return leafSet;
    }

    /** Tallies leaf cell statistics over all leaf cells in the design **/
    private void tallyLeafStatistics() {
        assert leafSet != null;

        // Set up area categories
        areaCategories = new ArrayList();
        AreaCategory areaCat = new AreaCategory("All");
        allCategories = areaCat;                       // maintain reference
        areaCategories.add(areaCat);
        if (style == ASYNCHRONOUS)
            addAreaCategoryHierarchy(areaCategories,
                Cell.Category.getMajorAsyncCategories(), "");
        else if (style == SYNCHRONOUS)
            addAreaCategoryHierarchy(areaCategories,
                Cell.Category.getMajorSyncCategories(), "");
        else if (style == MIXED) {
            addAreaCategoryHierarchy(areaCategories,
                Cell.Category.getMajorAsyncCategories(), "Async ");
            addAreaCategoryHierarchy(areaCategories,
                Cell.Category.getMajorSyncCategories(), "Sync ");
        }
        addAreaCategoryHierarchy(areaCategories,
                Cell.Category.getOtherCategories(),"");

        // Tally leaf and node statistics in each area category
        for (Iterator cit=fqcnToCellMap.values().iterator(); cit.hasNext();) {
            Cell cell = (Cell) cit.next();
            for (Iterator ai=areaCategories.iterator(); ai.hasNext();) {
                areaCat = (AreaCategory) ai.next();
                if (cell.isLeaf()) areaCat.addLeafStats(cell);
                if (!cell.isDpuFragment()) areaCat.addNodeProps(cell);
            }
        }

        // Determine median transistor length
        medGateLength = allCategories.leafStats.getMedianTransistorLength();
    }

    private void addAreaCategoryHierarchy(final List areaCategories,
                                          final List categoryList,
                                          final String descPrefix) {
        for (Iterator ci=categoryList.iterator(); ci.hasNext();) {
            Pair p = (Pair) ci.next();
            String desc = descPrefix + (String) p.getFirst();
            Category subCats[] = (Category[]) p.getSecond();
            AreaCategory parentAreaCat = 
                new AreaCategory(desc,subCats,allCategories,0);
            areaCategories.add(parentAreaCat);
            if (subCats.length > 1) {
                for (int i=0; i<subCats.length; i++) {
                    areaCategories.add(
                        new AreaCategory(subCats[i].toString(),subCats[i],
                                         parentAreaCat,1));
                }
            }
        }
    }

    public void printCellReports() {
        // Write HTML cell report pages
        File cellOutputDir = new File(outputDir + "/cells");
        if (!cellOutputDir.exists()) cellOutputDir.mkdir();
        for (Iterator cit=fqcnToCellMap.values().iterator(); cit.hasNext();) {
            Cell cell = (Cell) cit.next();
            cell.printHtmlReport(ci.getFullyQualifiedType(),
                                 cellOutputDir.getAbsolutePath(),
                                 medGateLength);
        }

        // Output leaf cell logic efficiency data sets
        try {
            cellOutputDir = new File(outputDir + "/leaf_data");
            if (!cellOutputDir.exists()) cellOutputDir.mkdir();
            PrintStream aveWidthFile = 
                new PrintStream( new BufferedOutputStream(
              new FileOutputStream(outputDir + "/leaf_data/ave_width.dat")));
            PrintStream inputRailsFile = 
                new PrintStream( new BufferedOutputStream(
              new FileOutputStream(outputDir+"/leaf_data/num_input_rails.dat")));
            PrintStream outputRailsFile = 
                new PrintStream( new BufferedOutputStream(
              new FileOutputStream(outputDir+"/leaf_data/num_output_rails.dat")));
            PrintStream inputEnablesFile = 
                new PrintStream( new BufferedOutputStream(
              new FileOutputStream(outputDir+"/leaf_data/num_input_enables.dat")));
            PrintStream outputEnablesFile = 
                new PrintStream( new BufferedOutputStream(
              new FileOutputStream(outputDir+"/leaf_data/num_output_enables.dat")));
            
            for (Iterator li=leafSet.iterator(); li.hasNext();) {
                Cell leaf = (Cell) li.next();
                LeafStats stats = leaf.getLeafStats();
                if (leaf.getCategory().isLogic() && 
                    leaf.getCategory().isAsynchronous() &&
                    stats.getUnclassifiedNodeFraction() < 0.10F &&
                    stats.getLogicEfficiency() > 0.05) {
                    String logicEffStr = 
                        formFloat.format(stats.getLogicEfficiency());

                    aveWidthFile.println(formFloat.format(
                        stats.getAverageTransistorWidth()*1e6) +
                                         " " + logicEffStr);

                    inputRailsFile.println(
                        String.valueOf(stats.getNodeTypeCount(
                            new NodeType(NodeType.INPUT_DATA_RAIL))) +
                        " " + logicEffStr);

                    outputRailsFile.println(
                        String.valueOf(stats.getNodeTypeCount(
                            new NodeType(NodeType.OUTPUT_DATA_RAIL))) +
                        " " + logicEffStr);

                    inputEnablesFile.println(
                        String.valueOf(stats.getNodeTypeCount(
                            new NodeType(NodeType.INPUT_ENABLE))) +
                        " " + logicEffStr);

                    outputEnablesFile.println(
                        String.valueOf(stats.getNodeTypeCount(
                            new NodeType(NodeType.OUTPUT_ENABLE))) + 
                        " " + logicEffStr);
                }
            }
            aveWidthFile.close();
            inputRailsFile.close();
            outputRailsFile.close();
            inputEnablesFile.close();
            outputEnablesFile.close();
        }
        catch (Exception e) {
            System.err.println("Warning! Couldn't write leaf cell data files.");
            System.err.println(e);
        }
    }

    /**
     * Generates an HTML report for the design.  
     * To be called after analyze().
     **/
    public void printReport(boolean verbose) {
        HtmlPage page;
        try {
            page = new HtmlPage(ci.getFullyQualifiedType(),
                                outputDir,"index.html",
                                "Cadalyze Netlist Report");
        }
        catch (IOException e) {
            System.err.println("Couldn't create HTML report "+
                               outputDir+"/index.html");
            System.err.println(e);
            return;
        }

        // Design info summary
        printDesignInfo(page);
        
        // High-level area breakdown
        page.section("Top-Level Area Breakdown");
        printCategoryBreakdown(page);

        // High-level node analysis
        page.section("Top-Level Node Statistics");
        printNodeStats(page);

        // Leaf cell statistics
        page.section("Leaf Cell Analyses");
        ArrayList lnklist = new ArrayList();
        printLeafCells();
        lnklist.add(new Pair("leafcells.html","Leaf Cell Categorizations"));
        for (Iterator ai=areaCategories.iterator(); ai.hasNext();) {
            // Generate and link to sub-report page
            AreaCategory areaCat = (AreaCategory) ai.next();
            if (areaCat.leafStats.getTransistorCount() > 0) {
                areaCat.leafStats.printHtmlStatsReport(
                    ci.getFullyQualifiedType(),
                    outputDir, areaCat.htmlFileName(),
                    areaCat.description+" Leaf Cells");
                lnklist.add(new Pair(areaCat.htmlFileName(),
                                     areaCat.description + " leaf cells"));
            }
        }
        page.listOfLinks(lnklist,false);

        // End page
        page.close();
    }

    public void printDesignInfo(final HtmlPage page) {

        // Median length intermediate
        String medLenStr = formFloat.format(medGateLength*1e6);

        page.section("Design Information");

        page.summaryTable();
        String styleString;
        switch (style) {
            case 0: styleString = "Asynchronous"; break;
            case 1: styleString = "Synchronous"; break;
            case 2: styleString = "Mixed"; break;
            default: styleString = "Unknown";
        }
        page.summaryTableLine("Design style:",styleString);
        page.summaryTableLine("Number of transistors (folded):",
            String.valueOf(allCategories.leafStats
                           .getUnfoldedTransistorCount()) + " (" +
            String.valueOf(allCategories.leafStats
                           .getTransistorCount()) + ")");
        page.summaryTableLine("Number of unique leaf cells (fragments):",
                              String.valueOf(fragLeafCnt+nonFragLeafCnt)+" ("+
                              String.valueOf(fragLeafCnt)+")");
        page.summaryTableLine("Average transistor width:",
            formFloat.format(allCategories.leafStats
                             .getAverageTransistorWidth()*1e6)+" um");
        page.summaryTableLine("Average non-staticizer transistor width:",
            formFloat.format(allCategories.leafStats.getAverageTransistorWidth(
                NetGraphAnalyzer.EdgeType.getNonStaticizerTypes())*1e6)+" um");
        page.summaryTableLine("Median transistor length:",medLenStr+" um");
        page.summaryTableLine("Total transistor area:",
            formFloat.format(allCategories.leafStats
                             .getTransistorArea()*1e12)+" um^2");
        page.summaryTableLine("Average drive strength per node:",
            formFloat.format(allCategories.nodeProps
                             .getAverageDriveStrength()));
        page.summaryTableLine("Average leaf cell connections per mid-level node:",
            formFloat.format(allCategories.nodeProps
                             .getAverageConnections()));
        page.summaryTableLine("Average transistor fanout per node:",
            formFloat.format(allCategories.nodeProps
                             .getAverageTransistorFanout()));
        page.summaryTableLine("Average gate load per node (per "+medLenStr+
                              " um length):",
            formFloat.format(allCategories.nodeProps
                             .getGateLoadPerLength(medGateLength)*1e6)+" um");

        page.summaryTableEnd();
    }

    public void printCategoryBreakdown(final HtmlPage page) {
        // Start report table
        List cols = new ArrayList();
        cols.add(new Pair("Category",Boolean.FALSE));
        cols.add(new Pair("Average Transistor Width",Boolean.TRUE));
        cols.add(new Pair("Transistor Count",Boolean.TRUE));
        cols.add(new Pair("Transistor Area",Boolean.TRUE));
        page.reportTableIndented(cols);

        // List each area category
        Iterator ai=areaCategories.iterator();
        if (ai.hasNext()) {
            AreaCategory allCat = (AreaCategory) ai.next();
            assert allCat.description.equals("All");
            while (ai.hasNext()) {
                AreaCategory areaCat = (AreaCategory) ai.next();
                if (areaCat.leafStats.getTransistorCount() == 0) continue;
                //cols = new ArrayList();
                //cols.add(areaCat.description);
                cols = areaCat.reportTableLineStart();
                cols.add(formFloat.format((float)areaCat.leafStats
                            .getAverageTransistorWidth()*1e6)+" um");
                cols.add(formPercent.format(
                            (float)areaCat.leafStats.getTransistorCount()/
                            areaCat.parentAreaCategory
                                .leafStats.getTransistorCount()));
                cols.add(formPercent.format(
                            areaCat.leafStats.getTransistorArea()/
                            areaCat.parentAreaCategory
                                .leafStats.getTransistorArea()));
                page.reportTableLine(cols);
            }
        }
        page.reportTableEnd();
    }

    public void printNodeStats(final HtmlPage page) {
        // Start report table
        List cols = new ArrayList();
        cols.add(new Pair("Category",Boolean.FALSE));
        cols.add(new Pair("Average Drive Strength",Boolean.TRUE));
        cols.add(new Pair("Average Transistor Fanout",Boolean.TRUE));
        cols.add(new Pair("Average Gate Load",Boolean.TRUE));
        page.reportTableIndented(cols);

        // List each area category
        AreaCategory allCat = null;
        Iterator ai=areaCategories.iterator();
        if (ai.hasNext()) {
            allCat = (AreaCategory) ai.next();
            assert allCat.description.equals("All");
            while (ai.hasNext()) {
                AreaCategory areaCat = (AreaCategory) ai.next();
                if (areaCat.leafStats.getTransistorCount() == 0) continue;
                cols = areaCat.reportTableLineStart();
                //cols = new ArrayList();
                //cols.add(areaCat.description);
                cols.add(formFloat.format((float)areaCat.nodeProps
                         .getAverageDriveStrength()));
                cols.add(formFloat.format(areaCat.nodeProps
                         .getAverageTransistorFanout()));
                cols.add(formFloat.format((float)areaCat.nodeProps
                         .getGateLoadPerLength(medGateLength)*1e6)+" um");
                page.reportTableLine(cols);
            }
        }
        page.reportTableEnd();
        allCat.nodeProps.printHtmlDistributions(page,medGateLength);
    }

    /** Prints a list of all leaf cells in the design **/
    public void printLeafCells() {
        HtmlPage page = null;
        try {
            page = new HtmlPage(ci.getFullyQualifiedType(),
                                outputDir,"leafcells.html",
                                "Leaf Cell Categorizations");
        }
        catch (IOException e) {
            System.err.println("Couldn't write "+outputDir+"/leafcells.html");
            System.err.println(e);
        }
        ArrayList cols = new ArrayList();
        cols.add(new Pair("Cell",Boolean.FALSE));
        cols.add(new Pair("Category",Boolean.TRUE));
        cols.add(new Pair("Style",Boolean.TRUE));
        cols.add(new Pair("Instance Count",Boolean.TRUE));
        cols.add(new Pair("Ave Width (um)",Boolean.TRUE));
        cols.add(new Pair("Area Contribution (um^2)",Boolean.TRUE));
        page.reportTable(cols);
        for (Iterator ct = getSortedLeafSet().iterator(); ct.hasNext();) {
            Cell cell = (Cell) ct.next();
            cols = new ArrayList();
            cols.add("<A HREF=cells/"+cell.getName()+".html>"+
                     cell.getName()+"</A>");
            cols.add(cell.getCategory().toString());
            cols.add((cell.isSynchronous() ? "S" : "A") + 
                     (cell.isFragment() ? "*" : ""));
            cols.add(String.valueOf(cell.getInstanceCount()));
            cols.add(formFloat.format(cell.getLeafStats()
                     .getAverageTransistorWidth()*1e6));
            cols.add(formFloat.format(cell.getInstanceCount() *
                     cell.getLeafStats().getTransistorArea() * 1e12));
            page.reportTableLine(cols);
        }
        page.reportTableEnd();
        page.writer.p();
        page.writer.println("* Fragment cell (Asynchronous only)");
        page.close();
    }

    /** Returns a list of all leaf cells, sorted by area contribution **/
    public SortedSet getSortedLeafSet() {
        class LeafCompare implements Comparator {
            public int compare(Object o1, Object o2) {
                Cell c1 = (Cell) o1, c2 = (Cell) o2;
                if (c1 == c2) return 0;
                if (c2.getInstanceCount() * 
                    c2.getLeafStats().getTransistorArea() * 1e12 -
                    c1.getInstanceCount() * 
                    c1.getLeafStats().getTransistorArea() * 1e12 > 0.0)
                    return 1;
                else return -1;
            }
        }
        TreeSet set = new TreeSet(new LeafCompare());
        set.addAll(leafSet);
        assert set.size() == leafSet.size();
        return set;
    }

    /** For debug **/
    private void printSubcellLists(final PrintStream ps) {
        for (Iterator cit = fqcnToCellMap.values().iterator(); cit.hasNext();) {
            Cell cell = (Cell) cit.next();
            if (!cell.getSubcellSet().isEmpty()) {
                ps.println("Subcells of "+cell.getName()+
                           (cell.isSynchronous() ? "[s]" : ""));
                for (Iterator scit=cell.getSubcellSet().iterator(); 
                     scit.hasNext();) {
                    Cell subCell = (Cell) scit.next();
                    ps.print(" "+subCell.getName()+
                             (cell.isLeaf() ? "[L]" : "") + "("+
                             cell.getSubcellInstanceCount(subCell)+"/"+
                             subCell.getInstanceCount()+")");
                }
                ps.println("");
            }
        }
    }

    private void debugPrintln(final String s) {
        if (debug) System.err.println(s);
    }

    private void debugPrint(final String s) {
        if (debug) System.err.print(s);
    }

    /***************************** STATIC METHODS *****************************/

    private static DecimalFormat formPercent = 
        (DecimalFormat)DecimalFormat.getInstance();

    private static DecimalFormat formFloat = 
        (DecimalFormat)DecimalFormat.getInstance();

    static {
        formPercent.applyPattern("#0.0%");
        formFloat.applyPattern("#0.00");
    }

    /** Very common HTML snippet **/
    private static void printTableReportLine(HtmlWriter page,
                                             String desc, String val) {
        page.tr(); page.td();
        page.println(desc);
        page.tdEnd(); page.td();
        page.println(val);
        page.tdEnd(); page.trEnd();
    }

    private static void printTableReportLine(HtmlWriter page,
                                             String desc, int val) {
        printTableReportLine(page,desc,String.valueOf(val));
    }

    private static void printPageHeader(HtmlWriter page, String title) {
        page.html(); page.head(); page.title();
        page.println(title);
        page.titleEnd();
        page.headEnd(); page.body();
    }

    private static final String usageString =
"Usage: java "+Cadalyze.class.getName()+"\n" +
"         [--debug=0|1]         Enable debug output (0)\n" +
"         [--verbose=0|1]       Enable verbose output (0)\n" +
"         [--output-dir=<dir>]  Directory to which all output files are\n"+
"                               written (.)\n"+
"         [--cast-path=<path>]  Standard cast path (.)\n" +
"         fqcn                  Cell to analyze\n\n";

    /** Command-line usage **/
    private static void usage_exit(final int exitStatus) {
        System.err.println(usageString);
        System.exit(exitStatus);
    }

    /** Main **/
    public static void main(String[] args) {
        try {
            boolean verbose = false;
            boolean debug = false;

            final CommandLineArgs parsedArgs = 
                new CommandLineArgsDefImpl( args );
            final CommandLineArgs argsWithConfigs =
                new CommandLineArgsWithConfigFiles( parsedArgs ); 
            final CommandLineArgs cachedArgs = 
                new CachingCommandLineArgs( argsWithConfigs );
            final CommandLineArgs theArgs = cachedArgs;

            if (theArgs.argExists("version")) {
                System.out.println( com.avlsi.util.debug.VersionInfo
                            .getVersionString(Cadalyze.class));
            }

            if (theArgs.getArgValue("verbose","").equals("1")) {
                verbose = true;
            }

            if (theArgs.getArgValue("debug","").equals("1")) {
                debug = true;
            }

            String outputDir = theArgs.getArgValue("output-dir",".");

            final FileSearchPath castPath =
                new FileSearchPath( theArgs.getArgValue( "cast-path", "." ) );

            final String gateString = theArgs.getArgValue("gates", null);
            List gateList = null;
            if (gateString != null)
                gateList = Arrays.asList(gateString.split(":"));

            final String lfilesString = theArgs.getArgValue("layout-info",null);
            List layoutInfoFiles = null;
            if (lfilesString != null)
                layoutInfoFiles = Arrays.asList(lfilesString.split(":"));

            final StringContainerIterator strIter = 
                theArgs.nonParsedArgumentsIterator();
            if (!strIter.hasNext()) usage_exit(1);
            final String castCellName = strIter.next();
            if (strIter.hasNext()) usage_exit(1);

            final CastFileParser cfp = new CastFileParser(castPath,"2");
            
            if (verbose) System.err.println("Parsing "+castCellName+"...");
            final CellInterface cell = 
                cfp.getFullyQualifiedCell(castCellName);

            final Cadalyze design = new Cadalyze(cell,cfp,gateList,
                                                 layoutInfoFiles,
                                                 outputDir,debug);

            design.analyze(verbose);
            if (debug) {
                System.out.println("Subcell listing:");
                design.printSubcellLists(System.out);
            }
            if (verbose) System.err.println("Generating HTML reports...");
            design.printCellReports();
            design.printReport(verbose);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
}
