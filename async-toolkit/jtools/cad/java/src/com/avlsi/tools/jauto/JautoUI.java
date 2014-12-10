/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.BufferedReader;
import java.io.File;
import java.io.FilenameFilter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilterWriter;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Writer;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.Stack;
import java.util.Set;
import java.util.TreeSet;
import java.util.TreeMap;
import java.util.StringTokenizer;
import java.lang.Math;

import com.avlsi.util.text.NumberFormatter;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.InvalidOperationException;
import com.avlsi.cast.impl.FloatValue;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.fast.*;
import com.avlsi.fast.ports.*;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.cdl.parser.CDLFactoryEmitter;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.cast.*;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.util.debug.Debug;
import com.avlsi.netlist.*;
import com.avlsi.netlist.impl.*;
import com.avlsi.io.FileSearchPath;

import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;

import com.avlsi.util.container.MultiSet;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.ObjectUtils;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.container.Triplet;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

import com.avlsi.util.functions.UnaryPredicate;

import com.avlsi.util.text.StringUtil;

import com.avlsi.tools.jauto.JautoMessageCenter;
import com.avlsi.tools.jauto.JautoUtil;


/**
 * This class contains mostly static functions for outputing runtime debug files for users.
 **/
public class JautoUI {
    
    private JautoUI() {
        throw new AssertionError();
    }

    public static boolean quiet = false;

    /**
     * Returns a new sorted set of cell types, sorted by type name.
     **/
    private static /*@ non_null @*/ SortedSet/*<CellType>*/ sortCellTypes(
            final /*@ non_null @*/ Set/*<CellType>*/ cellTypes) {
        final TreeSet/*<CellType>*/ sortedSet =
            new TreeSet/*<CellType>*/(new Comparator/*<CellType>*/() {
                /**
                 * Compares CellTypes based only on their name.
                 * Note: this comparator imposes orderings that are
                 * inconsistent with equals.
                 **/
                public int compare(Object /*CellType*/ o1,
                                   Object /*CellType*/ o2) {
                    final CellType c1 = (CellType) o1;
                    final CellType c2 = (CellType) o2;
                    return c1.typeName.compareTo(c2.typeName);
                }
            });

        sortedSet.addAll(cellTypes);
        return sortedSet;
    }

    /**
     * Returns a new sorted list of half operators, sorted by
     * canonical output node name and direction.
     **/
    private static /*@ non_null @*/ List/*<HalfOperator>*/
        sortHalfOperators(
            final /*@ non_null @*/ List/*<HalfOperator>*/ halfOps) {
        final List/*<HalfOperator>*/ sortedList =
            new ArrayList/*<HalfOperator>*/(halfOps);
        Collections.sort(
            sortedList,
            new Comparator/*<HalfOperator>*/() {
                /**
                 * Compares HalfOperators based only on their canonical
                 * output node name and direction.
                 * Note: this comparator imposes orderings that are
                 * inconsistent with equals.
                 **/
                public int compare(Object /*HalfOperator*/ o1,
                                   Object /*HalfOperator*/ o2) {
                    final HalfOperator halfOp1 = (HalfOperator) o1;
                    final HalfOperator halfOp2 = (HalfOperator) o2;
                    final int c1 = halfOp1.outputNet.canonicalName
                                          .getCadenceString().compareTo(
                                              halfOp2.outputNet.canonicalName
                                                     .getCadenceString());
                    if (c1 != 0)
                        return c1;
                    if (halfOp1.driveDirection == halfOp2.driveDirection)
                        return 0;
                    if (halfOp1.driveDirection ==
                            HalfOperator.DriveDirection.PULL_UP)
                        return -1;
                    else
                        return 1;
                }
            });

        return sortedList;
    }


    /**
     * This function is used to generate the "jauto.bs.info" and "jauto.as.info" files.
     * "bs" stands for "before-sizing" and "as" stands for "after-sizing".
     * <p>The function calls the "dumpInfo" function for each cell.
     * <p>Each cell in turn, writes out its own information and calls other elements' "dumpInfo" functions.
     **/
    public static void dumpAllInformation(CastDesign design, String fileName)
    {
        try{
            BufferedWriter bw = new BufferedWriter(new FileWriter(fileName));

            //bw.write("Sizing Information Dumping\n\n");

            for (Iterator ita =
                    sortCellTypes(design.allCellTypes).iterator();
                 ita.hasNext(); ) {
                CellType cella = (CellType)ita.next();
                cella.dumpInfo(bw, "");
            }

            bw.close();

        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }
         
    }


    /**
     * This function writes out information about transistor sizes 
     * different ways.  Three files are written:
     * <ul>
     *   <li>"sizes.debug" gives real transistor widths. For each
     *       half-operator, it will give transistor sizes for transistors
     *       in stacks of different depth.
     *   <li>"strength.debug" gives the strength (defined as the effective
     *       resistance) of the half-operators.
     *   <li>"hsizes.debug" gives the effective sizes of the half-operators.
     *   <li>"cells.debug" summarizes per cell.
     * </ul>
     **/
    public static void dumpSizeInformation(CastDesign design, String dirName)
    {
        File sizesDebugFile    = new File(dirName, "sizes.debug");
        File strengthDebugFile = new File(dirName, "strength.debug");
        File hsizesDebugFile   = new File(dirName, "hsizes.debug");
        File cellsDebugFile    = new File(dirName, "cells.debug");

        try{
            BufferedWriter sizesWriter    = new BufferedWriter(new FileWriter(sizesDebugFile));
            BufferedWriter strengthWriter = new BufferedWriter(new FileWriter(strengthDebugFile));
            BufferedWriter hsizesWriter   = new BufferedWriter(new FileWriter(hsizesDebugFile));
            BufferedWriter cellsWriter    = new BufferedWriter(new FileWriter(cellsDebugFile));

            for (Iterator ita =
                    sortCellTypes(design.allCellTypes).iterator();
                 ita.hasNext(); ) {
                CellType cta = (CellType)ita.next();
                if(cta.getListHalfOperators().size() > 0){
                    sizesWriter.write("CELL " + cta.typeName + " {\n");
                    strengthWriter.write("CELL " + cta.typeName + " {\n");
                    hsizesWriter.write("CELL " + cta.typeName + " {\n");
                    cellsWriter.write("CELL " + cta.typeName + " {\n");
                    int numInstances = cta.getPhysicalInstanceCount();
                    double allWidth = 0.0;
                    double maxWidth = Double.NEGATIVE_INFINITY;
                    double minWidth = Double.POSITIVE_INFINITY;
                    double widthHalfOps = 0.0;
                    int numHalfOps = 0;

                    // print sizes
                    for (Iterator itb =
                            sortHalfOperators(cta.getListHalfOperators())
                                .iterator();
                         itb.hasNext(); ) {
                        HalfOperator hoa = (HalfOperator)itb.next();
                        String s, driveDir;
                        if (hoa.driveDirection ==
                                HalfOperator.DriveDirection.PULL_DOWN) {
                            s = "N";
                            driveDir = "-";
                        } else {
                            s = "P";
                            driveDir = "+";
                        }
                        final String halfOp =
                            "  " + hoa.outputNet.canonicalName.getCadenceString() +
                            " " + driveDir;
                        sizesWriter.write(halfOp);
                        strengthWriter.write(halfOp);
                        hsizesWriter.write(halfOp);

                        // write logic widths to sizes.debug
                        double maxw=0.0;
                        for (Iterator itc = hoa.depths.iterator(); itc.hasNext(); ) {
                            int i = ((Integer)itc.next()).intValue();
                            double w = hoa.getWidth(i);
                            double f = hoa.getSymmetrizationFactor();
                            sizesWriter.write("  " + s + "W" + i + "=" +
                                              NumberFormatter.format(w*1e6, 3) + "u" +
                                              (f!=1 ? "*" + f : ""));
                            maxWidth=Math.max(maxWidth,w);
                            minWidth=Math.min(minWidth,w);
                            maxw=Math.max(maxw,w);
                        }
                        widthHalfOps+=maxw;
                        numHalfOps++;
                        strengthWriter.write(" " + NumberFormatter.format(hoa.getStrength(), 3) + "\n");
                        hsizesWriter.write(" " + NumberFormatter.format(hoa.getCurrentSize(), 3) + "\n");
                        sizesWriter.write("\n");
                    }

                    // second pass for staticizers
                    for (Iterator itb = cta.transistors.getNodes().iterator(); itb.hasNext(); ) {
                        NetGraph.NetNode node = (NetGraph.NetNode) itb.next();
                        String str = "  " + node.getName();
                        boolean pr = false;
                        for (int i=0; i<2; i++) {
                            int type = i==0 ? DeviceTypes.N_TYPE : DeviceTypes.P_TYPE;
                            double fbW = -1;
                            int fbD = -1;
                            for (Iterator itc = node.getFeedbackPaths().iterator(); itc.hasNext(); ) {
                                NetGraph.NetPath path = (NetGraph.NetPath) itc.next();
                                if (path.getType() == type) {
                                    double W = path.getMinWidth(false);
                                    if (fbW<0) fbW = W;
                                    else fbW = Math.min(fbW,W);
                                    fbD = Math.max(fbD,path.getDepth());
                                }
                            }
                            if (fbW>0 && fbD>0) {
                                pr = true;
                                str += "  " + (i==0 ? "N" : "P") + "WS=" + 
                                    NumberFormatter.format(fbW*1e6,3) + "u" +
                                    (fbD>1 ? "/" + fbD : "");
                            }
                        }
                        if (pr) sizesWriter.write(str + "\n");
                    }

                    // count total transistor width
                    for (Iterator itb = cta.transistors.getEdges().iterator(); itb.hasNext(); ) {
                        NetGraph.NetEdge edge = (NetGraph.NetEdge) itb.next();
                        allWidth += edge.width;
                    }

                    // finish cells.debug
                    double avgWidth = widthHalfOps / numHalfOps;
                    cellsWriter.write("  instances = " + numInstances + "\n");
                    cellsWriter.write("  all_width = " + NumberFormatter.format(allWidth*1e6,3) + "u\n");
                    cellsWriter.write("  avg_width = " + NumberFormatter.format(avgWidth*1e6,3) + "u\n");
                    cellsWriter.write("  max_width = " + NumberFormatter.format(maxWidth*1e6,3) + "u\n");
                    cellsWriter.write("  min_width = " + NumberFormatter.format(minWidth*1e6,3) + "u\n");

                    // finish
                    sizesWriter.write("}\n\n");
                    strengthWriter.write("}\n\n");
                    hsizesWriter.write("}\n");
                    cellsWriter.write("}\n");
                }
            }
            
            sizesWriter.close();
            strengthWriter.close();
            hsizesWriter.close();
            cellsWriter.close();
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }

    }



    /**
     * This function writes out delay values for the half-operators using Jauto delay model.
     * "hdelay.debug" gives delay values after sizing is finished.
     * <p>"hdelay_[n].debug" gives delay values after sizing iteration number "n".
     * <p>The delay information includes: maximum delay, minimum delay, maximum relative delay
     * and maximum adjusted relative delay.
     * <p>Maximum Delay is defined as the maximum delay of the half-operator in all its instantiation environments.
     * <P>Minimum Delay is defined as the minimum delay of the half-opeartor in all its instantiation environments.
     * <p>Maximum Relative Delay is Maximum Delay divided by Tau.
     * <p>Maximum Adjusted Relative Delay is Maximum Relative Delay divided by cell delay bias and native delay bias.
     **/
    public static void dumpDelayInformation(CastDesign design, String dirName, double tau, int iterationNumber)
    {
        File dir = new File(dirName, "iterations");
        
        File hdelayDebugFile;
        if (iterationNumber < 0)
            hdelayDebugFile = new File(dirName, "hdelay.debug");
        else
            hdelayDebugFile =
                new File(dir, "hdelay_" + iterationNumber + ".debug");

        try{
            if(!dir.exists()){
                dir.mkdirs();
            }

            BufferedWriter hdelayWriter = new BufferedWriter(new FileWriter(hdelayDebugFile));

            for (Iterator ita =
                    sortCellTypes(design.allCellTypes).iterator();
                 ita.hasNext(); ) {
                CellType cta = (CellType)ita.next();
                if(cta.getListHalfOperators().size() > 0){

                    final double cellDelayBias = cta.getDelay().getCellDelayBias();

                    hdelayWriter.write("CELL " + cta.typeName + " {\n");

                    for (Iterator itb =
                            sortHalfOperators(cta.getListHalfOperators())
                                .iterator();
                         itb.hasNext(); ) {
                        HalfOperator hoa = (HalfOperator)itb.next();

                        String s, driveDir;
                        if (hoa.driveDirection ==
                                HalfOperator.DriveDirection.PULL_DOWN) {
                            s = "N";
                            driveDir = "-";
                        } else {
                            s = "P";
                            driveDir = "+";
                        }

                        hdelayWriter.write("  " +
                                 hoa.outputNet.canonicalName.getCadenceString() + 
                                 driveDir);

                        double min_delay = Double.POSITIVE_INFINITY;
                        double max_delay = Double.NEGATIVE_INFINITY;
                        for (Iterator itc = hoa.outputNet.getGlobalNets().iterator(); itc.hasNext(); ) {
                            GlobalNet gna = (GlobalNet)itc.next();

                            final double delay = JautoUtil.calculateDelay(hoa, gna);

                            if(delay > max_delay){
                                max_delay = delay;
                            }

                            if(delay < min_delay){
                                min_delay = delay;
                            }

                        }

                        final double defaultDelayBias =
                            cta.getDelay().getNativeDelay(hoa.driveDirection == HalfOperator.DriveDirection.PULL_UP) / 100.0;
                        hdelayWriter.write(" MAX_DELAY=" + NumberFormatter.format(max_delay*1e12, 3) + "ps"
                            + " MIN_DELAY=" + NumberFormatter.format(min_delay*1e12, 3) + "ps"
                            + " MAX_REL_DELAY=" + NumberFormatter.format(max_delay/tau, 3)
                            + " MAX_ADJ_REL_DELAY=" + NumberFormatter.format(max_delay/tau/cellDelayBias/defaultDelayBias, 3)
                            );

                        hdelayWriter.write("\n");
                        
                    }

                    hdelayWriter.write("}\n\n");
                }
            }
            
            hdelayWriter.close();
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }

    }


    /**
     * This function writes out half-operator strength information on the flattened design.
     * The half-operator names use global (instance-based), canonical net names plus direction.
     * This function is written for PMCS project only.
     **/
    static public void dumpFlatStrengthInformation(CastDesign design, String dirName)
    {
        final File flatStrengthDebugFile =
            new File(dirName, "flat_strength.debug");

        Map/*<CellType,List<List<ConnectionInfo>>>*/ mapa =
            new HashMap/*<CellType,List<List<ConnectionInfo>>>*/();

        JautoUtil.getLeafCellInstantiationPaths
            (new ArrayList/*<ConnectionInfo>*/(),
             design.getTopLevelCell(),
             mapa);

        try{
            BufferedWriter flatStrengthWriter =
                new BufferedWriter(new FileWriter(flatStrengthDebugFile));

            for (Iterator ita = mapa.entrySet().iterator(); ita.hasNext(); ) {
                Map.Entry/*<CellType,List<List<ConnectionInfo>>*/ entry =
                    (Map.Entry) ita.next();
                CellType cta = (CellType) entry.getKey();
                List/*<List<ConnectionInfo>>*/ lsta =
                    (List/*<List<ConnectionInfo>>*/) entry.getValue();

                for (Iterator itb = lsta.iterator(); itb.hasNext(); ) {
                    List/*<ConnectionInfo>*/ lstb =
                        (List/*<ConnectionInfo>*/) itb.next();

                    for (Iterator itc =
                            sortHalfOperators(cta.getListHalfOperators())
                                .iterator();
                         itc.hasNext(); ) {
                        HalfOperator hoa = (HalfOperator)itc.next();

                        List/*<ConnectionInfo>*/ lstc =
                            new ArrayList/*<ConnectionInfo>*/();

                        StringBuffer sb = new StringBuffer(JautoUtil.getFlatCanonicalName(hoa.outputNet, lstb, lstc));
                        sb.append(" " +
                            (hoa.driveDirection ==
                                 HalfOperator.DriveDirection.PULL_DOWN ?
                             "-" : "+"));
                        sb.append(" " + NumberFormatter.format(hoa.getStrength(), 3));
                        sb.append("\n");

                        flatStrengthWriter.write(sb.toString().replaceAll("\\/top\\.", "")
                                                              .replaceAll("\\/Xtop\\/", ""));
                    }
                }
            }

            flatStrengthWriter.close();
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }

    }


    /**
     * This function writes out half-operator strength information in design hierarchy.
     * The half-operator names use local (cell-based), canonical net names plus direction.
     * This function is written for PMCS project only.
     **/
    public static void dumpHierStrengthInformation(CastDesign design, String dirName)
    {
        final File hierStrengthDebugFile =
            new File(dirName, "hier_strength.debug");

        try{
            BufferedWriter hierStrengthWriter =
                new BufferedWriter(new FileWriter(hierStrengthDebugFile));

            for (Iterator ita =
                    sortCellTypes(design.allCellTypes).iterator();
                 ita.hasNext(); ) {
                CellType cta = (CellType)ita.next();
                if(cta.getListHalfOperators().size() > 0){
                    String cellName = getGDSIICellName(cta.typeName);

                    hierStrengthWriter.write("CELL " + cellName + "\n");

                    for (Iterator itb =
                            sortHalfOperators(cta.getListHalfOperators())
                                .iterator();
                         itb.hasNext(); ) {
                        HalfOperator hoa = (HalfOperator)itb.next();
                        String s, driveDir;
                        if (hoa.driveDirection ==
                                HalfOperator.DriveDirection.PULL_DOWN) {
                            s = "N";
                            driveDir = "-";
                        } else {
                            s = "P";
                            driveDir = "+";
                        }


                        hierStrengthWriter.write("\tSTRENGTH " +
                                                 getGDSIINetName(cellName,
                                                                 hoa.outputNet
                                                                    .canonicalName
                                                                    .getCadenceString()) +
                                                 " " + driveDir);

                        hierStrengthWriter.write(" " + NumberFormatter.format(hoa.getStrength(), 3) + "\n");
                    }

                    hierStrengthWriter.write("\n");
                }
            }
            
            hierStrengthWriter.close();
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }
    }


    static private String getGDSIICellName(String cellName)
    {
        return cellName;
    }


    static private String getGDSIINetName(String cellName, String netName)
    {
        return netName;
    }

    private static String printCoordinate(final double x, final double y,
                                          int precision) {
        return "("  + NumberFormatter.format(x, precision) + ", "
                    + NumberFormatter.format(y, precision) + ")";
    }

    public static void dumpSinkSource(final CastDesign design,
                                      final String dirName) throws IOException {
        final File debugFile = new File(dirName, "sink_source.debug");
        final BufferedWriter bw = new BufferedWriter(new FileWriter(debugFile));
        for (Iterator cellIter = sortCellTypes(design.allCellTypes).iterator();
             cellIter.hasNext(); ) {
            final CellType cell = (CellType) cellIter.next();
            if (cell == design.getTopLevelCell() || cell.getLevel() == 0)
                continue;
            bw.write("CELL " + cell.typeName + " {\n");
            for (Iterator netIter = cell.getAllNets().iterator();
                 netIter.hasNext(); ) {
                final CellNet net = (CellNet) netIter.next();
                if (net.isPortNet()) continue;

                bw.write("  NET " + net.canonicalName + " {\n");
                final GlobalNet gnet = (GlobalNet) net.getGlobalNets().get(0);
                final Set seen = new HashSet();
                for (Iterator sinkIter = gnet.getListSinks().iterator();
                     sinkIter.hasNext(); ) {
                    final NetSink sink = (NetSink) sinkIter.next();
                    if (sink.type == NetType.HALF_OPERATOR_TRANSISTOR &&
                        seen.add(sink.getInstanceName())) {
                        bw.write("    SINK " + sink.getInstanceName() + "/" +
                                 sink.sink.subType.typeName + " " +
                                 printCoordinate(sink.getCoordinateX() * 1e6,
                                                 sink.getCoordinateY() * 1e6,
                                                 2) +
                                 "\n");
                    } else if (sink.type == NetType.CAPACITIVE_LOAD) {
                        bw.write("    SINK " +
                                 NumberFormatter.format(
                                     sink.getLoadCapacitance()*1e15, 3) +
                                 "fF\n");
                    }
                }

                seen.clear();
                for (Iterator sourceIter = gnet.getListSources().iterator();
                     sourceIter.hasNext(); ) {
                    final NetSource source = (NetSource) sourceIter.next();
                    if (source.type == NetType.HALF_OPERATOR_TRANSISTOR &&
                        seen.add(source.getInstanceName())) {
                        bw.write("    SOURCE " + source.getInstanceName() +
                                 "/" + source.source.subType.typeName + " " +
                                 printCoordinate(source.getCoordinateX() * 1e6,
                                                 source.getCoordinateY() * 1e6,
                                                 2) +
                                 "\n");
                    }
                }
                bw.write("  }\n");
            }
            bw.write("}\n");
        }
        bw.close();
    }

    /** summarize wiring information for one GlobalNet **/
    static private String wireSummary (GlobalNet gna) {
        return "  L=" + NumberFormatter.format(gna.getEstimatedWireLength()*1e6, 3) + "u"
             + "  W=" + NumberFormatter.format(gna.getWireWidth()*1e6, 3) + "u"
             + "  S=" + NumberFormatter.format(gna.getWireSpace()*1e6, 3) + "u"
             + "  C=" + NumberFormatter.format(gna.getWireCapacitance()*1e15, 3) + "f"
             + "  R=" + NumberFormatter.format(gna.getWireResistance(), 3) 
             + "  SPAN=" + NumberFormatter.format(gna.getEstimatedWireSpan()*1e6, 3) + "u";
    }

    /**
     * This function writes out information about interconnections in the design.
     * The information includes wire length, width, space, capacitance, resistance.
     * <p>At first, the wire information is written out cell-by-cell.
     * Within each cell, the wires are sorted according the their length.
     * At the end, all wires in the design is written out sorted.
     * <p>"wires.debug" gives basic information about wires.
     * "sp2.debug" gives additional information about geometry information of
     * the sources and sinks on the wire.
     **/
    static public void dumpWireInformation(CastDesign design, String dirName)
    {
        File wiresDebugFile = new File(dirName, "wires.debug");
        File sp2DebugFile = new File(dirName, "sp2.debug");

        Collection/*<CellNet>*/ allCellNets =
            new MultiSet/*<CellNet>*/(CellNetLengthSorter.getInstance());

        try{
            BufferedWriter wiresWriter = new BufferedWriter(new FileWriter(wiresDebugFile));
            BufferedWriter sp2Writer = new BufferedWriter(new FileWriter(sp2DebugFile));


            for (Iterator ita =
                    sortCellTypes(design.allCellTypes).iterator();
                 ita.hasNext(); ) {
                CellType cta = (CellType)ita.next();
                wiresWriter.write("CELL " + cta.typeName + " {\n");

                Collection/*<CellNet>*/ cellNets =
                    new MultiSet/*<CellNet>*/(CellNetLengthSorter.getInstance());

                for (Iterator itb = cta.getAllNets().iterator(); itb.hasNext(); ) {
                    CellNet cna = (CellNet)itb.next();

                    if(!cna.isPortNet()){
                        assert cna.getGlobalNets().size() <= 1
                            : "Internal CellNet have more than 1 global net.\n" +
                              "CellName: " + cta.typeName + "\n" +
                              "NetName:" + cna.canonicalName.getCadenceString();

                        cellNets.add(cna);
                        allCellNets.add(cna);
                    }
                }


                for (Iterator itb = cellNets.iterator(); itb.hasNext(); ) {
                    CellNet cna = (CellNet)itb.next();
                    GlobalNet gna = (GlobalNet) cna.getGlobalNets().get(0);

                    wiresWriter.write("  " 
                             + cna.canonicalName.getCadenceString()
                             + wireSummary(gna) + "\n");
                        
                }

                wiresWriter.write("}\n");
            }

            wiresWriter.write("\nCELL SORTED_ALL_NETS {\n");
            for (Iterator itb = allCellNets.iterator(); itb.hasNext(); ) {
                CellNet cna = (CellNet)itb.next();
                GlobalNet gna = (GlobalNet) cna.getGlobalNets().get(0);

                wiresWriter.write("  " 
                         + cna.container.typeName
                         + "/" + cna.canonicalName.getCadenceString()
                         + wireSummary(gna) + "\n");
                        
                sp2Writer.write("NET  " 
                         + cna.container.typeName
                         + " " + cna.canonicalName.getCadenceString()
                         + wireSummary(gna) + "\n");

                sp2Writer.write("Sources:\n");
                for (Iterator itc = gna.getListSources().iterator(); itc.hasNext(); ) {
                    NetSource nsra = (NetSource)itc.next();
                    if(nsra.type == NetType.HALF_OPERATOR_TRANSISTOR){
                        sp2Writer.write("("
                                  + NumberFormatter.format(nsra.coordinateX * 1.0E6, 2) + "u, "
                                  + NumberFormatter.format(nsra.coordinateY * 1.0E6, 2) + "u)"
                                  + "\n");
                    }
                }


                sp2Writer.write("Sinks:\n");
                for (Iterator itc = gna.getListSinks().iterator(); itc.hasNext(); ) {
                    NetSink nska = (NetSink)itc.next();
                    if(nska.type == NetType.HALF_OPERATOR_TRANSISTOR){
                        sp2Writer.write("("
                                  + NumberFormatter.format(nska.coordinateX * 1.0E6, 2) + "u, "
                                  + NumberFormatter.format(nska.coordinateY * 1.0E6, 2) + "u)"
                                  + "\n");
                    }
                }

                sp2Writer.write("\n");

                        
            }
            // TODO: add wiresWriter.write("}\n");
            
            wiresWriter.close();
            sp2Writer.close();
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }

    }

    static private void getRoutedWireInfo(CellType routed,
                                          CellType cta,
                                          HierName prefix,
                                          Collection result,
                                          Set routedCells) {
        for (Iterator itb = cta.getAllNets().iterator(); itb.hasNext(); ) {
            CellNet cna = (CellNet)itb.next();
            if(!cna.isPortNet()) result.add(new Triplet(routed, prefix, cna));
        }
        for (Iterator cii = cta.getAllSubcellConnections().iterator();
             cii.hasNext(); ) {
            ConnectionInfo ci = (ConnectionInfo) cii.next();
            if (!routedCells.contains(ci.child)) {
                getRoutedWireInfo(routed, ci.child,
                                  HierName.append(prefix, ci.nameInParent),
                                  result, routedCells);
            }
        }
    }

    static private Collection getRoutedCells(final CastDesign design,
                                             final Collection result) {
        final CellType top = design.getTopLevelCell();
        result.add(top);
        for (Iterator cii = top.getAllSubcellConnections().iterator();
             cii.hasNext(); ) {
            ConnectionInfo ci = (ConnectionInfo) cii.next();
            result.add(ci.child);
            ci.child.walkOnce(
                new CellTypeProcessor() {
                    public void processCellType(CellType c) {
                        if (CellUtils.isRouted(c.cast_cell)) {
                            result.add(c);
                        }
                    }
                }
            );
        }
        return result;
    }

    static public void dumpRoutedWireInformation(CastDesign design,
                                                 String dirName)
    {
        File wiresDebugFile = new File(dirName, "wires.routed.debug");

        Comparator lengthSorter = new Comparator() {
            final Comparator real = CellNetLengthSorter.getInstance();
            public int compare(Object a, Object b) {
                final Triplet triple1 = (Triplet) a;
                final Triplet triple2 = (Triplet) b;
                final CellNet cn1 = (CellNet) triple1.getThird();
                final CellNet cn2 = (CellNet) triple2.getThird();
                final GlobalNet gn1 = (GlobalNet) cn1.getGlobalNets().get(0);
                final GlobalNet gn2 = (GlobalNet) cn2.getGlobalNets().get(0);
                // sort in descending order by wire length
                int x = Double.compare(gn2.getEstimatedWireLength(),
                                       gn1.getEstimatedWireLength());
                if (x != 0) return x;

                // sort by cell type
                final CellType type1 = (CellType) triple1.getFirst();
                final CellType type2 = (CellType) triple2.getFirst();
                x = type1.typeName.compareTo(type2.typeName);
                if (x != 0) return x;

                // sort by canonical name
                final HierName prefix1 = (HierName) triple1.getSecond();
                final HierName fullName1 =
                    HierName.append(prefix1, cn1.canonicalName);

                final HierName prefix2 = (HierName) triple2.getSecond();
                final HierName fullName2 =
                    HierName.append(prefix2, cn2.canonicalName);
                return fullName1.compareTo(fullName2);
            }
        };

        Collection/*<CellNet>*/ allCellNets =
            new MultiSet/*<CellNet>*/(lengthSorter);


        try{
            BufferedWriter wiresWriter = new BufferedWriter(new FileWriter(wiresDebugFile));

            final Set routedCells = (Set) getRoutedCells(design,
                    new TreeSet(CellType.getNameComparator()));
            for (Iterator ita = routedCells.iterator(); ita.hasNext(); ) {
                CellType cta = (CellType)ita.next();
                wiresWriter.write("CELL " + cta.typeName + " {\n");

                Collection/*<CellNet>*/ cellNets =
                    new MultiSet/*<CellNet>*/(lengthSorter);

                getRoutedWireInfo(cta, cta, null, cellNets, routedCells);
                allCellNets.addAll(cellNets);

                for (Iterator itb = cellNets.iterator(); itb.hasNext(); ) {
                    Triplet triple = (Triplet) itb.next();
                    HierName prefix = (HierName)triple.getSecond();
                    CellNet cna = (CellNet)triple.getThird();
                    GlobalNet gna = (GlobalNet) cna.getGlobalNets().get(0);

                    wiresWriter.write("  " 
                             + (HierName.append(prefix, cna.canonicalName).getCadenceString())
                             + wireSummary(gna) + "\n");
                        
                }

                wiresWriter.write("}\n");
            }

            wiresWriter.write("\nCELL SORTED_ALL_NETS {\n");
            for (Iterator itb = allCellNets.iterator(); itb.hasNext(); ) {
                Triplet triple = (Triplet) itb.next();
                CellType routed = (CellType)triple.getFirst();
                HierName prefix = (HierName)triple.getSecond();
                CellNet cna = (CellNet)triple.getThird();
                GlobalNet gna = (GlobalNet) cna.getGlobalNets().get(0);

                wiresWriter.write("  " 
                         + routed.typeName
                         + "/" + (HierName.append(prefix, cna.canonicalName).getCadenceString())
                         + wireSummary(gna) + "\n");
                        
            }
            // TODO: add wiresWriter.write("}\n");
            
            wiresWriter.close();
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }

    }

    /**
     * A comparator which could be used to sort CellNet in descending order
     * based on wire length.
     **/
    private final static class CellNetLengthSorter implements Comparator {
        private static CellNetLengthSorter singleton = null;
        private CellNetLengthSorter() { }
        public static CellNetLengthSorter getInstance() {
            if (singleton == null) {
                singleton = new CellNetLengthSorter();
            }
            return singleton;
        }
        public int compare(Object a, Object b) {
            final CellNet cn1 = (CellNet) a;
            final CellNet cn2 = (CellNet) b;
            final GlobalNet gn1 = (GlobalNet) cn1.getGlobalNets().get(0);
            final GlobalNet gn2 = (GlobalNet) cn2.getGlobalNets().get(0);
            // sort in descending order by wire length
            int x = Double.compare(gn2.getEstimatedWireLength(),
                                   gn1.getEstimatedWireLength());
            if (x != 0) return x;

            // sort by cell type
            x = cn1.container.typeName.compareTo(cn2.container.typeName);
            if (x != 0) return x;

            // sort by canonical name
            return cn1.canonicalName.compareTo(cn2.canonicalName);
        }
    }

    /**
     * Inner class to sort strings in descending weight.  Its
     * redundant with all the other sortedInsert, StringSortOnKey
     * stuff, but at least I understand it.  -- Andrew Lines
     **/
    private final static class WeightedString implements Comparable {
        final String str;
        final double weight;
        WeightedString(String str, double weight) {
            this.str = str;
            this.weight = weight;
        }
        public int compareTo(Object B) {
            WeightedString b = (WeightedString) B;
            if      (weight>b.weight) return -1;
            else if (weight<b.weight) return  1;
            else return str.compareTo(b.str);
        }
    }
    

    /**
     * Approximately verify electromigration limits given Cap, Up, Dn.
     * Returns a WeightedString.
     **/
    private static WeightedString checkElectromigration(String prefix,
                                                        double Cap,
                                                        double Up, double Dn,
                                                        double cutoff,
                                                        /*@ non_null @*/ TechnologyData technologyData) {
        // convert delay to slew time
        final double DelayToSlew =
            technologyData.getDelayToSlewTimeConversionFactor();
        // cycle time
        final double Cycle = technologyData.getCycleTime();
        // minimum wire width
        final double Width = technologyData.getMinimumWireWidth();
        // voltage assumed for electromigration
        final double Voltage = technologyData.getVoltage();
        // average current limit per contact
        double Jav_via_limit = technologyData.getViaAvgCurrentLimit();
        // average current limit per width of M1
        double Jav_wire_limit = technologyData.getWireAvgCurrentLimit();
        // rms current limit per width of M2
        final double Jrms_wire_limit =
            technologyData.getWireRMSCurrentLimit();
        // peak current limit per width of M1
        final double Jpeak_wire_limit =
            technologyData.getWirePeakCurrentLimit();
        // correction for 100C instead of 110C
        final double Jav_temp_correction =
            technologyData.getAvgCurrentLimitTempCorrectionFactor();

        // convert delays to slews
        Up *= DelayToSlew;
        Dn *= DelayToSlew;

        // temperature correct Jav electromigration limits
        Jav_via_limit  *= Jav_temp_correction;
        Jav_wire_limit *= Jav_temp_correction;

        // compute average unidirectional current
        double Jav = Cap*Voltage/Cycle;
        
        // compute root-mean-squared bidirectional current
        double Jrms = Cap*Voltage*Math.sqrt(1/(Up*Cycle) + 1/(Dn*Cycle));

        // compute peak bidirectional current adjusted by duty cycle
        double Jpeak_up = Cap*Voltage*Math.sqrt(2/(Up*Cycle));
        double Jpeak_dn = Cap*Voltage*Math.sqrt(2/(Dn*Cycle));
        double Jpeak = (Jpeak_dn>Jpeak_up) ? Jpeak_dn : Jpeak_up;

        // compute relative electromigration violations
        double Jav_via_err    = Jav/Jav_via_limit;
        double Jav_wire_err   = Jav/Width/Jav_wire_limit;
        double Jrms_wire_err  = Jrms/Width/Jrms_wire_limit;
        double Jpeak_wire_err = Jpeak/Width/Jpeak_wire_limit;
        
        // max up the worst violation
        double worst = Jav_via_err;
        if (Jav_wire_err>worst)   worst = Jav_wire_err;
        if (Jrms_wire_err>worst)  worst = Jrms_wire_err;
        if (Jpeak_wire_err>worst) worst = Jpeak_wire_err;
        
        // add violations to list of weighted strings
        if (worst>cutoff)
            return new WeightedString
                (prefix +
                 " Jav_via_err=" + 
                 NumberFormatter.format(Jav_via_err,2) +
                 " Jav_wire_err=" + 
                 NumberFormatter.format(Jav_wire_err,2) +
                 " Jrms_wire_err=" + 
                 NumberFormatter.format(Jrms_wire_err,2) +
                 " Jpeak_wire_err=" + 
                 NumberFormatter.format(Jpeak_wire_err,2),
                 worst);
        return null;
    }

    /** Print EM violations for a cell from a MultiSet of WeightedString's. **/
    private static void printElectromigration(BufferedWriter bw,
                                              String cellName,
                                              MultiSet strs) 
        throws IOException {
        if (strs.size()>0) {
            bw.write("CELL " + cellName + " {\n");
            Iterator itc = strs.iterator();
            while (itc.hasNext()) {
                WeightedString str = (WeightedString) itc.next();
                bw.write("  " + str.str + "\n");
            }
            bw.write("}\n\n");
        }
    }

    /**
     * Report electromigration by local nets and by operators, if
     * greater than cutoff * limit.
     **/
    private static void 
        doElectromigrationReport(CastDesign design,
                                 File emLocalNetsFile,
                                 File emOperatorsFile,
                                 double cutoff) throws IOException {

        // open localnetsName file for writing
        BufferedWriter bw1 =
            new BufferedWriter(new FileWriter(emLocalNetsFile));

        // open operatorsName file for writing
        BufferedWriter bw2 =
            new BufferedWriter(new FileWriter(emOperatorsFile));
        
        // recurse through all cells
        for (Iterator ita = sortCellTypes(design.allCellTypes).iterator();
             ita.hasNext(); ) {
            CellType cta  = (CellType) ita.next();
            MultiSet/*<WeightedString>*/ strs1 =
                new MultiSet/*<WeightedString>*/();
            MultiSet/*<WeightedString>*/ strs2 =
                new MultiSet/*<WeightedString>*/();
            
            // recurse through all nets in this cell
            for (Iterator itb  = cta.getAllNets().iterator();
                 itb.hasNext(); ) {
                CellNet cna = (CellNet)itb.next();
                List/*<GlobalNet>*/ lsta = cna.getGlobalNets();
                
                // if its a local net, report on it
                if (!cna.isPortNet()) {
                    GlobalNet gna = (GlobalNet) lsta.get(0);
                    WeightedString str = checkElectromigration
                        (cna.canonicalName.getCadenceString(),
                         gna.getLoadCapacitance(),
                         gna.getUpDelay(),
                         gna.getDownDelay(),
                         cutoff,
                         design.getTechnologyData());
                    if (str!=null) strs1.add(str);
                }
                
                // if its driven locally, report on its worst global net
                if (!cna.connectedToSubcells() && 
                    cna.getListSources().size()>0) {
                    WeightedString worst = null;
                    for (Iterator itc = lsta.iterator(); itc.hasNext(); ) {
                        GlobalNet gna = (GlobalNet) itc.next();
                        WeightedString str = checkElectromigration
                            (cna.canonicalName.getCadenceString(),
                             gna.getLoadCapacitance(),
                             gna.getUpDelay(),
                             gna.getDownDelay(),
                             cutoff,
                             design.getTechnologyData());
                        if ((str!=null) &&
                            ((worst==null) || (str.compareTo(worst)<0))) 
                            worst = str;
                    }
                    if (worst!=null) strs2.add(worst);
                }
            }
            printElectromigration(bw1,cta.typeName,strs1);
            printElectromigration(bw2,cta.typeName,strs2);
        }
        bw1.close();
        bw2.close();
    }

    /**
     * Dump electromigration reports for all local nets and all
     * operators.  Reports the proportional violation of various
     * electromigration limits, assuming worst case wiring (1 via and
     * minimum width wires).  Uses estimated_delay and cap.  Added by
     * Andrew Lines.
     **/
    public static void dumpElectromigrationInformation(CastDesign design,
                                                       String dirName) {
        try {
            // report only potential errors above the limits
            doElectromigrationReport(design,
                                     new File(dirName, "electromigration_localnets.debug"),
                                     new File(dirName, "electromigration_operators.debug"),
                                     1);
            // report all em estimates for comparison purposes
            doElectromigrationReport(design,
                                     new File(dirName, "full_electromigration_localnets.debug"),
                                     new File(dirName, "full_electromigration_operators.debug"),
                                     0);
        } catch(IOException e) {
            e.printStackTrace(System.err);
        }
    }

    /**
     * DEPRECATED.  dumpSizeInformation does this better!
     *
     * This function writes out information about cell statistics.
     * The information includes total instance count of the cell,
     * total transistor width, average transistor width,
     * maximum transistor width, minimum transistor width
     * <p>"cells.debug" gives information after sizing is done
     * <p>"cells_[n].debug" gives information after sizing iteration number [n]
     * 
     * An empty block of information is written for mid-level cells.
     **/
    public static void dumpCellInformation(CastDesign design, String dirName, int iterationNumber)
    {
        File dir = new File(dirName, "iterations");

        File cellsDebugFile;
        if (iterationNumber < 0)
            cellsDebugFile = new File(dirName, "cells.debug");
        else
            cellsDebugFile =
                new File(dir, "cells_" + iterationNumber + ".debug");

        File sp1DebugFile = new File(dirName, "sp1.debug");


        try{
            if(!dir.exists()){
                dir.mkdirs();
            }

            BufferedWriter cellsWriter = new BufferedWriter(new FileWriter(cellsDebugFile));
            BufferedWriter sp1Writer = new BufferedWriter(new FileWriter(sp1DebugFile));

            for (Iterator ita =
                    sortCellTypes(design.allCellTypes).iterator();
                 ita.hasNext(); ) {
                CellType cta = (CellType)ita.next();
                cellsWriter.write("CELL " + cta.typeName + " {\n");

                assert cta.getLevel() >= 0;
                if (cta.getLevel() == 0) {
                    sp1Writer.write(cta.typeName);

                    int numInstances = cta.getPhysicalInstanceCount();
                    Set/*<NetGraph.NetEdge>*/ seenEdges =
                        new HashSet/*<NetGraph.NetEdge>*/();
                    double totalWidth = 0.0;
                    int numTransistors = 0;
                    double maxWidth = Double.NEGATIVE_INFINITY;
                    double minWidth = Double.POSITIVE_INFINITY;

                    for (Iterator itb = cta.getListHalfOperators().iterator(); itb.hasNext(); ) {
                        HalfOperator hoa = (HalfOperator)itb.next();

                        for (Iterator itc = hoa.transistors.iterator(); itc.hasNext(); ) {
                            NetGraph.NetEdge nea = (NetGraph.NetEdge)itc.next();

                            if(!seenEdges.contains(nea)){
                                seenEdges.add(nea);

                                if(nea.width > maxWidth){
                                    maxWidth = nea.width;
                                }

                                if(nea.width < minWidth){
                                    minWidth = nea.width;
                                }

                                totalWidth += nea.width;
                                numTransistors++;

                            }
                        }
                    }

                    double averageWidth = totalWidth / numTransistors;

                    cellsWriter.write("  instances = " + numInstances + "\n");
                    cellsWriter.write("  all_width = " + NumberFormatter.format(totalWidth*1e6,3) + "u\n");
                    cellsWriter.write("  avg_width = " + NumberFormatter.format(averageWidth*1e6,3) + "u\n");
                    cellsWriter.write("  max_width = " + NumberFormatter.format(maxWidth*1e6,3) + "u\n");
                    cellsWriter.write("  min_width = " + NumberFormatter.format(minWidth*1e6,3) + "u\n");

                    sp1Writer.write(" " + numInstances);
                    sp1Writer.write(" " + NumberFormatter.format(totalWidth*1e6,3));
                    sp1Writer.write(" " + NumberFormatter.format(averageWidth*1e6,3));
                    sp1Writer.write(" " + NumberFormatter.format(maxWidth*1e6,3));
                    sp1Writer.write(" " + NumberFormatter.format(minWidth*1e6,3));
                    sp1Writer.write("\n");
                }

                cellsWriter.write("}\n\n");
            }

            cellsWriter.close();
            sp1Writer.close();
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }

        
    }


    /**
     * The maximum number of times warnings and information about a single path
     * will be emitted.  A path may be emitted multiple times, because it is
     * used in different scenarios.
     **/
    private final static int WARNS_PER_PATH = 3;

    /**
     * This function writes out information about sizing paths in the design.
     * The information about paths is written out sorted by slack.
     * Path information includes total path delay, slack (budget - delay),
     * average transistor width over all the half-operators on the path,
     * cells and half-operators on the path.
     * <p>"paths_all.debug" gives all the paths.
     * <p>"paths_fixed.debug" gives paths with all fixed-size half-operators, 
     * including the loading half-operator of the last half-operator on the path.
     * <p>"paths_sizable.debug" gives paths with at least one half-operator sizable.
     * <p>"paths_all_[n].debug", "paths_fixed_[n].debug" and "paths_sizable_[n].debug"
     * give information about paths after sizing iteration number [n].
     **/
    public static void dumpPathInformation(CastDesign design, String dirName, double tau, int iterationNumber)
    {
        dumpPathInformation(design, dirName, tau, iterationNumber, false);
    }

    public static void dumpPathInformation(CastDesign design, String dirName, double tau, int iterationNumber, boolean topOnly)
    {
        File allPathsDebugFile = null, fixedPathsDebugFile = null, sizablePathsDebugFile = null,
             signOffPathsDebugFile = null, topPathsDebugFile = null;
        
        if(iterationNumber < 0){
            if (! topOnly) {
                allPathsDebugFile     = new File(dirName, "paths_all.debug");
                fixedPathsDebugFile   = new File(dirName, "paths_fixed.debug");
                sizablePathsDebugFile = new File(dirName, "paths_sizable.debug");
                signOffPathsDebugFile = new File(dirName, "paths_signoff.debug");
            }
            topPathsDebugFile     = new File(dirName, "paths_top.debug");
        }
        else{
            File iterationsDir = new File(dirName, "iterations");
            if (! topOnly) {
                allPathsDebugFile     = new File(iterationsDir, "paths_all_" + iterationNumber + ".debug");
                fixedPathsDebugFile   = new File(iterationsDir, "paths_fixed_" + iterationNumber + ".debug");
                sizablePathsDebugFile = new File(iterationsDir, "paths_sizable_" + iterationNumber + ".debug");
                signOffPathsDebugFile = new File(iterationsDir, "paths_signoff_" + iterationNumber + ".debug");
            }
            topPathsDebugFile     = new File(iterationsDir, "paths_top_" + iterationNumber + ".debug");
        }

        JautoMessageCenter  messageCenter = design.getMessageCenter();

        MultiMap/*<Double,String>*/ allPaths =
            new MultiMap/*<Double,String>*/(new TreeMap/*<Double,String>*/(),
                                            MultiMap.ARRAY_LIST_FACTORY);
        MultiMap/*<Double,String>*/ fixedPaths =
            new MultiMap/*<Double,String>*/(new TreeMap/*<Double,String>*/(),
                                            MultiMap.ARRAY_LIST_FACTORY);
        MultiMap/*<Double,String>*/ sizablePaths =
            new MultiMap/*<Double,String>*/(new TreeMap/*<Double,String>*/(),
                                            MultiMap.ARRAY_LIST_FACTORY);
        MultiMap/*<Double,String>*/ signOffPaths =
            new MultiMap/*<Double,String>*/(new TreeMap/*<Double,String>*/(),
                                            MultiMap.ARRAY_LIST_FACTORY);
        MultiMap/*<Double,String>*/ topPaths =
            new MultiMap/*<Double,String>*/(new TreeMap/*<Double,String>*/(),
                                            MultiMap.ARRAY_LIST_FACTORY);

        for (Iterator ita =
                sortCellTypes(design.allCellTypes).iterator();
             ita.hasNext(); ) {
            CellType cta = (CellType)ita.next();

            for (Iterator itb = getPathIterator(cta); itb.hasNext(); ) {
                AbstractPath cpa = (AbstractPath) itb.next();
                final MultiMap/*<Double,Integer>*/ slacks =
                    new MultiMap/*<Double,Integer>*/(
                            new TreeMap/*<Double,Integer>*/(),
                            MultiMap.ARRAY_LIST_FACTORY);

                for (int i = 0; i < cpa.slack.length; ++i) {
                    double slack = cpa.getSlack(i);
                    slacks.put(slack, i);
                    if (slack != cpa.getRealSlack(i)) {
                        String pathInfo = "// container=" + cta.typeName +
                                          "\n" + cpa.printPath(i);
                        signOffPaths.put(cpa.getRealSlack(i), pathInfo);
                    }
                }

                int count = 0;
                for (Iterator j = slacks.keySet().iterator(); j.hasNext(); ) {
                    final Double slackD = (Double) j.next();
                    final Collection entry = slacks.get(slackD);
                    for (Iterator k = entry.iterator();
                         k.hasNext() && (count < WARNS_PER_PATH || topOnly); ++count) {
                        final int i = ((Integer) k.next()).intValue();

                        double slack = cpa.getSlack(i);
                        if(slack <= -tau){
                            if(iterationNumber != 1){ // do not warn for first iteration for sizable paths
                                if(!cpa.isFixedSize(i)){
                                    String msa = "WARNING";
                                    String msb = "Large negative slack: " + NumberFormatter.format(slack*1e12,3) + "ps\n";
                                    String msc = "Please refer to file \"paths_sizable.debug\" for details.\n";

                                    messageCenter.createMessage(1, 4, msa, msb, msc);
                                }
                            }
                            else{ // large negative slack warning for completely fixed paths, for once only
                                if(cpa.isFixedSize(i)){
                                    String msa = "WARNING";
                                    String msb = "Large negative slack: " + NumberFormatter.format(slack*1e12,3) + "ps\n";
                                    String msc = "Please refer to file \"paths_fixed.debug\" for details.\n";

                                    messageCenter.createMessage(1, 4, msa, msb, msc);
                                }
                            }
                        }

                        String pathInfo = "// container=" + cta.typeName + "\n" + cpa.printPath(i);
                        if ( ! topOnly ) {
                            allPaths.put(slackD, pathInfo);

                            if(cpa.isFixedSize(i)){
                                fixedPaths.put(slackD, pathInfo);
                            }
                            else{
                                sizablePaths.put(slackD, pathInfo);
                            }
                        }
                        if (pathInfo.indexOf("endnet=/top.") >= 0) {
                            topPaths.put(slackD, pathInfo);
                        }
                    }
                }
            }
        }

        try{
            File dir = new File(dirName, "iterations");
            if(!dir.exists()){
                dir.mkdirs();
            }

            if (! topOnly) {
                dumpPathInformation(allPaths, allPathsDebugFile);
                dumpPathInformation(fixedPaths, fixedPathsDebugFile);
                dumpPathInformation(sizablePaths, sizablePathsDebugFile);
                dumpPathInformation(signOffPaths, signOffPathsDebugFile);
            }
            dumpPathInformation(topPaths, topPathsDebugFile);
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }

    }

    private static void dumpPathInformation
        (final /*@ non_null @*/ MultiMap/*<Double,String>*/ paths,
         final /*@ non_null @*/ File file)
        throws IOException {

        final BufferedWriter bw = new BufferedWriter(new FileWriter(file));

        for (Iterator/*<String>*/ i = paths.values().iterator(); i.hasNext(); ) {
            final String s = (String) i.next();
            bw.write(s);
            bw.write("\n");
        }

        bw.close();
    }

    private static void processDelaySignOff(final CellType cell,
                                            final Map paths) {
        for (Iterator pathIterator = getFixedPathIterator(cell);
             pathIterator.hasNext(); ) {
            final AbstractPath path = (AbstractPath) pathIterator.next();
            final StringBuffer buf = new StringBuffer();
            path.getPathString(buf);
            final String delayPart = (String) paths.get(buf.toString());
            if (delayPart != null) {
                final Map delays = new HashMap();
                final String parts[] = StringUtil.split(delayPart, ' ');
                for (int i = 0; i < parts.length; i = i + 3) {
                    delays.put(new Pair(parts[i], parts[i + 1]), parts[i + 2]);
                }
                final List gns = path.getEndNet().getGlobalNets();
                final double[] result;
                if (path.signoff == null) {
                    result = new double[gns.size()];
                    for (int i = 0; i < result.length; ++i)
                        result[i] = Double.NaN;
                    path.signoff = result;
                } else {
                    result = path.signoff;
                }
                for (int i = 0; i < gns.size(); ++i) {
                    final GlobalNet gn = (GlobalNet) gns.get(i);
                    if (isFixed(gn)) {
                        final CellNet cn = gn.getTopCellNet();
                        final Pair key =
                                new Pair(cn.container.typeName,
                                         cn.canonicalName.getAsString('.'));
                        final String delay = (String)
                            delays.get(
                                new Pair(cn.container.typeName,
                                         cn.canonicalName.getAsString('.')));
                        if (delay != null) {
                            final double d = Double.parseDouble(delay);
                            if (Double.isNaN(result[i])) {
                                result[i] = d;
                            } else {
                                result[i] = Math.min(result[i], d);
                            }
                        }
                    }
                }
            }
        }
    }

    public static void readDelaySignOff(CastDesign design, String fileName)
        throws IOException {
        final BufferedReader br = new BufferedReader(new FileReader(fileName));
        HashMap paths = null;
        String line;
        CellType type = null;
        while ((line = br.readLine()) != null) {
            if (line.startsWith("CELL")) {
                if (type != null)
                    processDelaySignOff(type, paths);
                type = design.getCell(line.substring(5));
                paths = new HashMap();
            } else {
                if (type != null) {
                    String[] parts = StringUtil.split(line, ':');
                    paths.put(parts[1], parts[0]);
                }
            }
        }
        if (type != null) processDelaySignOff(type, paths);
        br.close();
    }

    private static boolean isFixed(final GlobalNet gn) {
        for (Iterator i = gn.getListSources().iterator(); i.hasNext(); ) {
            final NetSource nsrc = (NetSource) i.next();
            if (nsrc.getType() == NetType.HALF_OPERATOR_TRANSISTOR &&
                !nsrc.getSource().subType.isFixedSize()) return false;
        }
        for (Iterator i = gn.getListSinks().iterator(); i.hasNext(); ) {
            final NetSink nsnk = (NetSink) i.next();
            if (nsnk.getType() == NetType.HALF_OPERATOR_TRANSISTOR &&
                !nsnk.getSink().subType.isFixedSize()) return false;
        }
        return CellUtils.isFixed(gn.getTopCellNet().container.cast_cell);
    }

    private static boolean getDelayString(final AbstractPath path,
                                          final StringBuffer buf) {
        final Map delays = new TreeMap(CellNet.getComparator()) {
            public Object put(Object key, Object value) {
                final Object old = super.put(key, value);
                if (old != null) {
                    throw new RuntimeException("Multiple values for key " +
                            key + " " + value + " (" + old + ")");
                }
                return old;
            }
        };

        int count = 0;
        for (Iterator i = path.getEndNet().getGlobalNets().iterator();
             i.hasNext(); ++count) {
            final GlobalNet gn = (GlobalNet) i.next();
            if (isFixed(gn))
                delays.put(gn.getTopCellNet(), new Double(path.delay[count]));
        }

        boolean needSpace = false;
        for (Iterator i = delays.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final CellNet cn = (CellNet) entry.getKey();
            final Double delay = (Double) entry.getValue();
            if (needSpace) buf.append(' ');
            buf.append(cn.container.typeName);
            buf.append(' ');
            buf.append(cn.canonicalName.getCadenceString());
            buf.append(' ');
            buf.append(delay);
            needSpace = true;
        }
        return needSpace;
    }

    public static void dumpDelaySignOff(CastDesign design, String dirName)
        throws IOException {
        final File signoffFile =
            new File(dirName, "delay_signoff.debug");
        final PrintWriter pw =
            new PrintWriter(new BufferedWriter(new FileWriter(signoffFile)));
        
        for (Iterator i = sortCellTypes(design.allCellTypes).iterator();
             i.hasNext(); ) {
            final CellType cell = (CellType) i.next();
            boolean header = true;
            for (Iterator pathIterator = getFixedPathIterator(cell);
                 pathIterator.hasNext(); ) {
                final AbstractPath path = (AbstractPath) pathIterator.next();
                final StringBuffer buf = new StringBuffer();
                if (getDelayString(path, buf)) {
                    if (header) {
                        pw.println("CELL " + cell.typeName);
                        header = false;
                    }
                    buf.append(':');
                    path.getPathString(buf);
                    pw.println(buf.toString());
                }
            }
        }
        pw.close();
    }

    private static void findHierarchy(final CellType ct,
                                      final HierName[] parts,
                                      int index,
                                      final Collection instList,
                                      final Collection cellList) {
        HierName attempt = null;
        while (index < parts.length) {
            attempt = HierName.append(attempt, parts[index]);
            index++;
            final ConnectionInfo ci = ct.getSubcellNamed(attempt);
            if (ci != null) {
                instList.add(ci.nameInParent);
                cellList.add(ci.child);
                if (index < parts.length)
                    findHierarchy(ci.child, parts, index, instList,
                                  cellList);
                return;
            }
        }
        throw new RuntimeException("Cannot find instance: " +
                                   join(parts, '.') + " index = " + index +
                                   " cell = " + ct.typeName);
    }

    /**
     * Given instance name <param>inst</param> that is relative to the cell
     * <param>top</param>, return the path in type space and instance space in
     * <param>instList</param> and <param>cellList</param>, respectively.  This
     * is complicated by inlining, because not every dot is a hierarchical
     * delimiter.
     **/
    private static void findHierarchy(final CellType top,
                                      final HierName inst,
                                      final Collection instList,
                                      final Collection cellList) {
        if (inst == null) return;
        final String s = inst.getAsString('.');
        final String[] ss = StringUtil.split(s, '.');
        final HierName[] hs = new HierName[ss.length];
        for (int i = 0; i < ss.length; ++i) {
            hs[i] = HierName.makeHierName(ss[i]);
        }
        findHierarchy(top, hs, 0, instList, cellList);
    }

    private static String join(final HierName[] names, final char c) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < names.length; ++i) {
            if (i != 0) sb.append(c);
            sb.append(names[i].getAsString('.'));
        }
        return sb.toString();
    }

    private static class ViolationInstance implements Comparable {
        private final CellType cell;
        private final HierName inst;
        private boolean loadOnly;
        public ViolationInstance(final CellType cell, final HierName inst,
                                 final boolean loadOnly) {
            this.cell = cell;
            this.inst = inst;
            this.loadOnly = loadOnly;
        }
        public void updateUsage(final ViolationInstance vi) {
            loadOnly = loadOnly && vi.loadOnly;
        }
        public boolean equals(final Object o) {
            if (o instanceof ViolationInstance)
                return equals((ViolationInstance) o);
            else
                return false;
        }
        public boolean equals(final ViolationInstance vi) {
            return cell.typeName.equals(vi.cell.typeName) &&
                   ObjectUtils.equals(inst, vi.inst);
        }
        public int hashCode() {
            return ObjectUtils.hashCode(cell.typeName) +
                   ObjectUtils.hashCode(inst);
        }
        public int compareTo(final Object o) {
            final ViolationInstance vi = (ViolationInstance) o;
            int x = cell.typeName.compareTo(vi.cell.typeName);
            if (x != 0) return x;
            return ObjectUtils.compare(inst, vi.inst);
        }
        private int getGlobalNetCount() {
            int count = Integer.MIN_VALUE;
            for (Iterator i = cell.getAllNets().iterator(); i.hasNext(); ) {
                final CellNet net = (CellNet) i.next();
                if (net.isPortNet()) {
                    count = Math.max(count, net.getGlobalNets().size());
                }
            }
            return count;
        }
        public String toString(final CellType top, final boolean isUnnamedTop) {
            final StringBuffer buf = new StringBuffer();
            final List instList = new ArrayList();
            final List cellList = new ArrayList();
            buf.append(cell.isFixedSize() ? "FIXED_" : "SIZABLE_");
            buf.append(getGlobalNetCount());
            if (loadOnly) buf.append('*');
            buf.append(' ');
            findHierarchy(top, inst, instList, cellList);
            if (isUnnamedTop) {
                // treat ports of the top level cell as local nodes of the top
                // level; remove the extra level of hierarchy
                instList.remove(0);
                cellList.remove(0);
            }
            for (Iterator i = cellList.iterator(); i.hasNext(); ) {
                final CellType ct = (CellType) i.next();
                buf.append(ct.typeName);
                if (i.hasNext()) buf.append('/');
            }
            buf.append(' ');
            buf.append(
                join((HierName[]) instList.toArray(new HierName[0]), '/'));

            return buf.toString();
        }
        public String toString() {
            return "cell: " + cell.typeName + " inst: " + inst;
        }
        public CellType getCell() {
            return cell;
        }
        public HierName getInstance() {
            return inst;
        }
    }

    private static Object lastElement(final List l) {
        if (l == null || l.isEmpty()) return null;
        else return l.get(l.size() - 1);
    }

    private static Triplet findNextConnection(final CellType cell,
                                              final HierName inst) {
        final int components = inst.getNumComponents();
        HierName head = null, tail = inst;
        ConnectionInfo ci = null;
        for (int i = 1; i < components; ++i) {
            head = HierName.append(head, tail.head());
            tail = tail.tail();
            if ((ci = cell.getSubcellNamed(head)) != null) break;
        }
        if (ci == null) {
            head = inst;
            tail = null;
            ci = cell.getSubcellNamed(head);
        }
        return new Triplet(head, tail, ci);
    }

    private static HierName computePartialInstance(CellType ancestor,
                                                   final CellType descendent,
                                                   HierName inst) {
        HierName result = null;
        while (true) {
            final Triplet t = findNextConnection(ancestor, inst);
            final ConnectionInfo ci = (ConnectionInfo) t.getThird();
            if (ci == null) {
                result = null;
                break;
            }
            final HierName head = (HierName) t.getFirst();
            inst = (HierName) t.getSecond();
            result = HierName.append(result, head);
            ancestor = ci.child;
            if (ci.child == descendent) break;
        }
        return result;
    }

    private static HierName hierAppend(final HierName a, final HierName b) {
        if (a == null) return b;
        if (b == null) return a;
        return HierName.append(a, b);
    }

    private static void addViolation(final Map violations, final CellType cell,
                                     final HierName inst, final boolean load) {
        final ViolationInstance candidate =
            new ViolationInstance(cell, inst, load);
        final ViolationInstance prev =
            (ViolationInstance) violations.get(candidate);
        if (prev != null) {
            candidate.updateUsage(prev);
        }
        violations.put(candidate, candidate);
    }

    private static void processViolation(final Map violationMap,
                                         final CellType pathContainer,
                                         final AbstractPath path,
                                         final float slackRatioLimit) {
        final CellNet endNet = path.getEndNet();
        final List globalNets = endNet.getGlobalNets();
        assert globalNets.size() == path.slack.length;
        assert path.slack.length == path.delay.length;

        Map pathInstance = null;
        HalfOperator lastHalfOp = null;
        SizingPath lastSizingPath = null;
        for (int i = 0; i < path.slack.length; ++i) {
            // calculate ratio of delay to budget
            final float slackRatio = (float)
                (path.delay[i] / (path.slack[i] + path.delay[i]));
            if (slackRatio <= slackRatioLimit) continue;

            if (pathInstance == null) {
                // instance names are relative to the container of the
                // abstract path
                pathInstance = new TreeMap();
                if (path instanceof SizingPath) {
                    addViolation(pathInstance, path.container, null, false);
                    lastHalfOp = (HalfOperator)
                        lastElement(((SizingPath) path).getPath());
                    lastSizingPath = (SizingPath) path;
                } else {
                    final CatPath catPath = (CatPath) path;
                    for (Iterator j = catPath.getCatPath().iterator();
                         j.hasNext(); ) {
                        final SizingPath sp = (SizingPath) j.next();
                        addViolation(pathInstance, sp.container,
                                     sp.getInstanceName(), false);
                        if (!j.hasNext()) {
                            lastSizingPath = sp;
                            lastHalfOp =
                                (HalfOperator) lastElement(sp.getPath());
                        }
                    }
                }
            }

            final GlobalNet gn = (GlobalNet) globalNets.get(i);
            final CellType gnContainer = gn.getTopCellNet().container;
            HierName pathPrefix = null;
            HierName gnPrefix = null;

            // There are 3 possibilities:
            // 1. The container of the global net instantiates the
            // container of the path.
            // 2. The container of the path instantiates the container of
            // the global net.
            // 3. The container of the global net is the same as the
            // container of the path.
            // Case 3 is easy, since nothing needs to be done.  In cases 1
            // and 2, we need to calculate the instance path from the
            // parent container to the child container, and append that to
            // instances relative to the child container.
            final CellType topMost;
            if (gnContainer == pathContainer) {
                topMost = gnContainer;
            } else {
                if (gnContainer.getLevel() > pathContainer.getLevel()) {
                    topMost = gnContainer;
                    HierName pathInst = null;
                    for (Iterator j = gn.getListSources().iterator();
                         j.hasNext(); ) {
                        final NetSource src = (NetSource) j.next();
                        if (src.type == NetType.HALF_OPERATOR_TRANSISTOR &&
                            src.source == lastHalfOp) {
                            // XXX: what if the same global net is driving by
                            // multiple instances of the same cell?
                            pathInst = src.getInstanceName();
                            break;
                        }
                    }
                    pathPrefix = computePartialInstance(gnContainer,
                                                        pathContainer,
                                                        pathInst);
                } else {
                    final HierName pathInst = lastSizingPath.getInstanceName();
                    topMost = pathContainer;
                    gnPrefix = computePartialInstance(pathContainer,
                                                      gnContainer,
                                                      pathInst);
                }
            }

            final Map instanceSet = new TreeMap();
            if (pathPrefix == null) {
                instanceSet.putAll(pathInstance);
            } else {
                for (Iterator j = pathInstance.keySet().iterator();
                     j.hasNext(); ) {
                    final ViolationInstance vio =
                        (ViolationInstance) j.next();
                    addViolation(instanceSet, vio.getCell(),
                                 hierAppend(pathPrefix,
                                            vio.getInstance()),
                                 false);
                }
            }
            final Collection sinks = (Collection) gn.getListSinks();
            for (Iterator j = sinks.iterator(); j.hasNext(); ) {
                // instance names are relative to the container of the
                // global net
                final NetSink sink = (NetSink) j.next();
                if (sink.type == NetType.HALF_OPERATOR_TRANSISTOR) {
                    addViolation(instanceSet, sink.sink.subType,
                                 hierAppend(gnPrefix, 
                                            sink.getInstanceName()),
                                 true);
                }
            }

            final Pair key = new Pair(instanceSet, topMost);
            final Float oldRatio = (Float) violationMap.get(key);
            if (oldRatio == null || oldRatio.floatValue() < slackRatio) {
                violationMap.put(key, new Float(slackRatio));
            }
        }
    }

    private static Iterator getPathIterator(final CellType cta) {
        final Iterator pathIterator;
        if (cta.getLevel() > 0){
            pathIterator = cta.getReducedCatPaths().iterator();
        } else {
            pathIterator =
                new FilteringIterator(cta.getSizingPaths().iterator(),
                    new UnaryPredicate() {
                        public boolean evaluate(final Object o) {
                            return !((SizingPath) o).isFragment();
                        }
                    }
                );
        }
        return pathIterator;
    }

    private static Iterator getFixedPathIterator(final CellType cta) {
        return
            new FilteringIterator(getPathIterator(cta),
                new UnaryPredicate() {
                    public boolean evaluate(final Object o) {
                        return ((AbstractPath) o).isComponentsFixed();
                    }
                }
            );
    }

    public static void dumpViolations(CastDesign design, String dirName,
                                      float slackRatioLimit)
        throws IOException {
        final File out = new File(dirName, "violations.debug");
        final BufferedWriter bw = new BufferedWriter(new FileWriter(out));
        final Map violationMap = new HashMap();
        for (Iterator ita = sortCellTypes(design.allCellTypes).iterator();
             ita.hasNext(); ) {
            CellType cta = (CellType)ita.next();

            for (Iterator pathIterator = getPathIterator(cta);
                 pathIterator.hasNext(); ) {
                final AbstractPath path = (AbstractPath) pathIterator.next();
                processViolation(violationMap, cta, path, slackRatioLimit);
            }
        }
        for (Iterator i = violationMap.entrySet().iterator();
             i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final Pair key = (Pair) entry.getKey();
            final Map violations = (Map) key.getFirst();
            final CellType top = (CellType) key.getSecond();
            final boolean isUnnamedTop = top == design.getTopLevelCell();
            final String topName;
            if (isUnnamedTop) {
                final Collection cis = top.getAllSubcellConnections();
                assert cis.size() == 1;
                topName =
                    ((ConnectionInfo) cis.iterator().next()).child.typeName;
            } else {
                topName = top.typeName;
            }
            final Float ratio = (Float) entry.getValue();
            bw.write("VIOLATION " + topName + " " + ratio + " {\n");
            for (Iterator j = violations.keySet().iterator(); j.hasNext(); ) {
                final ViolationInstance vio = (ViolationInstance) j.next();
                bw.write("  " + vio.toString(top, isUnnamedTop) + "\n");
            }
            bw.write("}\n");
        }
        bw.close();
    }


    /**
     * This function writes out information about overall circuit statistics.
     * The information is contained in the "summary" string
     **/
    public static void generateSizingSummary(String summary, String dirName, int iterationNumber)
    {
        File dir = new File(dirName, "iterations");

        File sweepDebugFile;
        if(iterationNumber < 0){ // final report
            sweepDebugFile = new File(dirName, "sweep.debug");
        }
        else{ // intermediate report
            sweepDebugFile =
                new File(dir, "sweep_" + iterationNumber + ".debug");
        }

        try{
            if(!dir.exists()){
                dir.mkdirs();
            }

            BufferedWriter bw = new BufferedWriter(new FileWriter(sweepDebugFile));

            bw.write(summary);

            bw.close();
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }

    }


    /**
     * This function writes out information about paths on a flattened design.
     * The purpose of this debug file is orginally designed for doing static
     * timing analysis using PathMill.
     **/
    static public void dumpFlatPathInformation(CastDesign design, String dirName)
    {
        File flatPathsDebugFile = new File(dirName, "flat_paths.debug");


        Map/*<CellType,List<List<ConnectionInfo>>>*/ mapa =
            new HashMap/*<CellType,List<List<ConnectionInfo>>>*/();

        JautoUtil.getLeafCellInstantiationPaths
            (new ArrayList/*<ConnectionInfo>*/(),
             design.getTopLevelCell(),
             mapa);

        try{
            BufferedWriter bw = new BufferedWriter(new FileWriter(flatPathsDebugFile));

            for (Iterator ita = mapa.entrySet().iterator(); ita.hasNext(); ) {
                Map.Entry entry = (Map.Entry) ita.next();
                CellType cta = (CellType) entry.getKey();
                List/*<List<ConnectionInfo>>*/ lsta =
                    (List/*<List<ConnectionInfo>>*/) entry.getValue();

                for (Iterator itb = lsta.iterator(); itb.hasNext(); ) {
                    List/*<ConnectionInfo>*/ lstb =
                        (List/*<ConnectionInfo>*/) itb.next();

                    for (Iterator itc = cta.getSizingPaths().iterator(); itc.hasNext(); ) {
                        SizingPath spa = (SizingPath)itc.next();

                        String stra = "";
                        double delay = 0.0;
                        for (Iterator itd = spa.getPath().iterator(); itd.hasNext(); ) {
                            HalfOperator hoa = (HalfOperator)itd.next();

                            // System.err.println("Calling JautoUtil.getFlatCanonicalName...");

                            List/*<ConnectionInfo>*/ lstc =
                                new ArrayList/*<ConnectionInfo>*/();
                            stra += JautoUtil.getFlatCanonicalName(hoa.outputNet, lstb, lstc);
                            stra += " (" +
                                (hoa.driveDirection ==
                                     HalfOperator.DriveDirection.PULL_DOWN ?
                                 "f" : "r") + ") ";

                            CellNet cnb = (CellNet)lstc.get(0);

                            assert cnb.getGlobalNets().size() <= 1
                                : "Internal CellNet have more than 1 global net.\n" +
                                  "CellName: " + cta.typeName + "\n" +
                                  "NetName:" + cnb.canonicalName.getCadenceString();

                            GlobalNet gna = (GlobalNet) cnb.getGlobalNets().get(0);

                            delay += JautoUtil.calculateDelay(hoa, gna);
                        }
                        // stra += "\n";

                        HalfOperator hoa = (HalfOperator)spa.getPath().get(0);
                        String strc = " (" +
                            (hoa.driveDirection ==
                                 HalfOperator.DriveDirection.PULL_DOWN ?
                             "r" : "f") + ") ";

                        for (Iterator itd = spa.getStartNets().iterator(); itd.hasNext(); ) {
                            CellNet cna = (CellNet)itd.next();

                            List/*<ConnectionInfo>*/ lstc =
                                new ArrayList/*<ConnectionInfo>*/();
                            String strb = JautoUtil.getFlatCanonicalName(cna, lstb, lstc);
                            strb += strc + stra;

                            String strd = strb.replaceAll("\\/top\\.", "");
                            strb = strd.replaceAll("\\/Xtop\\/", "");
                            bw.write("search_path match=complete " + strb);

                            bw.write(" (delay = " + NumberFormatter.format(delay, 3) + ")\n");
                        }
                    }
                }
            }

            bw.close();
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }
    }

    /**
     * Check paths for large relative delay violations.  Gives warnings.
     **/
    static public void checkPaths(CastDesign design, TransistorSizingTool tool) {
        double     tau = tool.getOptionUnitDelay();
        JautoMessageCenter      messageCenter = tool.getMessageCenter();
      
        // get the "worstTau" from worst relative constraint violation
        double worstTau = 0;
        for (Iterator ita = sortCellTypes(design.allCellTypes).iterator();
             ita.hasNext(); ) {
            CellType cta = (CellType) ita.next();
            double cellWorst = 0;
            if (cta.getLevel() > 0) { // mid cell
                for (Iterator itb = cta.getReducedCatPaths().iterator();
                     itb.hasNext(); ) {
                    CatPath cpa = (CatPath) itb.next();
                    for (int i = 0; i < cpa.delay.length; ++i) {
                        double t = tau * cpa.delay[i] / cpa.getBudget(i);
                        if (t>cellWorst) cellWorst = t;
                    }
                }
            } else { // leaf cell
                for (Iterator itb = cta.getSizingPaths().iterator();
                     itb.hasNext(); ) {
                    SizingPath spa = (SizingPath) itb.next();
                    if (!spa.isFragment()) {
                        for (int i = 0; i < spa.delay.length; ++i) {
                            double t = tau * spa.delay[i] / spa.getBudget(i);
                            if (t>cellWorst) cellWorst = t;
                        }
                    }
                }
            }
            if (cellWorst > worstTau) worstTau = cellWorst;
            if (cellWorst / tau > 1.01) {
                if (! quiet )
                    System.out.println("Cell Tau=" + 
                                   NumberFormatter.format(cellWorst*1e12,3) +
                                   "ps for " + cta.typeName);
            }
        }
        if (! quiet) 
            System.err.println("Target Tau=" +
                           NumberFormatter.format(tau*1e12,3) +
                           " Worst Tau=" + 
                           NumberFormatter.format(worstTau*1e12,3) + "ps");
    }

    /**
     * This function generates the sizing summay information (at the design level)
     * string for function "generateSizingSummary".
     * The sizing summary information includes total transistor counts,
     * total transistor width, average transistor width, maximum transistor width,
     * minimum transistor width, number of nodes (nets),
     * average transistor width per node, average wire length per node,
     * average gate capacitance per node, average wire capacitance per node,
     * average total capacitance per node.
     * <p>Finally it gives information about E*Tau^2.
     * <p>t -> Tau, C -> total cap, Ctt -> C*t*t, 
     * W -> ave tran width, GateC -> average gate cap % per node.
     **/
    static public String reportSizingResults(TransistorSizingTool tool)
    {
        String                  summary = "";

        CastDesign              design = tool.getSynthesizedDesign();
        double                  tau = tool.getOptionUnitDelay();
        double                  maxWidthN = tool.getOptionMaxWidthN();
        double                  maxWidthP = tool.getOptionMaxWidthP();
        TechnologyData          td = design.getTechnologyData();
        JautoMessageCenter      messageCenter = tool.getMessageCenter();

        // Report results for total transistor width
        Map/*<NetGraph.NetEdge,Integer>*/ transistorInstanceCountMap =
            new HashMap/*<NetGraph.NetEdge,Integer>*/();
        double totalWidth = 0.0;
        int numTransistors = 0;

        double maxWidth = Double.NEGATIVE_INFINITY;
        double minWidth = Double.POSITIVE_INFINITY;

        for (Iterator ita = sortCellTypes(design.allCellTypes).iterator();
             ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();
            int instanceCount = sta.getPhysicalInstanceCount();
            Set/*<NetGraph.NetEdge>*/ seenEdges =
                new HashSet/*<NetGraph.NetEdge>*/();
            double totalWidthForCell = 0.0;

            for (Iterator itb =
                    sortHalfOperators(sta.getListHalfOperators())
                        .iterator();
                 itb.hasNext(); ) {
                HalfOperator hoa = (HalfOperator)itb.next();

                double maxAllowedWidth;
                if (hoa.driveDirection ==
                        HalfOperator.DriveDirection.PULL_DOWN) {
                    maxAllowedWidth = maxWidthN;
                }
                else{
                    maxAllowedWidth = maxWidthP;
                }

                if (hoa.getCurrentSize() >= maxAllowedWidth) {
                    String msa = "WARNING";
                    String msb = "Variable value at maximum.\n";
                    String msc = "CellName: " + sta.typeName + "\n"
                        + "HalfOperatorName: " + hoa.outputNet.canonicalName.getCadenceString()
                        + (hoa.driveDirection ==
                               HalfOperator.DriveDirection.PULL_DOWN ?
                           "-" : "+") + "\n";

                    if (! quiet)
                        System.out.println(msa + ": " + msb + msc);

                    messageCenter.createMessage(1, 5, msa, msb, msc);
                }
                else{
                    // FIXME: hardcoded constant for warning threshold
                    if (hoa.getCurrentSize() >= 0.95 * maxAllowedWidth) {
                        String msa = "WARNING";
                        String msb = "Variable value within 5% of maximum.\n";
                        String msc = "CellName: " + sta.typeName + "\n"
                            + "HalfOperatorName: " + hoa.outputNet.canonicalName.getCadenceString()
                            + (hoa.driveDirection ==
                                   HalfOperator.DriveDirection.PULL_DOWN ?
                               "-" : "+") + "\n";

                        if (! quiet)
                            System.out.println(msa + ": " + msb + msc);

                        messageCenter.createMessage(1, 6, msa, msb, msc);
                    }
                }

                for (Iterator itc = hoa.transistors.iterator(); itc.hasNext(); ) {
                    NetGraph.NetEdge nea = (NetGraph.NetEdge)itc.next();
                    if (!seenEdges.contains(nea)) {

                        // FIXME: this is horrible
                        if((nea.width % 0.314159265E-6) == 0.0){
                            String msa = "ERROR";
                            String msb = "This transistor has been missed by the sizing algorithm.\n";
                            String msc = "Cell: " + sta.typeName
                                + ", gate: " + nea.gate.getName().getCadenceString()
                                + ", drain: " + nea.drain.getName().getCadenceString()
                                + ", source: " + nea.source.getName().getCadenceString()
                                + "\n";

                            messageCenter.createMessage(0, 2, msa, msb, msc);
                        }

                        if(nea.width > maxWidth){
                            maxWidth = nea.width;
                        }

                        if(nea.width < minWidth){
                            minWidth = nea.width;
                        }

                        totalWidthForCell += nea.width;

                        seenEdges.add(nea);
                        numTransistors += instanceCount;

                        transistorInstanceCountMap.put(nea, new Integer(instanceCount));
                    }
                }
            }

            totalWidth += totalWidthForCell * instanceCount;
        }

        double aveWidth = totalWidth / numTransistors;
        summary += "\n";
        summary += "************************** SUMMARY *************************" + "\n";
        summary += "Total   transistor count: " + numTransistors + "\n";
        summary += "Total   transistor width: " + totalWidth + "\n";
        summary += "Average transistor width: " + aveWidth + "\n";
        summary += "Maximum transistor width: " + maxWidth + "\n";
        summary += "Minimum transistor width: " + minWidth + "\n";

        totalWidth = 0.0;
        double totalWireLength = 0.0;
        double totalWireCap = 0.0;
        double totalGateCap = 0.0;
        int numNodes = 0;

        Map/*<NetGraph.NetEdge,Integer>*/ sinkTransistorInstanceCountMap =
            new HashMap/*<NetGraph.NetEdge,Integer>*/();

        for (Iterator ita = sortCellTypes(design.allCellTypes).iterator();
             ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();
            int instanceCount = sta.getPhysicalInstanceCount();

            for (Iterator itb = sta.getAllNets().iterator(); itb.hasNext(); ) {
                CellNet cna = (CellNet)itb.next();
                if(!cna.isPortNet()){
                    if(cna.globalNets != null){
                        assert cna.getGlobalNets().size() <= 1
                            : "Internal CellNet have more than 1 global net.\n" +
                              "CellName: " + sta.typeName + "\n" +
                              "NetName:" + cna.canonicalName.getCadenceString();
                        for (Iterator itc = cna.getGlobalNets().iterator(); itc.hasNext(); ) {
                            GlobalNet gna = (GlobalNet)itc.next();
    
                            numNodes += instanceCount;
                            totalWireLength += gna.getEstimatedWireLength() * instanceCount;
                            totalWireCap += gna.getWireCapacitance() * instanceCount;
    
                            double totalNSinkWidth = 0.0;
                            double totalPSinkWidth = 0.0;
                            double totalNSinkCap = 0.0;
                            double totalPSinkCap = 0.0;
                            for (Iterator itd = gna.getListSinks().iterator(); itd.hasNext(); ) {
                                NetSink nska = (NetSink)itd.next();
                                if(nska.type == NetType.HALF_OPERATOR_TRANSISTOR){
                                    NetGraph.NetEdge nea = nska.transistor;
                                    double width = nea.width / nea.shareCount;
                                    if (nea.type == DeviceTypes.N_TYPE) {
                                        totalNSinkWidth += width;
                                        totalNSinkCap += width * nea.length * td.getUnitNmosGateCapacitance(nea.getTransistorType());
                                    } else {
                                        totalPSinkWidth += width;
                                        totalPSinkCap += width * nea.length * td.getUnitPmosGateCapacitance(nea.getTransistorType());
                                    }

                                    Integer itea =
                                        (Integer) sinkTransistorInstanceCountMap.get(nea);
                                    if (itea != null) {
                                        Integer iteb = new Integer(itea.intValue() + instanceCount);
                                        sinkTransistorInstanceCountMap.put(nea, iteb);

                                        int k = ((Integer) transistorInstanceCountMap.get(nea)).intValue();

                                        assert iteb.intValue() <= nea.shareCount * k
                                            : "Transistor over-counting.\n" +
                                               iteb.intValue() +
                                               "   " + (nea.shareCount * k) +
                                               "   " + nea.shareCount +
                                               "   " + nska.sink.outputNet.canonicalName.getCadenceString() +
                                               "   " + cna.listSinks.size() +
                                               "   " + nska.sink.subType.typeName + "\n" +
                                               "Cell name: " + sta.typeName + "\n" +
                                               "Transistor gate name: " + nea.gate.name.getCadenceString();
                                    }
                                    else{
                                        sinkTransistorInstanceCountMap.put(nea, new Integer(0));
                                    }
                                }

                                assert nska.type != NetType.CELL
                                    : "Globalnet should only contain type-0 NetSource/NetSink at this stage.\n" +
                                      "CellName: " + sta.typeName + "\n" +
                                      "NetName: " + cna.canonicalName.getCadenceString();
                            }
    
                            totalWidth += instanceCount *
                                (totalNSinkWidth + totalPSinkWidth);
                            totalGateCap += instanceCount *
                                (totalNSinkCap + totalPSinkCap);
                        }
                    }
                }
            }
        }

        // summary variables
        double aveWireCap = totalWireCap / numNodes;
        double aveGateCap = totalGateCap / numNodes;
        double aveAllCap = aveWireCap + aveGateCap;
        double Ett = aveAllCap*tau*tau;

        // debugging output for this sizing run
        summary += "\n";
        summary += "Number of nodes: " + numNodes + "\n";
        summary += "Average transistor width per node: " + totalWidth / numNodes + "\n";
        summary += "Average wire length per node: " + totalWireLength / numNodes + "\n";
        summary += "Average gate capacitance per node: " + aveGateCap + "\n";
        summary += "Average wire capacitance per node: " + aveWireCap + "\n";
        summary += "Average capacitance per node: " + aveAllCap + "\n";
        summary += "************************************************************" + "\n";
        summary += "t=" + NumberFormatter.format(tau*1e12,3) + "ps" +
                   " C=" + NumberFormatter.format(aveAllCap*1e15,3) + "fF" +
                   " Ctt=" + NumberFormatter.format(Ett*1e39,1) + "fF*ps^2" +
                   " W=" + NumberFormatter.format(aveWidth*1e6,3) + "um" + 
                   " GateC=" + 
                   NumberFormatter.format(100*aveGateCap/aveAllCap,1) +"%" + "\n";
        summary += "************************************************************" + "\n";
        summary += "\n\n";

        if (! quiet)
            System.out.print(summary);
        return summary;

    }


    /**
     * Check for big transistor sizes.  Give warnings.
     **/
    static public void checkSizes(CastDesign design,
                                  TransistorSizingTool tool) {
        JautoMessageCenter messageCenter = design.getMessageCenter();
        for (Iterator ita = sortCellTypes(design.allCellTypes).iterator();
             ita.hasNext(); ) {
            CellType cta = (CellType)ita.next();
            if(cta.getListHalfOperators().size() > 0) {
                for (Iterator itb =
                         sortHalfOperators(cta.getListHalfOperators()).iterator();
                     itb.hasNext(); ) {
                    HalfOperator hoa = (HalfOperator)itb.next();
                    String s, driveDir;
                    final double warnWidth;
                    if (hoa.driveDirection ==
                        HalfOperator.DriveDirection.PULL_DOWN) {
                        s = "N";
                        driveDir = "-";
                        warnWidth = tool.getOptionNmosWarnWidth();
                    } else {
                        s = "P";
                        driveDir = "+";
                        warnWidth = tool.getOptionPmosWarnWidth();
                    }
                    
                    for (Iterator itc = hoa.depths.iterator(); itc.hasNext(); ) {
                        int i = ((Integer)itc.next()).intValue();
                        double f = hoa.getWidth(i);
                        
                        if ((warnWidth>=0) && (f > warnWidth)) {
                            String msa = "WARNING";
                            String msb =
                                "Transistor size larger than " +
                                s.toLowerCase() + "mosWarnWidth (" +
                                warnWidth * 1.0e6 + "u).\n";
                                String msc = "CellName: " + cta.typeName + "\n"
                                    + "HalfOperatorName: " + hoa.outputNet.canonicalName.getCadenceString()
                                    + (hoa.driveDirection ==
                                       HalfOperator.DriveDirection
                                       .PULL_DOWN ?
                                       "-" : "+") + "\n"
                                    + s + "W" + i + "=" + NumberFormatter.format(f*1e6, 3) + "u\n";
                                
                                messageCenter.createMessage(1, 7, msa, msb, msc);
                        }
                    }
                }
            }
        }
    }

    /**
     * This function writes out information about strength ratio between P/N stacks.
     **/
    static public void dumpStrengthRatioInformation(CastDesign design, String dirName, double thres)
    {
        File strengthRatioDebugFile =
            new File(dirName, "strength_ratio.debug");

        JautoMessageCenter  messageCenter = design.getMessageCenter();

        MultiMap/*<Double,String>*/ allStrengthRatios =
            new MultiMap/*<Double,String>*/(new TreeMap/*<Double,String>*/(),
                                            MultiMap.ARRAY_LIST_FACTORY);

        try{
            BufferedWriter bw = new BufferedWriter(new FileWriter(strengthRatioDebugFile));

            for (Iterator ita =
                    sortCellTypes(design.allCellTypes).iterator();
                 ita.hasNext(); ) {
                CellType cta = (CellType)ita.next();
                bw.write("CELL " + cta.typeName + " {\n");

                MultiMap/*<Double,String>*/ cellStrengthRatios =
                    new MultiMap/*<Double,String>*/
                        (new TreeMap/*<Double,String>*/(),
                         MultiMap.ARRAY_LIST_FACTORY);

                for (Iterator itb = cta.getAllNets().iterator(); itb.hasNext(); ) {
                    CellNet cna = (CellNet)itb.next();

                    Pair/*<Double,Double>*/ ratios = cna.getStrengthRatios();
                    double ratioPN = ((Double) ratios.getFirst()).doubleValue();
                    double ratioNP = ((Double) ratios.getSecond()).doubleValue();
                    double maxRatio = Math.max(ratioPN, ratioNP);

                    if (maxRatio > thres) {
                        String msa = "WARNING";
                        String msb = "Large pull-up/pull-down strength ratio: " + NumberFormatter.format(maxRatio, 3) + "\n";
                        String msc = "\tCellName = " + cta.typeName
                            + "; Net = " + cna.canonicalName.getCadenceString()
                            + "; PN = " + NumberFormatter.format(ratioPN, 3)
                            + "; NP = " + NumberFormatter.format(ratioNP, 3)
                            + "\n";

                        messageCenter.createMessage(1, 8, msa, msb, msc);
                    }

                    if (maxRatio > 0.0) {
                        String stra = cna.canonicalName.getCadenceString() 
                            + "; PN = " + NumberFormatter.format(ratioPN, 3)
                            + "; NP = " + NumberFormatter.format(ratioNP, 3)
                            + "\n";

                        String strb = cta.typeName + "; " + stra;

                        final Double negativeRatio = new Double(-maxRatio);
                        cellStrengthRatios.put(negativeRatio, stra);
                        allStrengthRatios.put(negativeRatio, strb);
                    }
                }

                for (Iterator itc = cellStrengthRatios.values().iterator(); itc.hasNext(); ) {
                    String s = (String) itc.next();
                    bw.write("\t" + s);
                }

                bw.write("}\n");
            }

            bw.write("ALL_SORTED_RATIO {\n");

            for (Iterator itc = allStrengthRatios.values().iterator(); itc.hasNext(); ) {
                String s = (String) itc.next();
                bw.write("\t" + s);
            }

            bw.write("}\n");
            bw.close();
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }

    }


    /**
     * This function writes out information about cell partition for sizing groups.
     * This information includes all fixed-size cells, sizable cells in each group.
     * Each final sizing group contains already-partitioned sizable cells and all
     * fixed-size cells.
     **/
    static public void dumpSubtypePartitionInformation
        (CastDesign design,
         List/*<List<CellType>>*/ groupedSubtypes,
         List/*<CellType>*/ fixedSubtypes,
         String dirName)
    {
        File subtypePartitionDebugFile =
            new File(dirName, "subtype_partition.debug");

        try{
            BufferedWriter bw = new BufferedWriter(new FileWriter(subtypePartitionDebugFile));

            bw.write("ALL FIXED SUBTYPES:\n");
            for (Iterator ita = fixedSubtypes.iterator(); ita.hasNext(); ) {
                CellType cta = (CellType)ita.next();
                bw.write("\t" + cta.typeName + "\n");
            }
            bw.write("\n");

            int i = 0;
            for (Iterator ita = groupedSubtypes.iterator(); ita.hasNext(); ) {
                bw.write("Group " + i + ":\n");
                List/*<CellType>*/ lsta = (List/*<CellType>*/) ita.next();
                for (Iterator itb = lsta.iterator(); itb.hasNext(); ) {
                    CellType cta = (CellType)itb.next();
                    bw.write("\t" + cta.typeName + "    " + cta.getNumPaths() + "\n");
                }
                bw.write("\n");
                ++i;
            }

            bw.close();
        }
        catch(IOException e){
            e.printStackTrace(System.err);
        }

    }

    private static class SdcConstraint {
        final CellType top;
        final HierName pathPrefix;
        final CellNet outputNet;
        final int driveDirection;
        double budget;
        double wireCap;
        double driveRes;
        final Collection<Pair<HierName,CellNet>> ports;

        public SdcConstraint(final CellType top,
                             final HierName pathPrefix,
                             final CellNet outputNet,
                             final int driveDirection,
                             final double budget,
                             final double wireCap,
                             final double driveRes,
                             final Collection<Pair<HierName,CellNet>> ports) {
            this.top = top;
            this.pathPrefix = pathPrefix;
            this.outputNet = outputNet;
            this.driveDirection = driveDirection;
            this.budget = budget;
            this.wireCap = wireCap;
            this.driveRes = driveRes;
            this.ports = ports;
        }
        private HierName concat(final HierName... names) {
            HierName result = names[0];
            for (int i = 1; i < names.length; ++i) {
                if (names[i] != null) {
                    result = HierName.append(result, names[i]);
                }
            }
            return result;
        }
        private HierName makeHierName(final HierName a, final String b) {
            try {
                return HierName.makeHierName(a, b);
            } catch (InvalidHierNameException e) {
                throw new RuntimeException(e);
            }
        }
        private HierName getHierarchy(final HierName inst, final CellNet net) {
            final Collection instList = new ArrayList();
            findHierarchy(top, inst, instList, new ArrayList());

            HierName result = null;
            for (Iterator i = instList.iterator(); i.hasNext(); ) {
                final HierName n = (HierName) i.next();
                result = makeHierName(result, n.getCadenceString());
            }

            result = makeHierName(result, net.canonicalName.getCadenceString());

            return result;
        }
        public TreeSet<HierName> getNames(final HierName inst) {
            final TreeSet<HierName> names = new TreeSet<HierName>();
            if (outputNet.isPortNet()) {
                names.add(getHierarchy(concat(inst, pathPrefix), outputNet));
            }
            for (Pair<HierName,CellNet> port : ports) {
                final HierName h = concat(inst, port.getFirst());
                if (h != null) names.add(getHierarchy(h, port.getSecond()));
            }
            return names;
        }
        public void write(final TreeSet<HierName> names, final PrintWriter w) {
            // look at the "deepest" port first
            if (!names.isEmpty()) {
                w.printf("%d %g %g %g", driveDirection, budget, wireCap,
                         driveRes);
                for (HierName instPort : names.descendingSet()) {
                    w.print(" " + instPort.getAsString('/'));
                }
                w.println();
            }
        }
        public void update(final double budget, final double wireCap,
                           final double driveRes) {
            this.budget = Math.min(this.budget, budget);
            this.wireCap = Math.max(this.wireCap, wireCap);
            this.driveRes = Math.max(this.driveRes, driveRes);
        }
        public boolean equals(final Object o) {
            return o instanceof SdcConstraint &&
                   ObjectUtils.equals(pathPrefix,
                                      ((SdcConstraint) o).pathPrefix) &&
                   driveDirection == ((SdcConstraint) o).driveDirection &&
                   outputNet == ((SdcConstraint) o).outputNet &&
                   ports.equals(((SdcConstraint) o).ports);
        }
        public int hashCode() {
            return ObjectUtils.hashCode(pathPrefix) +
                   System.identityHashCode(outputNet) +
                   driveDirection +
                   ports.hashCode();
        }
    }

    private static void traceNet(
            final HierName inst,
            final CellNet net,
            final Collection<Pair<HierName,CellNet>> result) {
        for (HierName h : (Set<HierName>) net.subcellconnectionNames) {
            ConnectionInfo ci = net.container.getSubcellConnectedTo(h);
            CellNet childNet = ci.child.getNet(ci.getChildName(h));
            final HierName newInst = HierName.append(inst, ci.nameInParent);
            result.add(new Pair<HierName,CellNet>(newInst,childNet));
            traceNet(newInst, childNet, result);
        }
    }

    private static Collection<Pair<HierName,CellNet>>
    traceNet(final HierName inst, final CellNet net) {
        final Collection<Pair<HierName,CellNet>> result =
            new HashSet<Pair<HierName,CellNet>>();
        traceNet(inst, net, result);
        return result;
    }

    private static void processSdcConstraint(
            final CastDesign design,
            final Map<String,Map<SdcConstraint,SdcConstraint>> constraintMap,
            final CellType pathContainer,
            final SizingPath lastSizingPath,
            final GlobalNet gn,
            final double sdcBudget) {
        final HalfOperator lastHalfOp =
            (HalfOperator) lastElement(lastSizingPath.getPath());

        final CellType gnContainer = gn.getTopCellNet().container;
        HierName pathPrefix = null;
        HierName gnPrefix = null;

        final CellType topMost;
        if (gnContainer == pathContainer) {
            topMost = gnContainer;
        } else {
            if (gnContainer.getLevel() > pathContainer.getLevel()) {
                topMost = gnContainer;
                HierName pathInst = null;
                for (Iterator j = gn.getListSources().iterator();
                     j.hasNext(); ) {
                    final NetSource src = (NetSource) j.next();
                    if (src.type == NetType.HALF_OPERATOR_TRANSISTOR &&
                        src.source == lastHalfOp) {
                        // XXX: what if the same global net is driving by
                        // multiple instances of the same cell?
                        pathInst = src.getInstanceName();
                        break;
                    }
                }
                pathPrefix = computePartialInstance(gnContainer,
                                                    pathContainer,
                                                    pathInst);
            } else {
                final HierName pathInst = lastSizingPath.getInstanceName();
                topMost = pathContainer;
                gnPrefix = computePartialInstance(pathContainer,
                                                  gnContainer,
                                                  pathInst);
            }
        }

        final double loadCap =
            gn.getGateCapacitance() + gn.getConstCapacitance();

        Map<SdcConstraint,SdcConstraint> constraints =
            constraintMap.get(topMost.typeName);
        if (constraints == null) {
            constraints = new HashMap<SdcConstraint,SdcConstraint>();
            constraintMap.put(topMost.typeName, constraints);
        }

        final HierName pathName;
        if (lastSizingPath.getInstanceName() == null) {
            pathName = pathPrefix;
        } else {
            pathName = HierName.append(pathPrefix,
                                       lastSizingPath.getInstanceName());
        }

        final SdcConstraint constraint =
            new SdcConstraint(design.getTopLevelCell(), pathName,
                              lastHalfOp.outputNet,
                              lastHalfOp.driveDirection, sdcBudget,
                              loadCap,
                              gn.getDriveResistance(lastHalfOp),
                              traceNet(gnPrefix, gn.getTopCellNet()));

        final SdcConstraint prev = constraints.get(constraint);
        if (prev == null) {
            constraints.put(constraint,constraint);
        } else {
            prev.update(sdcBudget, loadCap,
                        gn.getDriveResistance(lastHalfOp));
        }
    }

    private static boolean isSdcVisible(
            final GlobalNet gn,
            final MultiMap<GlobalNet,String> portNets,
            final Set<String> libs) {
        final Set<String> portContainers = (Set<String>) portNets.get(gn);
        if (portContainers != null) {
            final Set<String> intersect = new HashSet<String>(libs);
            intersect.retainAll(portContainers);
            return !intersect.isEmpty();
        }
        return false;
    }

    private static double getLastDelay(SizingPath path, int index) {
        final GlobalNet gn =
            (GlobalNet) path.getEndNet().getGlobalNets().get(index);
        final HalfOperator halfOp =
            (HalfOperator) lastElement(path.getPath());
        return gn.calculateDelay(halfOp);
    }

    private static void processSdcPaths(
            final CastDesign design,
            final DelayCalculator delayCalc,
            final Map<String,Map<SdcConstraint,SdcConstraint>> constraintMap,
            final CellType pathContainer,
            final AbstractPath path,
            final MultiMap<GlobalNet,String> portNets,
            final Set<String> sdcLibs,
            final double delayScale) {
        final Collection<SizingPath> sdcPaths = new ArrayList<SizingPath>();
        double partialDelay = 0;
        int partialCount = 0;
        SizingPath lastSizingPath = null;
        if (path instanceof CatPath) {
            final CatPath catPath = (CatPath) path;
            for (Iterator i = catPath.getCatPath().iterator(); i.hasNext(); ) {
                final SizingPath sizingPath = (SizingPath) i.next();
                if (i.hasNext()) {
                    final CellNet endNet = sizingPath.getEndNet();
                    final GlobalNet gn =
                        (GlobalNet) endNet.getGlobalNets().get(0);
                    if (isSdcVisible(gn, portNets, sdcLibs)) {
                        partialDelay += getLastDelay(sizingPath, 0);
                        partialCount += 1.5;
                        sdcPaths.add(sizingPath);
                    }
                } else {
                    lastSizingPath = sizingPath;
                }
            }
        } else {
            lastSizingPath = (SizingPath) path;
        }

        final List globalNets = path.getEndNet().getGlobalNets();
        for (int i = 0; i < path.slack.length; ++i) {
            final GlobalNet gn = (GlobalNet) globalNets.get(i);
            final double pathSlack =
                path.getBudget(i) - path.getDelay(i) * delayScale;
            final boolean visible = isSdcVisible(gn, portNets, sdcLibs);
            final double totalDelay =
                visible ? partialDelay + getLastDelay(lastSizingPath, i)
                        : partialDelay;
            final double totalCount =
                visible ? partialCount + 1 : partialCount;
            for (SizingPath sdcPath : sdcPaths) {
                final double delay = getLastDelay(sdcPath, 0);
                final double sdcBudget =
                    //pathSlack * (delay / totalDelay) + delay * delayScale;
                    pathSlack * (1.5 / totalCount) + delay * delayScale;
                processSdcConstraint(
                    design, constraintMap, pathContainer, sdcPath,
                    (GlobalNet) sdcPath.getEndNet().getGlobalNets().get(0),
                    sdcBudget);
            }
            if (visible) {
                final double delay = getLastDelay(lastSizingPath, i);
                final double sdcBudget =
                    //pathSlack * (delay / totalDelay) + delay * delayScale;
                    pathSlack / totalCount + delay * delayScale;
                processSdcConstraint(
                    design, constraintMap, pathContainer, lastSizingPath,
                    gn, sdcBudget);
            }
        }
    }

    public static void dumpSdcConstraints(CastDesign design,
                                          DelayCalculator delayCalc,
                                          double delayScale,
                                          MultiMap<GlobalNet,String> portNets,
                                          Set<String> sdcLibs,
                                          String dirName)
        throws IOException {
        final File out = new File(dirName, "sdc.debug");
        final PrintWriter pw =
            new PrintWriter(new BufferedWriter(new FileWriter(out)));
        final Map<String,Map<SdcConstraint,SdcConstraint>> constraintMap =
            new HashMap<String,Map<SdcConstraint,SdcConstraint>>();

        for (Iterator ita = sortCellTypes(design.allCellTypes).iterator();
             ita.hasNext(); ) {
            CellType cta = (CellType)ita.next();

            for (Iterator pathIterator = getPathIterator(cta);
                 pathIterator.hasNext(); ) {
                final AbstractPath path = (AbstractPath) pathIterator.next();
                processSdcPaths(design, delayCalc, constraintMap, cta, path,
                                portNets, sdcLibs, delayScale);
            }
        }

        pw.println("VERSION 2");

        final MultiSet allConstraints = new MultiSet(
                new Comparator<Pair<TreeSet<HierName>,SdcConstraint>>() {
                    public int compare(
                        final Pair<TreeSet<HierName>,SdcConstraint> p1,
                        final Pair<TreeSet<HierName>,SdcConstraint> p2) {
                        int x = p1.getFirst().first().compareTo(
                                    p2.getFirst().first());
                        if (x == 0) {
                            x = p1.getSecond().driveDirection -
                                p2.getSecond().driveDirection;
                        }
                        return x;
                    }
                });

        design.getTopLevelCell().walkMany(
            new CellType.InstanceProcessor() {
                public void processInstance(CellType t, HierName inst,
                                            Collection<ConnectionInfo> path) {
                    final Map<SdcConstraint,SdcConstraint> constraints =
                        constraintMap.get(t.typeName);
                    
                    if (constraints != null) {
                        for (SdcConstraint constraint : constraints.values()) {
                            final TreeSet<HierName> names =
                                constraint.getNames(inst);
                            if (!names.isEmpty()) {
                                allConstraints.add(
                                    new Pair<TreeSet<HierName>,SdcConstraint>
                                        (names, constraint));
                            }
                        }
                    }
                }
            }
        );
        for (Iterator i = allConstraints.iterator(); i.hasNext(); ) {
            final Pair<TreeSet<HierName>,SdcConstraint> p =
                (Pair<TreeSet<HierName>,SdcConstraint>) i.next();
            final TreeSet<HierName> names = p.getFirst();
            final SdcConstraint constraint = p.getSecond();
            constraint.write(names, pw);
        }

        pw.close();
    }

    /**
     * Convert current real time into better looking string.
     **/
    public static /*@ non_null @*/ String getCurrentTime() {
        return new SimpleDateFormat("HH:mm:ss MM/dd/yyyy").format(new Date());
    }
}
