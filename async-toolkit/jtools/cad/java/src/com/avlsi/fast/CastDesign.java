/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.fast;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import java.util.List;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import com.avlsi.tools.jauto.TransistorSizingTool;
import com.avlsi.tools.jauto.NetSource;
import com.avlsi.tools.jauto.NetSink;
import com.avlsi.tools.jauto.NetType;
import com.avlsi.tools.jauto.DebugOption;
import com.avlsi.tools.jauto.TechnologyData;
import com.avlsi.tools.jauto.JautoMessageCenter;

import com.avlsi.cast.CastFile;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellImpl;
import com.avlsi.cell.CellUtils;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.cdl.CDLFileFormatException;
import com.avlsi.file.cdl.parser.AspiceCellAdapter;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;

import com.avlsi.io.SearchPath;
import com.avlsi.io.SearchPathFile;

import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.UnimplementableProductionRuleException;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.dsim.InstanceData;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.text.StringUtil;

/**
 * Class to represent a cast design.
 *
 * @author Aaron Denney
 * @version $Date$
 **/

public final class CastDesign {
    private final Map/*<String,CellType>*/ allCells        = new TreeMap/*<String,CellType>*/();

    /** flags for each cell.  Has estimated positions and sizes. **/
    public static final int FLOORPLAN = 1;
    /** flags for each cell.  Has transistors. **/
    public static final int TRANSISTORS = 2;
    /** flags for each cell.  Has transistors, and their size has been specified. **/
    public static final int TRANSISTORS_SIZED = 4;
    /** flags for each cell.  Has transistors in place.  Routed? **/
    public static final int TRANSISTORS_PLACED_SIZED = 8;
    
    /** set of all celltypes in the design **/
    public Set/*<CellType>*/ allCellTypes =
        new TreeSet/*<CellType>*/(CellType.getNameComparator());

    final TechnologyData        techData;

    public JautoMessageCenter   messageCenter;

    /** Names for Vdd, GND, and _RESET */
    private final HierName Vdd, GND, _RESET;

    /** Cadencize to use */
    private final Cadencize cad;

    /**
     * Cast parser to store for later use by
     * <code>GlobalNet.getDelayFunction</code>.
     **/
    private final CastFileParser castFileParser;

    private final float tau;

    /**
     * A special delaybias that applies only to fixed size cells.
     **/
    private final float fixedDelayBias;

    /**
     * A special delaybias that applies only to sizable cells.
     **/
    private final float sizableDelayBias;

    private final boolean keepCsp;

    /**
     * A collection of cell types that contains anonymous instances that aren't
     * wiring cells.
     **/
    private final Set<String> anonymousInstances = new TreeSet<String>();

    /**
     * A collection of cell types that have been synthesized as part of the
     * sizing environment.
     **/
    private final Set<String> synthCells;

    /**
     * The real top level cell to be sized, if an external sizing environment
     * was synthesized.
     **/
    private final CellInterface realTop;

    /**
     * Whether to use Steiner trees for wirelength and wirespan estimation.
     **/
    private final boolean useSteinerTree;

    //Constructs a cast design from a cell interface.
    public CastDesign(final CellInterface theCell, final HierName Vdd,
                      final HierName GND, final HierName _RESET,
                      final float tau, final TechnologyData techData,
                      final CastFileParser cfp) {
        this(theCell, Vdd, GND, _RESET, tau, techData, new Cadencize(true),
             cfp, 1, Collections.<String>emptySet(), new HashSet(), false,
             false);
    }

    private static CellInterface createFakeEnv(final CellInterface cell) {
        final CellImpl fakeEnv = new CellImpl( "" );
        final HierName fakeInstName = HierName.makeHierName( "top" );
        fakeEnv.addSubcellPair( fakeInstName, cell, false );
        return fakeEnv;
    }

    public CastDesign(final CellInterface theCell, final HierName Vdd,
                      final HierName GND, final HierName _RESET,
                      final float tau, final TechnologyData techData,
                      final Cadencize cad, final CastFileParser cfp,
                      final boolean useAstaExtraDelay) {
        this(theCell, Vdd, GND, _RESET, tau, techData, cad, cfp, 1,
             Collections.<String>emptySet(), new HashSet(), false,
             useAstaExtraDelay);
    }

    public CastDesign(final CellInterface theCell, final HierName Vdd,
                      final HierName GND, final HierName _RESET,
                      final float tau, final TechnologyData techData,
                      final Cadencize cad, final CastFileParser cfp,
                      final float fixedDelayBias,
                      final Set<String> synthCells,
                      final Set skipCells,
                      final boolean keepCsp,
                      final boolean useAstaExtraDelay) {
        this(Vdd, GND, _RESET, tau, techData, cad, cfp, createFakeEnv(theCell),
             null, fixedDelayBias, 1, synthCells, skipCells, keepCsp,
             useAstaExtraDelay, false);
    }

    public CastDesign(final CellInterface theCell, final HierName Vdd,
                      final HierName GND, final HierName _RESET,
                      final float tau, final TechnologyData techData,
                      final Cadencize cad, final CastFileParser cfp,
                      final float fixedDelayBias,
                      final float sizableDelayBias,
                      final Set<String> synthCells,
                      final Set skipCells,
                      final boolean keepCsp,
                      final boolean useAstaExtraDelay) {
        this(Vdd, GND, _RESET, tau, techData, cad, cfp, createFakeEnv(theCell),
             null, fixedDelayBias, sizableDelayBias, synthCells, skipCells,
             keepCsp, useAstaExtraDelay, false);
    }

    public CastDesign(final HierName Vdd, final HierName GND,
                       final HierName _RESET, final float tau,
                       final TechnologyData techData, final Cadencize cad,
                       final CastFileParser cfp, final CellInterface top,
                       final CellInterface realTop,
                       final float fixedDelayBias,
                       final float sizableDelayBias,
                       final Set<String> synthCells,
                       final Set skipCells,
                       final boolean keepCsp,
                       final boolean useAstaExtraDelay,
                       final boolean useSteinerTree){
        this.Vdd = Vdd;
        this.GND = GND;
        this._RESET = _RESET;
        this.tau = tau;
        this.techData = techData;
        this.cad = cad;
        this.castFileParser = cfp;
        this.fixedDelayBias = fixedDelayBias;
        this.sizableDelayBias = sizableDelayBias;
        this.keepCsp = keepCsp;
        this.synthCells = synthCells;
        this.realTop = realTop;
        this.useSteinerTree = useSteinerTree;
        convertSubcellsFromCast(cad, top, skipCells, useAstaExtraDelay);
    }

    /**
     * Find the minimum delaybias for each cell type.  The minimum delaybias
     * gives the most conservative delay budget.
     **/
    private void updateDelayBias(final CellInterface cell,
                                 final InstanceData instData,
                                 final Map result) {
        if (cell.isNode()) return;

        final String type = cell.getFullyQualifiedType();
        final float curr = instData.getDelayBias(null);
        final Float bias = (Float) result.get(type);
        if (bias == null) result.put(type, new Float(curr));
        else if (curr < bias.floatValue()) result.put(type, new Float(curr));
        else return;

        for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final HierName inst = (HierName) p.getFirst();
            final CellInterface subcell = (CellInterface) p.getSecond();
            updateDelayBias(subcell,
                            instData.translate(cell, subcell, inst, cad),
                            result);
        }
    }

    public final JautoMessageCenter setMessageCenter(JautoMessageCenter mc)
    {
        messageCenter = mc;

        return messageCenter;
    }

    public final JautoMessageCenter getMessageCenter()
    {
        return messageCenter;
    }

    public CellType getCell(String name) {
        return (CellType)allCells.get(name);
    }

    public CellType getTopLevelCell() {
        return getCell("");
    }

    public CellType getRealTopCell() {
        return realTop == null ? null
                               : getCell(realTop.getFullyQualifiedType());
    }


    public TechnologyData getTechnologyData(){
        return techData;
    }

    public /*@ non_null @*/ Cadencize getCadencize() {
        return cad;
    }

    public CastFileParser getCastFileParser() {
        return castFileParser;
    }

    public /*@ non_null @*/ CellInterface getCellForGate(
            final NetGraph.GateInstance gate) {
        try {
            return castFileParser.getFullyQualifiedCell(gate.getType());
        } catch (CastSemanticException e) {
            // We can't get an exception here because the file has
            // already been parsed, and has been cached, so it can't
            // have any errors.
            throw new AssertionError(e);
        }
    }

    // FIXME: Should this be moved to the CellType constructor?
    /**
     * Converts cells from CellInterface to CellType, storing
     * result in <code>allCells</code>.
     **/
    private void convertSubcellsFromCast(final Cadencize c,
                                         final CellInterface start,
                                         final Set skipCells,
                                         final boolean useAstaExtraDelay) {
        if (start.isNode()) {
            return;
        }
        // if (!start.hasRealProductionRule()) {
        //     return;
        // }
        String type = start.getFullyQualifiedType();
        if (allCells.containsKey(type)) {
            return;
        }

        CadenceInfo ci = c.convert(start);

        if (type.equals("$env")) { type = ""; };

        final boolean isInternalEnv = synthCells.contains(type);
        if (!isInternalEnv && !type.equals("")) {
            for (Iterator i = start.getAllSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName n = (HierName) p.getFirst();
                final CellInterface sc = (CellInterface) p.getSecond();
                if (n.isAnonymous() && !CellUtils.isWiring(sc)) {
                    anonymousInstances.add(start.getFullyQualifiedType());
                    break;
                }
            }
        }

        for (Iterator i = start.getSubcellPairs(); i.hasNext(); ) {
            Pair p = (Pair) i.next();
            HierName n = (HierName) p.getFirst();
            CellInterface sc = (CellInterface) p.getSecond();
            convertSubcellsFromCast(c, sc, skipCells, useAstaExtraDelay);
            // FIXME: Preserve connectivity, create subcell connections, etc.
        }

        CellType ct =
            new CellType(this, c, start, Vdd, GND, _RESET, isInternalEnv,
                         useAstaExtraDelay);
        allCells.put(type, ct);
        for (Iterator i = start.getSubcellPairs(); i.hasNext(); ) {
            Pair p = (Pair) i.next();
            HierName n = (HierName) p.getFirst();
            CellInterface sc = (CellInterface) p.getSecond();
            final boolean hasCsp = keepCsp && sc.hasRunnableCsp();
            final boolean hasRouted = CellUtils.hasRouted(sc);
            if (sc.isNode() || skipCells.contains(sc.getFullyQualifiedType())) {
                continue;
            }
            CellType sct = this.getCell(sc.getFullyQualifiedType());

            // if a wiring cell has no directives, then it cannot affect the
            // sizing process at all, so discard it here
            if (sct.isWiringCell &&
                DirectiveUtils.getDirectiveBlock(sc.getBlockInterface())
                    == null &&
                !hasCsp &&
                !hasRouted) {
                continue;
            }

            ConnectionInfo conn = new ConnectionInfo(ct, sct, n, c);
            if (!sct.isWiringCell || hasCsp || hasRouted) {
                ct.subcellconnections.put(n, conn);
                sct.parentcellconnections.add(conn);
            }
            for (ConnectionInfo.NameIterator j = conn.parentIterator(); j.hasNext(); ) {
                HierName s = j.next();
                ct.subcellports.put(s, conn);
                ct.getNet(s).changeToSubcellconnection(s, sct.isWiringCell);
            }
        }

    }

    public Collection<String> getAnonymousInstances() {
        return Collections.unmodifiableSet(anonymousInstances);
    }

    /**
     * Instance and connect the global nets, assuming c is a top-level cell.
     **/
    public void addGlobalNets(CellType c) {
        // FIXME:
    }
    
    public static NetGraph getGateNetGraph(CastFileParser cfp,
            String cellName, HierName Vdd, HierName GND, Cadencize cad)
        throws CastSemanticException,UnimplementableProductionRuleException {

        final NetGraph netgraph =
            new NetGraph(null,null,null, Vdd, GND, Collections.EMPTY_SET);
        final CellInterface ci;
        ci = cfp.getFullyQualifiedCell(cellName);
        netgraph.addCellInterface(ci, new NetGraph[0], cfp, cad);
        netgraph.gateType = cellName;
        final boolean stack = ((Boolean) DirectiveUtils.getTopLevelDirective(ci, DirectiveConstants.STACK_GATE)).booleanValue();
        if (!stack) netgraph.prepareForLvs();
        return netgraph;
    }

    public void print()
    {
        System.out.println("-----------------------------------------------------------");
        System.out.println("List of all cell types in this design:");
        for (Iterator ita = allCellTypes.iterator(); ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();
            System.out.println(sta.toString());
        }
        System.out.println("-----------------------------------------------------------");
    }


    public String toString()
    {
        String s = "-------------------------------------------------------\n";
        s += "List of all cell types in this design:\n";

        for (Iterator ita = allCellTypes.iterator(); ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();
            s += sta.toString();
        }
        s += "-----------------------------------------------------------\n";

        return s;
    }


    /**
     * Function to generate HalfOperator data structures for cells.
     **/
    public void generateHalfOperators(Set/*<CellType>*/ cellTypes)
    {
        if(DebugOption.printLevel <= 2){
            System.out.println("-----------------------------------------------------------");
            System.out.println("Generating half-operators");
        }

        for (Iterator ita = cellTypes.iterator(); ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();
            generateHalfOperator(sta);
        }

        if(DebugOption.printAll){
            System.out.println("-----------------------------------------------------------");
        }
    }

    /**
     * Generate HalfOperator data structures for the given cell.
     **/
    public void generateHalfOperator(final CellType sta) {
        final double cellDelayBias = sta.getDelay().getCellDelayBias();

        if(DebugOption.printLevel <= 2){
            System.out.println("************* Processing Cell: " + sta.typeName);
        }

        if(sta.transistors.getEdges().size() == 0){ // there is no transistor in this cell
            if(DebugOption.printAll){
                System.out.println("This is a cell with no transistors");
                System.out.println("*************\n");
            }
            return;
        }


        final float celltau = tau;

        // reset sizes of all transistors to -infinity, depth to -1
        // Anything less than the minumum possible size or depth is fine.
        // TODO: Maybe should set transistor length too?
        Collection transistors = (Collection) sta.transistors.getNodes();
        for (Iterator itb = transistors.iterator(); itb.hasNext(); ) {
            NetGraph.NetNode nna = (NetGraph.NetNode)itb.next();
            
            for (Iterator itc = nna.edges.iterator(); itc.hasNext(); ) {
                NetGraph.NetEdge nea = (NetGraph.NetEdge)itc.next();
                nea.size = Double.NEGATIVE_INFINITY;
                nea.depth = -1;
            }
        }

        // create the set of all output nodes
        // not including the output node of the first inverter in the staticizer
        Set/*<NetGraph.NetNode>*/ normalOutputNodes =
            new TreeSet/*<NetGraph.NetNode>*/();
        for (Iterator itb = transistors.iterator(); itb.hasNext(); ) {
            NetGraph.NetNode nna = (NetGraph.NetNode)itb.next();
            if(DebugOption.printAll){
                System.out.print("Node: \"" + nna.name.getCadenceString() + "\" is ");
            }

            if(nna.isOutput()){
                if(nna.isStaticizerInverter()){
                    if(DebugOption.printAll){
                        System.out.println("staticizer output node");
                    }
                }
                else{
                    normalOutputNodes.add(nna);
                    if(DebugOption.printAll){
                        System.out.println("normal output node");
                    }
                }

            }
            else{
                if(DebugOption.printAll){
                    System.out.println("not output node.");
                }
            }
        }
        if(DebugOption.printAll){
            System.out.println("Number of output nodes: " +
                               normalOutputNodes.size());
            System.out.println("*************\n");
        }


        // generate all half-operators
        List/*<HalfOperator>*/ halfOperators =
            sta.getListHalfOperators();

        boolean nonDefaultLength = false;

        for (Iterator itb = normalOutputNodes.iterator();
             itb.hasNext(); ) {
            NetGraph.NetNode nna = (NetGraph.NetNode)itb.next();
            HierName nodename = nna.name;

            // find paths, check if operator is combinational or a library gate
            nna.findPaths();
            boolean isCombinational = nna.isCombinational();
            boolean isLibraryGate = (nna.getGate() != null);

            // debugging
            if (DebugOption.printAll) {
                System.out.println("*************************");
                System.out.println("Created 2 new half-operators");
                System.out.println("node=" + nna.name + 
                                   " combinational=" + isCombinational + 
                                   " libraryGate=" + isLibraryGate);
            }
            
            // delaybias from non-CAST source
            final double externalDelayBias =
                sta.isFixedSize() ? sta.design.fixedDelayBias
                                  : sta.design.sizableDelayBias;

            // create pull-down half-operator
            HalfOperator dnHalfOp = new HalfOperator();
            dnHalfOp.driveDirection = HalfOperator.DriveDirection.PULL_DOWN;
            dnHalfOp.outputNode = nna;
            dnHalfOp.setDelayBias(sta.getDelay().getDelay(nna.name, false, celltau) * externalDelayBias / tau);
            dnHalfOp.subType = sta;
            dnHalfOp.setSymmetrizationFactor(getDownSymmetrizationFactor(nna, sta));
            dnHalfOp.isCombinational = isCombinational;
            dnHalfOp.isLibraryGate = isLibraryGate;

            // create pull-up half-operator
            HalfOperator upHalfOp = new HalfOperator();
            upHalfOp.driveDirection = HalfOperator.DriveDirection.PULL_UP;
            upHalfOp.outputNode = nna;
            upHalfOp.setDelayBias(sta.getDelay().getDelay(nna.name, true, celltau) * externalDelayBias / tau);
            upHalfOp.subType = sta;
            upHalfOp.setSymmetrizationFactor(getUpSymmetrizationFactor(nna, sta));
            upHalfOp.isCombinational = isCombinational;
            upHalfOp.isLibraryGate = isLibraryGate;
            
            boolean found = false;
            for (Iterator ite = sta.getAllNets().iterator(); ite.hasNext(); ) {
                CellNet cna = (CellNet)ite.next();
                if (cna.canonicalName.equals(nna.name)) {
                    found = true;

                    dnHalfOp.outputNet = cna;
                    dnHalfOp.minDelay = cellDelayBias * cna.getDownMinDelay();
                    dnHalfOp.setStrengthBias(cna.getDownStrengthBias());
                    if (dnHalfOp.getStrengthBias() > 1.0) {
                        System.err.println("NOTE: Got strength bias: " + dnHalfOp.getStrengthBias() 
                            + " on " + sta.typeName + "/" +
                            cna.canonicalName + "-");
                    }
                    upHalfOp.outputNet = cna;
                    upHalfOp.minDelay = cellDelayBias * cna.getUpMinDelay();
                    upHalfOp.setStrengthBias(cna.getUpStrengthBias());
                    if (upHalfOp.getStrengthBias() > 1.0) {
                        System.err.println("NOTE: Got strength bias: " + upHalfOp.getStrengthBias() 
                            + " on " + sta.typeName + "/" +
                            cna.canonicalName + "+");
                    }
                    break;
                }
            }

            assert found
                : "Output netnode doesn't have corresponding cellnet\n" +
                   sta.toString() + "\n" +
                   "-----> Name of the netnode: " +
                   nna.name.getCadenceString();

            Float effectiveResistanceUp = null;
            Float effectiveResistanceDown = null;
            if (techData.getUseIntrinsicCap()) {
                final NetGraph.GateInstance gate = nna.getGate();
                if (gate != null) {
                    final HierName portName =
                        gate.getPortName(nna.getName());
                    final CellInterface gateCell = getCellForGate(gate);
                    effectiveResistanceUp =
                        (Float) DirectiveUtils
                            .getHalfOpDirectiveValue(gateCell,
                                DirectiveConstants.EFFECTIVE_RESISTANCE,
                                portName, true, cad);
                    effectiveResistanceDown =
                        (Float) DirectiveUtils
                            .getHalfOpDirectiveValue(gateCell,
                                DirectiveConstants.EFFECTIVE_RESISTANCE,
                                portName, false, cad);
                }
            }

            // Do a first pass through the paths to compute the maxDepth
            int maxDepthDown = 0;
            int maxDepthUp = 0;
            for (Iterator iPath = nna.getPaths().iterator();
                 iPath.hasNext(); ) {
                NetGraph.NetPath netPath =
                    (NetGraph.NetPath) iPath.next();
                // Replicate control of next loop
                if (!netPath.isFeedBack()) {
                    final int numEdges = netPath.getEdges().size();
                    if (netPath.getType() == DeviceTypes.N_TYPE) {
                        maxDepthDown = Math.max(maxDepthDown, numEdges);
                    } else {
                        assert netPath.getType() == DeviceTypes.P_TYPE;
                        maxDepthUp = Math.max(maxDepthUp, numEdges);
                    }
                }
            }

            // calculate relative size of the transistors with corresponding half-operator
            for (Iterator itc = nna.getPaths().iterator(); itc.hasNext(); ) {
                NetGraph.NetPath npa = (NetGraph.NetPath)itc.next();

                if(DebugOption.printLevel <= 1){
                    System.out.println("**********************************************");
                    System.out.println(npa.toString());
                    System.out.println("**********************************************");
                }

                int pathType = npa.getType(); // type of the path
                if(!npa.isFeedBack()){
                    if(DebugOption.printLevel <= 1){
                        System.out.println("**********************************************");
                        System.out.println(npa.toString());
                        System.out.println("**********************************************");
                    }

                    List/*<NetGraph.NetEdge>*/ edges = npa.getEdges();
                    int numEdges = edges.size();

                    boolean floating = true;
                    for (Iterator ei = edges.iterator(); ei.hasNext(); ) {
                        final NetGraph.NetEdge edge = (NetGraph.NetEdge) ei.next();
                        floating = floating && edge.floating;
                    }

                    double symmetrizeFactor = 1.0;

                    if (pathType == DeviceTypes.N_TYPE) {
                        dnHalfOp.addDepths(numEdges, floating);
                        symmetrizeFactor =
                            dnHalfOp.getSymmetrizationFactor();
                    } else {
                        assert pathType == DeviceTypes.P_TYPE;
                        upHalfOp.addDepths(numEdges, floating);
                        symmetrizeFactor =
                            upHalfOp.getSymmetrizationFactor();
                    }

                    if(!(npa.getEndNode().isGND() || npa.getEndNode().isVdd())){
                        if(DebugOption.printLevel <= 3) {
                            System.out.println("NOTE: Transistor sharing between half-operators found, not processed.");
                        }
                    }

                    // assign relative sizes to the transistors on this path
                    for (Iterator itd = edges.iterator(); itd.hasNext(); ) {
                        NetGraph.NetEdge nea = (NetGraph.NetEdge)itd.next();

                        final double resistanceRatio;
                        if (pathType == DeviceTypes.N_TYPE) { // PULL_DOWN
                            final double[] erf =
                                techData.getEffectiveResistanceFactorN(nea.getTransistorType());
                            if (effectiveResistanceDown != null) {
                                resistanceRatio =
                                    (effectiveResistanceDown
                                         .floatValue() / erf[0]) *
                                    (erf[numEdges - 1] /
                                     erf[maxDepthDown - 1]);
                            } else {
                                final int limitedDepth =
                                    Math.min(numEdges,
                                             techData.stackLimitN);
                                resistanceRatio =
                                    erf[limitedDepth - 1] / erf[0];
                            }
                        } else { // PULL_UP
                            assert pathType == DeviceTypes.P_TYPE;
                            final double[] erf =
                                techData.getEffectiveResistanceFactorP(nea.getTransistorType());
                            if (effectiveResistanceUp != null) {
                                resistanceRatio =
                                    (effectiveResistanceUp
                                         .floatValue() / erf[0]) *
                                    (erf[numEdges - 1] /
                                     erf[maxDepthUp - 1]);
                            } else {
                                final int limitedDepth =
                                    Math.min(numEdges,
                                             techData.stackLimitP);
                                resistanceRatio =
                                    erf[limitedDepth - 1] / erf[0];
                            }
                        }

                        double effectiveWidth = resistanceRatio * numEdges *
                            (nea.length / techData.getDefaultGateLength());

                        if(symmetrizeFactor > 1.0){
                            if(DebugOption.printLevel <= 2){
                                System.out.println("Note: symmetrized transistor found");
                            }
                            effectiveWidth = effectiveWidth / symmetrizeFactor;
                        }

                        // compute max size and depth
                        if (nea.size < effectiveWidth)
                            nea.size = effectiveWidth;
                        if (nea.depth < numEdges)
                            nea.depth = numEdges;

                        if (pathType == DeviceTypes.N_TYPE) { // PULL_DOWN
                            if(DebugOption.printAll){
                                System.out.println("Added PULL_DOWN transistor: " 
                                    + nea.gate.name + " Size: " + nea.size);
                            }
                            if (dnHalfOp.transistors.contains(nea)) {
                                if(DebugOption.printLevel <= 2){
                                    System.out.println("Note: Transistor sharing found within same half-operator");
                                }
                            }
                            else{
                                dnHalfOp.transistors.add(nea);
                            }
                        } else { // PULL_UP
                            assert pathType == DeviceTypes.P_TYPE;
                            if(DebugOption.printAll){
                                System.out.println("Added PULL_UP transistor: " 
                                    + nea.gate.name + " Size: " + nea.size);
                            }
                            if (upHalfOp.transistors.contains(nea)) {
                                if(DebugOption.printLevel <= 2){
                                    System.out.println("Note: Transistor sharing found within same half-operator");
                                }
                            }
                            else{
                                upHalfOp.transistors.add(nea);
                            }
                        }
                    }
                    if(DebugOption.printAll){
                        System.out.println("***************");
                    }
                }
            }
            if(DebugOption.printAll){
                System.out.println("*************************");
            }

            // Find the total up and down sizes attached to output node
            // this will be used to compute the diffusion cap.
            double downSize = 0.0;
            double upSize = 0.0;
            for (Iterator iEdge = nna.getEdges(); iEdge.hasNext(); ) {
                final NetGraph.NetEdge edge =
                    (NetGraph.NetEdge) iEdge.next();
                assert edge.source == nna || edge.drain == nna;
                // The size will be NEGATIVE_INFINITY if the transistor
                // is not part of a half-operator
                if (edge.size != Double.NEGATIVE_INFINITY) {
                    if (edge.type == DeviceTypes.N_TYPE)
                        downSize += edge.size;
                    else
                        upSize += edge.size;
                }
            }
            dnHalfOp.setOutputDiffusionLength(downSize);
            upHalfOp.setOutputDiffusionLength(upSize);

            if (dnHalfOp.transistors.size() != 0) {
                halfOperators.add(dnHalfOp);
                nonDefaultLength |= checkTransistorLength(dnHalfOp);
            }
            else{
                if(DebugOption.printAll){
                    System.out.println("Warning: this half-operator does not have transistors");
                    System.out.println("Will not be added to the list");
                    System.out.println(dnHalfOp.toString());
                }
            }

            if (upHalfOp.transistors.size() != 0) {
                halfOperators.add(upHalfOp);
                nonDefaultLength |= checkTransistorLength(upHalfOp);
            }
            else{
                if(DebugOption.printAll){
                    System.out.println("Warning: this half-operator does not have transistors");
                    System.out.println("Will not be added to the list");
                    System.out.println(upHalfOp.toString());
                }
            }
        }

        if (nonDefaultLength) {
            System.err.println("WARNING: cell " + sta.typeName +
                    " contains transistors with non-default length.");
        }

        if(DebugOption.printLevel <= 3){
            System.out.println("----------------------------------------------------------------");
            System.out.println("Halfoperator coverage check for cell: " + sta.typeName);
            String stra = sta.checkHalfOperatorCoverage();
            System.out.println("Result: " + stra);
            System.out.println("----------------------------------------------------------------");
        }
    }

    private boolean checkTransistorLength(final HalfOperator op) {
        final double l =
            op.subType.design.getTechnologyData().getDefaultGateLength();
        boolean nonDefaultLength = false;
        for (NetGraph.NetEdge nea : op.transistors) {
            if (nea.length != l) nonDefaultLength = true;
        }
        if (nonDefaultLength) {
            final String dir =
                op.driveDirection ==
                    HalfOperator.DriveDirection.PULL_DOWN ? "-" : "+";
            System.out.println(
                    "INFO: half-operator " + op.subType.typeName + "/" +
                    op.outputNode.name + dir +
                    " contains transistors with non-default length.");
        }
        return nonDefaultLength;
    }

    private double getUpSymmetrizationFactor(final NetGraph.NetNode netNode,
                                             final CellType cellType) {
        return getSymmetrizationFactor(netNode, netNode.getPaths(),
                                       DeviceTypes.P_TYPE, cellType, this);
    }

    private double getDownSymmetrizationFactor(final NetGraph.NetNode netNode,
                                               final CellType cellType) {
        return getSymmetrizationFactor(netNode, netNode.getPaths(), 
                                       DeviceTypes.N_TYPE, cellType, this);
    }

    /**
     * Get the symmetrization factor for a half-operator.  Pass-gates
     * are ignored unless the half-operator consists entirely of
     * pass-gates, in which case the symmetrization factor is assumed
     * to be 1.
     **/
    public static double getSymmetrizationFactor
        (final NetGraph.NetNode netNode,
         final Collection netPaths,
         final int pathType,
         final CellType cellType,
         final CastDesign design) {
        assert (pathType==DeviceTypes.N_TYPE)||(pathType==DeviceTypes.P_TYPE)
            : "NetPath.type should be either N_TYPE or P_TYPE";

        // set to zero to signal that the symmetrization factor is unknown
        double symmetrizationFactor = 0;
        boolean allPathsPass = true;
        for (Iterator iPath = netPaths.iterator(); iPath.hasNext(); ) {
            NetGraph.NetPath netPath = (NetGraph.NetPath) iPath.next();

            if (netPath.getType() != pathType)
                continue;
            if (netPath.isFeedBack())
                continue;
            // ignore pass gates
            if (!netPath.getEndNode().isRail()) {
                // Give warning for more than one transistor in pass gate
                if (netPath.getEdges().size() > 1)
                    design.messageCenter.createMessage(1, 20, "WARNING",
                        "Found pass gate with more than one transistor\n",
                        "Found " + netPath.getEdges().size() +
                        " transistors in pass gate " + netPath +
                        " in cell " + cellType.typeName + "\n");
                continue;
            }
            allPathsPass = false;

            // get folding factor for NetPath
            double M = netPath.getFoldingFactor(netPaths);
            if (symmetrizationFactor == 0) {
                symmetrizationFactor = M;
            } else if (symmetrizationFactor != M) {
                // TODO: turn this into an assertion if we only care
                // about processing netlists generated by jauto.
                design.messageCenter.createMessage(1, 19, "WARNING",
                    "Symmetrization factors do not agree for paths in a half-op.",
                    "cell type: " + cellType.typeName +
                    "\nnode name: " + netNode.name +
                    "\npath type: " + pathType +
                    "\nold symmetrization factor: " + symmetrizationFactor +
                    "\nnew symmetrization factor: " + M + "\n");
            }
        }

        if (allPathsPass) {
            assert symmetrizationFactor == 0;
            return 1;
        } else {
            assert symmetrizationFactor > 0;
            return symmetrizationFactor;
        }
    }


    /**
     * Function to put I/O direction properties on all port nets.
     **/
    public static void addPortDirection(Set/*<CellType>*/ cellTypes)
    {
        if(DebugOption.printAll){
            System.out.println("*************************************************************************");
            System.out.println("Assigning I/O directions to port nets");
            System.out.println("*************************************************************************");
        }

        // Create a sorted list for all celltypes, from low to high
        // (leaf to high level)
        ArrayList/*<CellType>*/ lsta = new ArrayList/*<CellType>*/();

        for (Iterator ita = cellTypes.iterator(); ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();
            sortedInsert(lsta, sta);
        }

        // Annotate all the port cellnets
        for (Iterator ita = lsta.iterator(); ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();
            Map<HierName,Integer> markPorts = new HashMap<HierName,Integer>();
            for (Map.Entry<String,Integer> portDir :
                    CellUtils.markPorts(sta.cast_cell).entrySet()) {
                try {
                    HierName hport =
                        HierName.makeHierName(portDir.getKey(), '.');
                    markPorts.put(
                        (HierName) sta.namespace.getCanonicalKey(hport),
                        portDir.getValue());
                } catch (InvalidHierNameException e) {
                    throw new RuntimeException(
                        "Cannot create HierName: " + portDir.getKey(), e);
                }
            }

            if(DebugOption.printAll){
                System.out.println(sta.toString());
            }

            for (Iterator itb = sta.getAllNets().iterator();
                 itb.hasNext(); ) {
                CellNet neta = (CellNet)itb.next();
                if(neta.isPortNet()){ // only process port nets
                    neta.visited = false; // reset the "visited" flag
                    HierName hna = neta.canonicalName;

                    if (!sta.transistors.isEmpty()) {
                        // there are transistors in the netgraph

                        for (Iterator itc = sta.transistors.getNodes().iterator(); itc.hasNext(); ) {
                            NetGraph.NetNode nna = (NetGraph.NetNode)itc.next();

                            if (hna.equals(nna.name)) { // the cellnet exists in netgraph
                                if(nna.isOutput()){ // should be at least output port
                                    
                                    // see if it also drives a gate
                                    boolean found = false;
                                    nodeLoop:
                                    for (Iterator itd = sta.transistors.getNodes().iterator(); itd.hasNext(); ) {
                                        NetGraph.NetNode nnb = (NetGraph.NetNode)itd.next();

                                        for (Iterator ite = nnb.edges.iterator(); ite.hasNext(); ) {
                                            NetGraph.NetEdge nea = (NetGraph.NetEdge)ite.next();
                                            if(nea.source.isStaticizerInverter() || nea.drain.isStaticizerInverter()){
                                                if(DebugOption.printAll){
                                                }
                                            }
                                            else{
                                                if(nea.gate == nna){
                                                    found = true;
                                                    break nodeLoop;
                                                }
                                            }
                                        }
                                    }
                                        

                                    if(found){ // this is a IN/OUT net
                                        if(DebugOption.printAll){
                                            System.out.println("1. This is a I/O port in the netgraph");
                                            System.out.println("Name of the netnode: " + nna.name);
                                        }
                                        neta.portDirection =
                                            CellNet.INPUTOUTPUT;
                                        neta.visited = true;
                                    }
                                    else{
                                        if(neta.visited){
                                            if (neta.portDirection == CellNet.INPUT) {
                                                // already marked as input
                                                neta.portDirection =
                                                    CellNet.INPUTOUTPUT;
                                                if(DebugOption.printAll){
                                                    System.out.println("2. This is a I/O port in the netgraph");
                                                    System.out.println("Name of the netnode: " + nna.name);
                                                }
                                            }
                                        }
                                        else{
                                            neta.portDirection = CellNet.OUTPUT;
                                            neta.visited = true;
                                        }
                                    }
                                }
                                else{
                                    if(neta.visited){
                                        if (neta.portDirection == CellNet.OUTPUT) {
                                            // already marked as output
                                            neta.portDirection =
                                                CellNet.INPUTOUTPUT;
                                            if(DebugOption.printAll){
                                                System.out.println("3. This is a I/O port in the netgraph");
                                                System.out.println("Name of the netnode: " + nna.name);
                                            }
                                        }
                                    }
                                    else{
                                        neta.portDirection = CellNet.INPUT;
                                        neta.visited = true;
                                    }
                                }
                            }
                        }

                        // FIXME: possible cell/transistor mix in the future, don't use getLevel
                        if((sta.getLevel() == 0) && (!neta.visited)){
                            String msa = "WARNING";
                            String msb = "Port net is not in the netgraph of the leaf cell.\n";
                            String msc = "CellName: " + sta.typeName + "\n"
                                + "NetName: " + neta.canonicalName.getCadenceString() + "\n";

                            sta.design.getMessageCenter().createMessage(1, 99, msa, msb, msc);
                        }
                    }

                    // check cells next
                    for (Iterator itc = neta.getSetSubcellNets().iterator(); itc.hasNext(); ) {
                        CellNet netb = (CellNet)itc.next();

                        if (netb.portDirection == CellNet.INPUT) {
                            if(neta.visited){
                                if (neta.portDirection == CellNet.OUTPUT) {
                                    // already marked as output
                                    neta.portDirection = CellNet.INPUTOUTPUT;
                                } else if (neta.portDirection == CellNet.UNKNOWN) {
                                    // already marked as unknown
                                    neta.portDirection = CellNet.INPUT;
                                }
                            }
                            else{
                                neta.portDirection = CellNet.INPUT;
                            }
                        } else if (netb.portDirection == CellNet.OUTPUT) {
                            if(neta.visited){
                                if (neta.portDirection == CellNet.INPUT) {
                                    // already marked as input
                                    neta.portDirection = CellNet.INPUTOUTPUT;
                                } else if (neta.portDirection == CellNet.UNKNOWN) {
                                    // already marked as unknown
                                    neta.portDirection = CellNet.OUTPUT;
                                }
                            }
                            else{
                                neta.portDirection = CellNet.OUTPUT;
                            }
                        } else if (netb.portDirection == CellNet.INPUTOUTPUT) {
                            neta.portDirection = CellNet.INPUTOUTPUT;
                        } else if (netb.portDirection == CellNet.UNKNOWN) {
                            String msa = "WARNING";
                            String msb = "Found port with UNKNOWN direction.\n";
                            String msc = "CellName: " + netb.container.typeName + "\n"
                                + "NetName: " + netb.canonicalName.getCadenceString() + "\n";

                            netb.container.design.getMessageCenter().createMessage(1, 98, msa, msb, msc);
                        }

                        neta.visited = true;
                    }

                    if (sta.isInternalEnv()) {
                        final Integer portDir =
                            markPorts.get(neta.canonicalName);
                        if (portDir != null) {
                            switch (portDir.intValue()) {
                              case PortDefinition.IN:
                                neta.portDirection = CellNet.INPUT;
                                break;
                              case PortDefinition.OUT:
                                neta.portDirection = CellNet.OUTPUT;
                                break;
                              case PortDefinition.INOUT:
                                neta.portDirection = CellNet.INPUTOUTPUT;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }


    /**
     * Generate the source/sink lists for all the cellnets in the design.
     * Used for GlobalNet generation.
     **/
    public void generateSourceSinkLists(Set/*<CellType>*/ cellTypes)
    {
        if(DebugOption.printSSG){
            System.out.println("*******************************************************");
            System.out.println("Generating the lists of sources and sinks for all cell nets");
            System.out.println("*******************************************************");
        }


        for (Iterator ita = cellTypes.iterator(); ita.hasNext(); ) {
            if(DebugOption.printSSG){
                System.out.println("******************************");
            }
            
            CellType sta = (CellType)ita.next();
            List/*<HalfOperator>*/ halfOperators =
                sta.getListHalfOperators();

            if(DebugOption.printSSG){
                System.out.println(sta.toString());
            }

            for (Iterator itb = sta.getAllNets().iterator(); itb.hasNext(); ) {
                if(DebugOption.printSSG){
                    System.out.println("--------------------------------------------------------------");
                }
                CellNet cna = (CellNet)itb.next();
                cna.listSources.clear();
                cna.listSinks.clear();
                HierName hna = cna.canonicalName;

                // look for user-defined load capacitance, add it as a sink
                double loadCap = cna.getLoadCapacitance();
                if(DebugOption.printLevel <= 1){
                    System.out.println("Obtained loadcap: " + loadCap + " for net: " + cna.canonicalName.getCadenceString());
                }

                if(loadCap > 0.0){
                    NetSink nska = new NetSink(NetType.CAPACITIVE_LOAD);
                    nska.loadCapacitance = loadCap;
                    cna.listSinks.add(nska);

                    if(DebugOption.printLevel <= 1){
                        System.out.println("Load capacitance sink " + loadCap + " added to net: " + cna.canonicalName.getCadenceString());
                    }
                }


                if(DebugOption.printSSG){
                    System.out.println(cna.toString());
                }

                // work through all the instances the net connects to 
                for (Iterator itc = cna.getListInstanceInfo().iterator(); itc.hasNext(); ) {
                    CellNet.InstanceInfo insta = (CellNet.InstanceInfo)itc.next();
                    if (insta.inst.child.isJautoIgnore())
                        continue;
                    Set/*<CellNet>*/ subcellNets = insta.setSubcellNets;

                    CellType childCellType = null;

                    // Check port directions for childCellType, to decide whether
                    // to add as a source, or a sink, or both
                    int direction = -1;
                    for (Iterator itd = subcellNets.iterator(); itd.hasNext(); ) {
                        CellNet cnc = (CellNet)itd.next();
                        // container should be the same for all cellnets
                        assert childCellType == null || childCellType == cnc.container;
                        childCellType = cnc.container;

                        if (direction == -1 || direction == CellNet.UNKNOWN)
                            direction = cnc.portDirection;
                        else if (direction == CellNet.INPUT) {
                            if (cnc.portDirection == CellNet.OUTPUT ||
                                cnc.portDirection == CellNet.INPUTOUTPUT)
                                direction = CellNet.INPUTOUTPUT;
                        } else if (direction == CellNet.OUTPUT) {
                            if (cnc.portDirection == CellNet.INPUT ||
                                cnc.portDirection == CellNet.INPUTOUTPUT)
                                direction = CellNet.INPUTOUTPUT;
                        }
                    }


                    // Add source/sink list
                    if (direction == CellNet.INPUT ||
                        direction == CellNet.INPUTOUTPUT) {
                        // INPUT, INPUTOUTPUT -> NetSink
                        NetSink nska = new NetSink(NetType.CELL);
                        nska.cellSink = childCellType;
                        nska.setSubcellNets = subcellNets;
                        nska.coordinateX = insta.coordinateX;
                        nska.coordinateY = insta.coordinateY;
                        nska.orientation = insta.orientation;
                        nska.prefixInstanceName(insta.inst.nameInParent);
                        cna.listSinks.add(nska);

                        if(DebugOption.printSSG){
                            System.out.println("Created a sink");
                            System.out.println(nska.toString());
                        }
                            
                    }

                    if (direction == CellNet.OUTPUT ||
                        direction == CellNet.INPUTOUTPUT) {
                        // OUTPUT, INPUTOUTPUT -> NetSource
                        NetSource nsra = new NetSource(NetType.CELL);
                        nsra.cellSource = childCellType;
                        nsra.setSubcellNets = subcellNets;
                        nsra.coordinateX = insta.coordinateX;
                        nsra.coordinateY = insta.coordinateY;
                        nsra.orientation = insta.orientation;
                        nsra.prefixInstanceName(insta.inst.nameInParent);
                        cna.listSources.add(nsra);

                        if(DebugOption.printSSG){
                            System.out.println("Created a source");
                            System.out.println(nsra.toString());
                        }
                            
                    }

                    // REVIEW: Why can't this be an assertion?
                    //   It does happen.  Why?
                    // UNKNOWN -> can't be!
                    if (direction == CellNet.UNKNOWN) {
                        System.out.println("NOTE: net with unknown direction");
                        //System.out.println(cna.toString());
                    }

                }


                // add default sinks to primary I/O ports
                // TODO: not adding sources, no method to deal with it yet, may need later
                if(cna.isPortNet() && (sta == getTopLevelCell())){ // primary input/output
                    /*
                    if (cna.portDirection == CellNet.INPUT ||
                        cna.portDirection == CellNet.INPUTOUTPUT) {
                        nsra = new NetSource(CellType.CAPACITIVE_LOAD);
                        cna.listSources.add(nsra);
                    }
                    */

                    if (cna.portDirection == CellNet.OUTPUT ||
                        cna.portDirection == CellNet.INPUTOUTPUT) {
                        NetSink nska = new NetSink(NetType.CAPACITIVE_LOAD);
                        cna.listSinks.add(nska);
                    }

                    assert cna.portDirection != CellNet.UNKNOWN
                        : "Port net with unknown direction at top-level: " +
                           cna;
                }


                // Generate half-operator and transistor type sources and
                // sinks.
                for (Iterator itc = sta.transistors.getNodes().iterator(); itc.hasNext(); ) {
                    NetGraph.NetNode nna = (NetGraph.NetNode)itc.next();
                    
                    if (hna.equals(nna.name)) { // the cellnet exists in the netgraph
                        if(nna.isOutput()){ // look for sources for output nodes
                            for (Iterator itd = halfOperators.iterator(); itd.hasNext(); ) {
                                HalfOperator hoa = (HalfOperator)itd.next();
                                assert hoa.outputNode != null
                                    : "Null pointer to netnode";
                                if(DebugOption.printSSG){
                                    System.out.println("-----------------------");
                                    System.out.println("\t\t" + hoa.outputNode.toString());
                                    System.out.println("-----------------------");
                                }
                                if(hoa.outputNode == nna){
                                    NetSource nsra = new NetSource(NetType.HALF_OPERATOR_TRANSISTOR);
                                    nsra.source = hoa;
                                    cna.listSources.add(nsra);

                                    if(DebugOption.printSSG){
                                        System.out.println("\t\tAdded one half-operator to the list of sources");
                                        System.out.println("\t\t" + nna.toString());
                                    }

                                }
                            }
                        }

                        // look for sinks for all nodes
                        Set/*<NetGraph.NetEdge>*/ seenEdges =
                            new HashSet/*<NetGraph.NetEdge>*/();
                        for (Iterator itd = sta.transistors.getNodes().iterator(); itd.hasNext(); ) {
                            NetGraph.NetNode nnb = (NetGraph.NetNode)itd.next();

                            for (Iterator ite = nnb.edges.iterator(); ite.hasNext(); ) {
                                NetGraph.NetEdge nea = (NetGraph.NetEdge)ite.next();

                                if(nea.gate == nna){ // found the netnode as the gate of a transistor
                                    if (!seenEdges.contains(nea)) {
                                        int count = 0;
                                        for (Iterator itf = halfOperators.iterator(); itf.hasNext(); ) {
                                            HalfOperator hoa = (HalfOperator)itf.next();
                                            if(hoa.transistors.contains(nea)){
                                                if(DebugOption.printSSG){
                                                    System.out.println("Added a sink to the sink list");
                                                    System.out.println("Name of the net: " + nna.name);
                                                }

                                                NetSink nska =
                                                    new NetSink(NetType.HALF_OPERATOR_TRANSISTOR);
                                                nska.sink = hoa;
                                                nska.transistor = nea;
                                                cna.listSinks.add(nska);

                                                count++;
                                            }
                                        }

                                        nea.shareCount = Math.max(1, count);

                                        seenEdges.add(nea);
                                    }
                                }
                            }
                        }
                    }
                }
                if(DebugOption.printSSG){
                    System.out.println("--------------------------------------------------------------");
                }

            }

            if(DebugOption.printSSG){
                System.out.println("******************************");
            }
        }
    }


    /**
     * Sorted insert of a celltype to list, by cell level number, from low to high.
     **/
    public static void sortedInsert(ArrayList/*<CellType>*/ lst1, CellType st1)
    {
        int k = st1.getLevel(); // get the level number of the sub-type
        int j = lst1.size();
        for(int i=0;i<j;++i){
            CellType sta = (CellType)lst1.get(i);
            int l = sta.getLevel();

            if(l == k){
                if(sta == st1){ // the sub-type is already in the list
                    return;
                }
            }

            if(l > k){
                lst1.add(i, st1); // insert this sub-type at index i
                return;
            }
        }

        lst1.add(st1); // add this sub-type to the end of the list
    }



    /**
     * Mark non-observable ports.
     **/
    public static void markNonObservablePorts(Set/*<CellType>*/ cellTypes)
    {
        for (Iterator ita = cellTypes.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();

            Boolean blb = (Boolean)DirectiveUtils.getTopLevelDirective(cella.cast_cell, DirectiveConstants.CELLNONOBSERVABLE);

            // "fragment" directive makes all port nets non-observable
            boolean isFragment = blb.booleanValue();

            cella.setIsFragment(isFragment);

            if(isFragment){
                
                if(DebugOption.printLevel <= 3){
                    System.out.println("Fragment cell found:");
                    System.out.println(cella.toString());
                }

                for (Iterator itb = cella.getAllNets().iterator(); itb.hasNext(); ) {
                    CellNet cna = (CellNet)itb.next();

                    if(cna.isPortNet()){ // only mark port net non-observable
                        cna.setIsObservable(false);
                    }
                }


            }
        }


        // "cutpath" directive makes any net observable
        for (Iterator ita = cellTypes.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();

            for (Iterator itb = cella.getAllNets().iterator(); itb.hasNext(); ) {
                CellNet cna = (CellNet)itb.next();

                if(cna.isCutPath()){ 
                    cna.setIsObservable(true);
                }
            }
        }
    }

    public boolean useSteinerTree() {
        return useSteinerTree;
    }
}
