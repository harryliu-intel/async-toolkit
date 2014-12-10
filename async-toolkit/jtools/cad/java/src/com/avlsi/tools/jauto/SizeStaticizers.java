/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.*;
import java.util.*;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.fast.*;
import com.avlsi.tools.jauto.*;
import com.avlsi.util.debug.Debug;

import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;


public class SizeStaticizers {

    /**
     * Size feedback transistors for all the staticizers in the
     * tool's synthesized design to strength
     *
     * @param tool
     *        The tool whose synthesized design should have its staticizers
     *        sized and whose staticizer ratio should be used.
     **/
    public static void sizeStaticizers(TransistorSizingTool tool) {
        sizeStaticizers(tool.getSynthesizedDesign());
    }

    /**
     * Sizes a staticizer so its conductance G to satisfies
     * minG<=G<=maxG and leakG<=G.  If userG!=0, instead tries to
     * satisfy G==userG.  Reports violations of these constraints.
     *
     * @param tdata
     *        Technology data
     * @param node
     *        Output NetNode
     * @param npa
     *        Feedback NetPath to size
     * @param minG
     *        Minimum staticizer conductance
     * @param maxG
     *        Maximum staticizer conductance
     * @param leakG
     *        Minimum staticizer conductance for leakage
     * @param userG
     *        Desired staticizer conductance from staticizer_ratio directive
     **/
    private static void setStaticizerStrength(TechnologyData tdata,
                                              NetGraph.NetNode node,
                                              NetGraph.NetPath npa,
                                              double minG,  double maxG,
                                              double leakG, double userG,
                                              ArrayList report) {
        boolean nmos = npa.getType() == DeviceTypes.N_TYPE;
        int ttype = npa.getTransistorType();
        int maxDepth = nmos ? tdata.staticizerMaxDepthN : tdata.staticizerMaxDepthP;
        double incW = tdata.staticizerWidthGrid;
        double minW = tdata.minimumTransistorWidth;
        double L = tdata.minimumTransistorLength;
        double lG = Math.max(minG,leakG);
        double uG = maxG;
        if (userG!=0) lG = uG = userG;
        double G=0;
        if (!npa.isWeakFeedBack()) {
            // size combinational staticizer by increasing width and checking G
            // report.add("NOTE: sizing combinational staticizer on " + node.name);
            int depth = npa.getDepth();
            double erf = nmos ?
                tdata.getEffectiveResistanceFactorN(ttype,depth-1) :
                tdata.getEffectiveResistanceFactorP(ttype,depth-1);
            // for vanBerkel gates, don't make feedback wider than logic
            double maxW = npa.getMinWidth(true);
            if (maxW==0) maxW=100*minW; // TODO: something less arbitrary
            for (double W=minW; W<=maxW; W+=incW) {
                npa.sizeFeedback(W);
                G = 1 / npa.getSquares() / erf;
                if (G> uG) break; // too strong already, quit now
                if (G>=lG) break; // found a good width, finish
            }
        } else {
            // search depth and width for the best weak staticizer
            // G<=uG takes precedence over G>=lG
            int depth;
            double W=0;
            for (depth=1; depth<=maxDepth; depth++) {
                double erf = nmos ?
                    tdata.getEffectiveResistanceFactorN(ttype,depth-1) :
                    tdata.getEffectiveResistanceFactorP(ttype,depth-1);
                // pick narrowest staticizer allowed
                W = lG * L * depth * erf;
                W = Math.max(minW,incW*Math.ceil(W/incW));
                G = W / L / depth / erf;
                if (G>uG) {
                    // pick widest staticizer allowed
                    W = uG * L * depth * erf;
                    W = Math.max(minW,incW*Math.floor(W/incW));
                    G = W / L / depth / erf;
                }
                if (G<=uG) break; // staticizer with this depth works
            }
            if (depth>maxDepth) depth=maxDepth;

            // size feedback path
            npa.sizeWeakFeedBack(W,tdata.minimumTransistorLength,depth);
        }

        // report staticizer sizing violations
        String s = (nmos ? "NMOS" : "PMOS") + " staticizer on " +
            node.name + (nmos ? "+" : "-");
        if (G>maxG*1.01)
            report.add("ERROR: " + s + " violates maxStaticizerRatio by " +
                       NumberFormatter.format(G/maxG,3) + " times");
        if (G<minG*0.99)
            report.add("ERROR: " + s + " violates minStaticizerRatio by " +
                       NumberFormatter.format(minG/G,3) + " times");
        if (G<leakG*0.99)
            report.add("ERROR: " + s + " violates staticizerLeakageRatio by " +
                       NumberFormatter.format(leakG/G,3) + " times");
        if (userG!=0 && (G>userG*1.1 || G<userG))
            report.add("ERROR: " + s + " violates staticizer_ratio directive by " +
                       NumberFormatter.format(G/userG,3) + " times");
    }

    /** Find the transistor type for the staticizer of half-operator **/
    private static int getStaticizerTransistorType(HalfOperator hoa) {
        int type = 0;
        boolean up = hoa.driveDirection == HalfOperator.DriveDirection.PULL_UP;
        List globalNets = hoa.outputNet.getGlobalNets();
        for (Iterator ita = globalNets.iterator(); ita.hasNext(); ) {
            GlobalNet gna = (GlobalNet) ita.next();
            for (Iterator itb = gna.getListSources().iterator(); itb.hasNext(); ) {
                NetSource nsra = (NetSource) itb.next();
                if (nsra.getType() == NetType.HALF_OPERATOR_TRANSISTOR) {
                    HalfOperator hob = nsra.source;
                    NetGraph.NetNode node = hob.outputNode;
                    for (Iterator itc = node.getFeedbackPaths().iterator(); itc.hasNext(); ) {
                        NetGraph.NetPath path = (NetGraph.NetPath) itc.next();
                        boolean n = path.getType() == DeviceTypes.N_TYPE;
                        if (n == up) {
                            if (type==0) type = path.getTransistorType();
                            else if (type!=path.getTransistorType()) return 0;
                        }
                    }
                }
            }
        }
        return type;
    }

    /** Report the minimium size for logic to overpower the weakest staticizer **/
    public static double getMinimumLogicSize(TechnologyData tdata, HalfOperator ho) {
        double W = tdata.minimumTransistorWidth;
        double L = tdata.minimumTransistorLength;

        // get ho properties
        int dir = ho.driveDirection;
        boolean nmos = (dir == HalfOperator.DriveDirection.PULL_DOWN);
        int ttype = ho.getTransistorType();
        int stat_tt = getStaticizerTransistorType(ho);
        if (stat_tt==0) {
            System.err.println("WARNING: unknown transistor type for " 
                               + (nmos ? "PMOS" : "NMOS") + " staticizer of " +
                               ho.subType.typeName + "/" +
                               ho.outputNet.canonicalName.toString() +
                               (nmos ? "-" : "+"));
            return 0; // don't up-size logic
        }

        // get conductance of weakest opposing staticizer
        int depth = nmos ? tdata.staticizerMaxDepthP : tdata.staticizerMaxDepthN;
        double stat_erf = nmos ?
            tdata.getEffectiveResistanceFactorP(stat_tt,depth-1) :
            tdata.getEffectiveResistanceFactorN(stat_tt,depth-1);
        double statG = W / L / depth / stat_erf;

        // solve size that satisfies statG<(size/L/erf)*maxStaticizerRatio
        double erf = nmos ?
            tdata.getEffectiveResistanceFactorN(ttype,0) :
            tdata.getEffectiveResistanceFactorP(ttype,0);
        return statG * L * erf / tdata.maxStaticizerRatio;
    }

    /** Getx explicit staticizer_ratio directive **/
    private static double getStaticizerRatio(final Map ratios,
                                             final HierName output,
                                             final Float cellRatio) {
        final Float val = (Float) ratios.get(output);
        if (val!=null) return val.doubleValue(); // half-operator directive
        if (cellRatio!=null) return cellRatio.doubleValue(); // cell directive
        return 0; // no directive
    }

    /**
     * Size feedback transistors for all the staticizers in
     * <code>design</code>
     *
     * @param design
     *        The design whose staticizers are to be sized.
     **/
    public static void sizeStaticizers(final CastDesign design) {
        // get relevant technology data
        TechnologyData tdata = design.getTechnologyData();
        double gateL = tdata.getDefaultGateLength();
        double minStaticizerRatio = tdata.minStaticizerRatio;
        double maxStaticizerRatio = tdata.maxStaticizerRatio;
        double minCombinationalStaticizerRatio = tdata.minCombinationalStaticizerRatio;
        double maxCombinationalStaticizerRatio = tdata.maxCombinationalStaticizerRatio;
        double staticizerLeakageRatio = tdata.staticizerLeakageRatio;

        // start processing all cells
        for (Iterator ita = design.allCellTypes.iterator(); ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();
            ArrayList report = new ArrayList();
            if (! JautoUI.quiet)
                System.out.println("Sizing staticizers in cell " + sta.typeName + "...");

            // query staticizer_ratio directives
            final Map ratios = DirectiveUtils.getPrsDirective(sta.cast_cell,
                    DirectiveConstants.STATICIZER_RATIO,
                    DirectiveConstants.HALFOP_TYPE);
            final Map ratioUp = DirectiveUtils.canonizeKey(sta.namespace,
                    DirectiveUtils.getUps(ratios));
            final Map ratioDn = DirectiveUtils.canonizeKey(sta.namespace,
                    DirectiveUtils.getDowns(ratios));
            final Float cellRatio =
                (Float) DirectiveUtils.getTopLevelDirective(sta.cast_cell,
                        DirectiveConstants.STATICIZER_RATIO);

            // process all local CellNet's of this cell
            for (Iterator itb = sta.getAllNets().iterator(); itb.hasNext(); ) {

                // get CellNet, Name, NetNode
                CellNet outputNet = (CellNet) itb.next();
                String Name = outputNet.canonicalName.toString();
                NetGraph.NetNode outputNode =
                    sta.transistors.findNetNode(outputNet.canonicalName);
                if (outputNode==null) continue; // no transistors

                // get half-operator staticizer_ratio directives
                double upRatio = getStaticizerRatio(ratioUp, outputNet.canonicalName, cellRatio);
                double dnRatio = getStaticizerRatio(ratioDn, outputNet.canonicalName, cellRatio);

                // prepare to accumulate leakage and active strengths and transistor_types
                double minPullDnSize  = Double.POSITIVE_INFINITY;
                double minPullUpSize  = Double.POSITIVE_INFINITY;
                double maxPullUpWidth = 0;
                double maxPullDnWidth = 0;
                int tt_up = 0;
                int tt_dn = 0;

                // get all global nets for this CellNet
                List/*<GlobalNet>*/ globalNets = outputNet.getGlobalNets();
                assert globalNets.size() != 0
                    : "CellNet has no corresponding global net.\n" +
                    "CellName: " + sta.typeName + "\n" +
                      "NetName: " +
                      outputNet.canonicalName.getCadenceString();

                // search through GlobalNet's to find leakage and active sizes and tt
                for (Iterator itc = globalNets.iterator(); itc.hasNext(); ) {
                    GlobalNet gna = (GlobalNet)itc.next();

                    // find largest N and P diffusion perimeter for leakage strength ratio
                    double upWidth = gna.getTotalDriverWidth(DeviceTypes.P_TYPE);
                    double dnWidth = gna.getTotalDriverWidth(DeviceTypes.N_TYPE);
                    if (upWidth > maxPullUpWidth) maxPullUpWidth = upWidth;
                    if (dnWidth > maxPullDnWidth) maxPullDnWidth = dnWidth;

                    // find weakest logic half-operator for active strength ratio, plus tt
                    for (Iterator itd = gna.getListSources().iterator(); itd.hasNext(); ) {
                        NetSource nsra = (NetSource)itd.next();
                        if(nsra.getType() == NetType.HALF_OPERATOR_TRANSISTOR){
                            HalfOperator hob = nsra.source;
                            double hobSize = hob.getCurrentSize();
                            int tt = hob.getTransistorType();
                            if (hob.driveDirection == HalfOperator.DriveDirection.PULL_DOWN) {
                                if (hobSize < minPullDnSize) minPullDnSize = hobSize;
                                if (tt_dn==0) tt_dn = tt;
                                else if (tt_dn!=tt) tt_dn=-1; // detect mixed transistor_type
                            }
                            else {
                                if (hobSize < minPullUpSize) minPullUpSize = hobSize;
                                if (tt_up==0) tt_up = tt;
                                else if (tt_up!=tt) tt_up=-1; // dected mixed transistor_type
                            }
                        }
                    }
                }

                // size feedback paths of this outputNode based on active and leakage conductance
                outputNode.findPaths();
                for (Iterator itc = outputNode.getFeedbackPaths().iterator(); itc.hasNext(); ) {
                    NetGraph.NetPath npa = (NetGraph.NetPath)itc.next();
                    boolean nmos = (npa.getType()==DeviceTypes.N_TYPE);
                    boolean weak = npa.isWeakFeedBack();
                    double minSR = weak ? minStaticizerRatio : minCombinationalStaticizerRatio;
                    double maxSR = weak ? maxStaticizerRatio : maxCombinationalStaticizerRatio;
                    if (nmos && tt_up>0) { // size staticizer pull-down
                        double erfP   = tdata.getEffectiveResistanceFactorP(tt_up, 0);
                        double leakP  = tdata.getLeakageResistanceP(tt_up);
                        double logicG = minPullUpSize  / gateL / erfP;
                        double leakG  = maxPullUpWidth / gateL / leakP * staticizerLeakageRatio;
                        double minG   = logicG * minSR;
                        double maxG   = logicG * maxSR;
                        double userG  = logicG * upRatio;
                        setStaticizerStrength(tdata,outputNode,npa,minG,maxG,leakG,userG,report);
                    } else if (!nmos && tt_dn>0) { // size staticizer pull-up
                        double erfN   = tdata.getEffectiveResistanceFactorN(tt_dn, 0);
                        double leakN  = tdata.getLeakageResistanceN(tt_dn);
                        double logicG = minPullDnSize  / gateL / erfN;
                        double leakG  = maxPullDnWidth / gateL / leakN * staticizerLeakageRatio;
                        double minG   = logicG * minSR;
                        double maxG   = logicG * maxSR;
                        double userG  = logicG * dnRatio;
                        setStaticizerStrength(tdata,outputNode,npa,minG,maxG,leakG,userG,report);
                    } else {
                        report.add("WARNING: " + Name + (nmos ? "+" : "-") + 
                                   " has " + (nmos ? "NMOS" : "PMOS") +
                                   " feedback but opposing logic is missing or mixed threshold");
                    }
                }
            }

            // debug netgraph
            // System.err.println("NetGraph of cell " + sta.typeName);
            // System.err.println(sta.transistors);

            // report anything interesting for this cell type
            if (report.size()!=0) {
                System.err.println("Sizing staticizers in cell " + sta.typeName);
                for (Iterator itb=report.iterator(); itb.hasNext(); ) {
                    String s = (String) itb.next();
                    System.err.println(s);
                }
                System.err.println();
            }
        }

        // TODO: deprecate everything about precharge transistors
        if (hasPrechargeTransistors(design)) {
            System.err.println("ERROR: precharge transistors not supported\n");
            System.exit(1);
        }
    }

    /** Check to see if precharge transistors exist in the design **/
    private static boolean hasPrechargeTransistors(final CastDesign design) {
        for (Iterator i = design.allCellTypes.iterator(); i.hasNext(); ) {
            CellType cell = (CellType) i.next();
            for (Iterator j = cell.transistors.getNodes().iterator(); j.hasNext(); ) {
                final NetGraph.NetNode node = (NetGraph.NetNode) j.next();
                final NetGraph.GateInstance gate = node.getGate();
                if (gate != null && gate.isPrechargePrimitive()) {
                    return true;
                }
            }
        }
        return false;
    }

    /** Convert a length in meters to microns. **/
    private static String toMicron(final double length, final int precision) {
        return NumberFormatter.format(length * 1e6, precision) + "um";
    }

    /** TODO: remove **/
    private static void sizePrechargeTransistor(final CastDesign design,
                                                final CellType cell,
                                                final NetGraph.NetNode node) {
        final TechnologyData tdata = design.getTechnologyData();

        // find the pre-(dis)charge transistor
        NetGraph.NetEdge precharge = null;
        for (Iterator i = node.getEdges(); i.hasNext() && precharge == null; ) {
            final NetGraph.NetEdge edge = (NetGraph.NetEdge) i.next();
            if (edge.precharge) precharge = edge;
        }

        assert precharge.type == DeviceTypes.N_TYPE ||
               precharge.type == DeviceTypes.P_TYPE;
        assert precharge != null : "No pre-(dis)charge transistor found on " +
                                   "node: " + node.name + " in " +
                                   cell.typeName;

        // determine if this is a pre-charge or a pre-discharge transistor; it
        // is impossible to have both a pre-charge and a pre-discharge
        // transistor on the same node
        final boolean isPrecharge =
            (precharge.type == DeviceTypes.N_TYPE &&
                (precharge.source.isVdd() || precharge.drain.isVdd())) ||
            (precharge.type == DeviceTypes.P_TYPE &&
                (precharge.source.isGND() || precharge.drain.isGND()));

        // size the pre-discharge transistor as if it were a staticizer by
        // copying the length and width from the appropriate transistor in the
        // staticizer, which we assume has already been sized properly
        if (!isPrecharge) {
            // find the output node
            final Collection paths = node.findPaths(false);
            assert paths.size() > 0;
            NetGraph.NetNode output = null;
            for (Iterator i = paths.iterator(); i.hasNext() && output == null; )
            {
                final NetGraph.NetPath path = (NetGraph.NetPath) i.next();
                final NetGraph.NetNode s = path.getStartNode();
                final NetGraph.NetNode e = path.getEndNode();
                if (s != node && !s.isRail()) {
                    output = s;
                } else if (e != node && !e.isRail()) {
                    output = e;
                }
            }
            assert output != null;

            // find the appropriate staticizer transistor
            boolean notSized = true;
            output.findPaths();
            for (Iterator i = output.getPaths().iterator();
                 i.hasNext() && notSized; ) {
                final NetGraph.NetPath path = (NetGraph.NetPath) i.next();
                if (path.isFeedBack() && path.getType() == precharge.type) {
                    final NetGraph.NetEdge edge =
                        (NetGraph.NetEdge) path.getEdges().get(0);
                    precharge.length = edge.length;
                    precharge.width = edge.width;
                    notSized = false;
                }
            }
            if (notSized) {
                System.err.println("ERROR: Unable to size pre-discharge " +
                                   "transistor on node: " + node.name + " in " +
                                   cell.typeName);
            }

            // skip the rest of the processing
            return;
        }

        final Collection paths = node.findPaths(true);
        final double symFactor =
            CastDesign.getSymmetrizationFactor(node, paths, precharge.type,
                                               cell, design);
        final Map sizes = new HashMap();

        for (Iterator i = paths.iterator(); i.hasNext(); ) {
            final NetGraph.NetPath path = (NetGraph.NetPath) i.next();
            final Collection edges = path.getEdges();
            final double[] erf = precharge.type == DeviceTypes.N_TYPE ?
                tdata.getEffectiveResistanceFactorN(path.getTransistorType()) :
                tdata.getEffectiveResistanceFactorP(path.getTransistorType());
            final double effectiveWidth = edges.size() *
                erf[Math.min(edges.size(), erf.length) - 1] / erf[0];
            for (Iterator j = edges.iterator(); j.hasNext(); ) {
                final NetGraph.NetEdge edge = (NetGraph.NetEdge) j.next();
                final double previous = sizes.containsKey(edge) ?
                    ((Double) sizes.get(edge)).doubleValue() :
                    Double.NEGATIVE_INFINITY;
                sizes.put(edge, new Double(Math.max(previous, effectiveWidth)));
            }
        }

        double width = 0;
        for (Iterator i = sizes.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final NetGraph.NetEdge edge = (NetGraph.NetEdge) entry.getKey();
            width += edge.width / ((Double) entry.getValue()).doubleValue();
        }
        // the desired width to length ratio
        final double ratio = width / sizes.size() / symFactor
                                   / tdata.getDefaultGateLength()
                                   * tdata.getPrechargeRatio();

        final double desiredWidth = tdata.minimumTransistorLength * ratio;
        if (desiredWidth >= tdata.minimumTransistorWidth) {
            precharge.length = tdata.minimumTransistorLength;
            precharge.width = desiredWidth;
        } else {
            precharge.width = tdata.minimumTransistorWidth;
            precharge.length = tdata.minimumTransistorWidth / ratio;
            if (precharge.length > tdata.maximumPrechargeTransistorLength) {
                System.out.println(
                    "WARNING: Precharge transistor on " + cell.typeName + "/" +
                    node.name + " wants width to length ratio of " +
                    NumberFormatter.format(ratio, 3) + " but length of " +
                    toMicron(precharge.length, 3) + " exceeds maximum of " +
                    toMicron(tdata.maximumPrechargeTransistorLength, 3));
            }
        }
    }

    /** TODO: remove **/
    static void sizePrechargeTransistors(final CastDesign design) {
        for (Iterator i = design.allCellTypes.iterator(); i.hasNext(); ) {
            CellType cell = (CellType) i.next();
            for (Iterator j = cell.transistors.getNodes().iterator();
                 j.hasNext(); ) {
                final NetGraph.NetNode node = (NetGraph.NetNode) j.next();
                final NetGraph.GateInstance gate = node.getGate();
                if (gate != null && gate.isPrechargePrimitive()) {
                    sizePrechargeTransistor(design, cell, node);
                }
            }
        }
    }
}
