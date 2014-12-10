/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.jauto;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;


import com.avlsi.fast.*;
import com.avlsi.tools.jauto.*;

import com.avlsi.file.common.HierName;

import com.avlsi.geometry.SteinnerTree;

import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.Visitor;
import com.avlsi.netlist.impl.SimpleAbstractDeviceIterator;

import com.avlsi.tools.lvs.NetGraph;

import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.util.debug.Debug;
/**
 * Class representing a net that crosses cell boundaries.
 *
 * Attached at all ports, and at the net at the top level.
 *
 * Contains pointers to all terminal ports.
 *
 * @author Aaron Denney
 * @version $Date$
 **/

public class DelayCalculator{

    private CastDesign                  design;

    private TransistorSizingTool        tool;

    private TechnologyData              tech;

    /** Constructor 1 */
    public DelayCalculator()
    {
        design = null;
        tool = null;
        tech = null;
    }


    /** Constructor 2 */
    public DelayCalculator(CastDesign d, TransistorSizingTool t)
    {
        design = d;
        tool = t;
        tech = d.getTechnologyData();
    }


    /** 
     * Timing analysis for given sizing path.  The calculated delays are the
     * sum of the delays for all half-operators in the path except for the last
     * plus the delay of global nets for the last half-operator.
     * 
     * <p> The delay budget is the sum of the delay biases for all half
     * operators times tau (the unit delay) plus the extra delay of a global
     * net attached to the last half-operator.
     *
     * <p>This function modifies the <code>sp1</code> argument.
     * <ul>
     *   <li><code>delay</code> is set to the calculated delay.
     *   <li><code>slack</code> is set to the delay budget minus the
     *       calculated delay.
     *   <li><code>criticalNet</code> is set to the last net in the path with
     *   the minimum slack
     * </ul>
     * 
     * @return the calculated maximum delay for the <code>SizingPath</code>
     **/
    public final double calculateDelay(SizingPath sp1)
    {
        CellNet criticalNet = null;
        double delay = 0.0;
        double totalDelayBias = 0.0;
        double tau = tool.getOptionUnitDelay();
        double[] allDelays = null;
        double[] allSlacks = null;
        double[] allDelaybias = null;

        List/*<HalfOperator>*/ halfOperators = sp1.getPath();
        final int numHalfOperators = halfOperators.size();
        for (int iHalfOp = 0; iHalfOp < numHalfOperators; ++iHalfOp) {
            HalfOperator hoa = (HalfOperator) halfOperators.get(iHalfOp);
            totalDelayBias += hoa.getDelayBias();

            if (iHalfOp < numHalfOperators - 1) { // not the last half-operator
                CellNet cna = hoa.outputNet;
                List/*<GlobalNet>*/ globalNets = cna.getGlobalNets();

                assert globalNets.size() == 1
                    : "Internal cell net has more than 1 global net";

                GlobalNet gna = (GlobalNet) globalNets.get(0);

                delay += gna.calculateDelay(hoa);

            }
            else{ // the last half-operator
                CellNet cna = sp1.getEndNet();

                // delay budget before instance based delaybias
                final double unbiasedBudget = totalDelayBias * tau;
                double maxDelay = Double.NEGATIVE_INFINITY;
                double minSlack = Double.POSITIVE_INFINITY;
                criticalNet = cna;
                allDelays = new double[cna.getGlobalNets().size()];
                allSlacks = new double[cna.getGlobalNets().size()];
                allDelaybias = new double[cna.getGlobalNets().size()];
                int count = 0;
                for (Iterator ita = cna.getGlobalNets().iterator();
                     ita.hasNext(); ++count) {
                    GlobalNet gna = (GlobalNet)ita.next();
                    allDelaybias[count] =
                        TransistorSizingTool.findDelayBias(gna, hoa);
                    double partialBudget = unbiasedBudget * allDelaybias[count];
                    cna = gna.getTopCellNet();

                    double g = gna.calculateDelay(hoa);
                    maxDelay = Math.max(maxDelay, g);
                    allDelays[count] = delay + g;

                    double e = getExtraDelay(hoa, gna, tau);
                    allSlacks[count] = (partialBudget + e) - allDelays[count];

                    if (allSlacks[count] < minSlack) {
                        minSlack = allSlacks[count];
                        criticalNet = cna;
                    }
                }

                delay += maxDelay;
            }
        }

        sp1.delay = allDelays;
        sp1.slack = allSlacks;
        sp1.delaybias = allDelaybias;
        sp1.criticalNet = criticalNet;

        return delay;
    }


    /** 
     * Timing analysis for given concatenated path.  The calculated delays are
     * the sum of the delays for all half-operators in the path except for the
     * last plus the delay of global nets for the last half-operator.
     * 
     * <p> The delay budget is the sum of the delay biases for all half
     * operators times tau (the unit delay) plus the extra delays of all
     * half-operators ending a non-final sizing paths plus the extra delay of
     * an global net attached to the last half-operator of the last sizing
     * path.
     *
     * <p>This function modifies the <code>cp1</code> argument.
     * <ul>
     *   <li><code>delay</code> is set to the calculated delay.
     *   <li><code>slack</code> is set to the delay budget minus the
     *       calculated delay.
     *   <li><code>criticalNet</code> is set to the last net in the path with
     *   the minimum slack
     * </ul>
     * 
     * @return the calculated maximum delay for the <code>CatPath</code>
     **/
    public final double calculateDelay(CatPath cp1)
    {
        CellNet criticalNet = null;
        double delay = 0.0;
        double totalDelayBias = 0.0;
        double extraDelay = 0.0;
        double tau = tool.getOptionUnitDelay();
        double[] allDelays = null;
        double[] allSlacks = null;
        double[] allDelaybias = null;

        List/*<SizingPath>*/ sizingPaths = cp1.getCatPath();
        int numSizingPaths = sizingPaths.size();
        for (int iSizingPath = 0; iSizingPath < numSizingPaths; ++iSizingPath) {
            SizingPath spa = (SizingPath) sizingPaths.get(iSizingPath);
            double pathDelayBias = 0.0;

            List/*<HalfOperator>*/ halfOperators = spa.getPath();
            final int numHalfOperators = halfOperators.size();
            for (int iHalfOp = 0; iHalfOp < numHalfOperators; ++iHalfOp) {
                HalfOperator hoa = (HalfOperator) halfOperators.get(iHalfOp);
                pathDelayBias += hoa.getDelayBias();
                if (iHalfOp < numHalfOperators - 1) { // not the last half-operator
                    CellNet cna = hoa.outputNet;
                    List/*<GlobalNet>*/ globalNets = cna.getGlobalNets();

                    assert globalNets.size() == 1
                        : "Internal cell net has more than 1 global net";

                    GlobalNet gna = (GlobalNet) globalNets.get(0);

                    delay += gna.calculateDelay(hoa);

                }
                else{ // the last half-operator
                    if (iSizingPath < numSizingPaths - 1) { // not the last sizing path
                        CellNet cna = spa.getEndNet();
                        List/*<GlobalNet>*/ globalNets = cna.getGlobalNets();

                        assert globalNets.size() == 1
                            : "Internal cell net has more than 1 global net";

                        GlobalNet gna = (GlobalNet) globalNets.get(0);

                        delay += gna.calculateDelay(hoa);

                        extraDelay += getExtraDelay(hoa, gna, tau);

                        totalDelayBias += pathDelayBias *
                            TransistorSizingTool.findDelayBias(gna, hoa);
                    }
                    else{ // the last sizing path
                        CellNet cna = cp1.getEndNet();

                        // delay budget before instance based delaybias
                        double maxDelay = Double.NEGATIVE_INFINITY;
                        double minSlack = Double.POSITIVE_INFINITY;
                        criticalNet = cna;
                        allDelays = new double[cna.getGlobalNets().size()];
                        allSlacks = new double[cna.getGlobalNets().size()];
                        allDelaybias = new double[cna.getGlobalNets().size()];
                        int count = 0;
                        for (Iterator ita = cna.getGlobalNets().iterator();
                             ita.hasNext(); ++count) {
                            GlobalNet gna = (GlobalNet)ita.next();
                            allDelaybias[count] =
                                TransistorSizingTool.findDelayBias(gna, hoa);
                            final double biasedBudget =
                                (totalDelayBias +
                                 pathDelayBias * allDelaybias[count]) * tau;
                            double partialBudget = biasedBudget + extraDelay;
                            cna = gna.getTopCellNet();

                            double g = gna.calculateDelay(hoa);
                            maxDelay = Math.max(maxDelay, g);
                            allDelays[count] = delay + g;

                            double d = getExtraDelay(hoa, gna, tau);
                            allSlacks[count] =
                                partialBudget + d - allDelays[count];

                            if (allSlacks[count] < minSlack) {
                                minSlack = allSlacks[count];
                                criticalNet = cna;
                            }
                        }

                        delay += maxDelay;
                    }
                }
            }
        }

        cp1.delay = allDelays;
        cp1.slack = allSlacks;
        cp1.delaybias = allDelaybias;
        cp1.criticalNet = criticalNet;


        return delay;
    }


    /** 
     * Computes the average delay of non-fragment paths in the cell type
     * <code>ct1</code>.
     **/
    public final double calculateDelay(CellType ct1)
    {
        int pathCount = 0;
        double totalDelay = 0.0;

        if(ct1.getLevel() == 0){

            for (Iterator ita = ct1.getSizingPaths().iterator();
                 ita.hasNext(); ) {
                SizingPath spa = (SizingPath)ita.next();

                if(!spa.isFragment()){
                    pathCount++;
                    totalDelay += calculateDelay(spa);
                }
            }
        }
        else{

            for (Iterator ita = ct1.getSizingPaths().iterator();
                 ita.hasNext(); ) {
                CatPath cpa = (CatPath)ita.next();

                if(!cpa.isFragment()){
                    calculateDelay(cpa);
                }
            }
            for (Iterator ita = ct1.getCatPaths().iterator();
                 ita.hasNext(); ) {
                CatPath cpa = (CatPath)ita.next();
                if(!cpa.isFragment()){

                    pathCount++;
                    totalDelay += calculateDelay(cpa);
                }
            }

            for (Iterator ita = ct1.getReducedCatPaths().iterator();
                 ita.hasNext(); ) {
                CatPath cpa = (CatPath)ita.next();
                if(!cpa.isFragment()){

                    calculateDelay(cpa);
                }
            }
        }


        return totalDelay / pathCount;
    }


    /** 
     * Computes the average delay of the cells (as computed by
     * <code>{@link #calculateDelay(CellType)}</code>) in the design
     * <code>cd1</code>.
     **/
    public final double calculateDelay(CastDesign cd1)
    {
        if(design != cd1){
            System.out.println("Warning: circuit design for timing analysis has been changed from the initial setting.");
            design = cd1;
            tech = design.getTechnologyData();
        }

        int cellCount = 0;
        double totalDelay = 0.0;
        for (Iterator ita = design.allCellTypes.iterator(); ita.hasNext(); ) {
            CellType cta = (CellType)ita.next();

            cellCount++;
            totalDelay += calculateDelay(cta);
        }

        return totalDelay / cellCount;
    }


    /** Timing analysis for default design */
    public final double calculateDelay()
    {
        return calculateDelay(design);
    }


    /** Query cast extra_delay directive **/
    static double getExtraDelay(HalfOperator ho1, GlobalNet gn1, double tau)
    {
        CellNet cna = gn1.getTopCellNet();
        double tauInSpec = cna.container.getTau();
        double tauToUse = tauInSpec < 0.0 ? tau : tauInSpec;
        double netExtraDelay;
        if (ho1.driveDirection == HalfOperator.DriveDirection.PULL_DOWN)
            netExtraDelay = cna.getDownExtraDelay();
        else // PULL-UP
            netExtraDelay = cna.getUpExtraDelay();
        return tauToUse * netExtraDelay / 100.0;
    }

}
