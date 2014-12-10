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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Collections;
import java.util.List;
import java.util.LinkedHashSet;
import java.util.HashSet;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Iterator;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.prs.ProductionRule;

import com.avlsi.tools.jauto.*;
import com.avlsi.util.container.ObjectUtils;
import com.avlsi.util.debug.Debug;


/**
 * Class representing half-operators.
 *
 * Just references a gate network + an output.
 *
 * @author Aaron Denney
 * @author Qing Wu
 * @version $Date$
 **/

public class HalfOperator {
    public static final class DriveDirection {
        private DriveDirection() { throw new AssertionError(); }
        public static final int PULL_DOWN = 0;
        public static final int PULL_UP   = 1;
        public static final int PASS_GATE = 2;
    }

    private double              delayBias = 1.0;
    private double              strengthBias = 1.0;
    public double               minDelay = -1.0;

    /**
     * Cell containing the half-operator.
     **/
    public CellType             subType;


    public NetGraph.NetNode     outputNode;
    public CellNet              outputNet;
    public Set<NetGraph.NetEdge> transistors;

    // Set of depths of the transistor stack
    public SortedSet/*<Integer>*/ depths;
    //@invariant depth.containsAll(nofloating);
    private SortedSet/*<Integer>*/ nofloating;

    // drive direction: DriveDirection.PULL_DOWN or DriveDirection.PULL_UP
    public int                   driveDirection;

    // current effective width of the halfoperator, in Meter
    private double               currentSize;

    // previous effective width of the halfoperator, in Meter
    private double               previousSize;

    /**
     * The length per meter "effective" size of diffusion of transistors
     * in this half-operator attached to the outputNode.  Must be multiplied
     * by <code>currentSize</code> to get a length in meters.
     **/
    private double outputDiffusionLength;

    // isFixed == true -> use value (currentSize), otherwise use variable (variableName)
    private boolean              isFixed;

    // name of the variable associated with this halfoperator
    // must be unique in the scope of the whole cast design
    private String               variableName;

    /**
     * Factor by which this half operator is symmetrized.
     **/
    private double symmetrizationFactor;

    // is this part of a combinational or dynamic gate?
    public boolean               isCombinational;
    
    // did this match part of a gate from the library?
    public boolean               isLibraryGate;
    
    // Set of NetNodes that have been precharged
    private Set prechargeNodes = null;

    private int transistorType = TechnologyData.NO_TRANSISTOR_TYPE;

    private static Comparator comparator = null;

    private static double EPSILON = 1e-12;

    public HalfOperator(){
        // FIXME: removed hardcoded constant
        currentSize = 0.3E-6;
        previousSize = 0.0;
        outputDiffusionLength = Double.NaN;
        isFixed = true;
        variableName = "";
        isCombinational = true;
        isLibraryGate = false;

        outputNode = null;
        outputNet = null;
        transistors = new LinkedHashSet<NetGraph.NetEdge>();
        depths = new TreeSet/*<Integer>*/();
        nofloating = new TreeSet/*<Integer>*/();

        driveDirection = DriveDirection.PULL_DOWN;
    }


    /**
     * Lock size.
     **/
    public boolean fixVariable() {
        isFixed = true;

        return isFixed;
    }

    /**
     * Unlock size, unless this half-operator is from a fixed size cell.
     **/
    public boolean unfixVariable() {
        if(!subType.isFixedSize()){
            isFixed = false;
        }

        return isFixed;
    }


    /**
     * Returns whether the size variable related to the half-operator is fixed or not
     **/
    public boolean isVariableFixed() {
        return isFixed;
    }

    /**
     * Set the symmetrization factor for this half-operator.
     **/
    public void setSymmetrizationFactor(final double symmetrizationFactor) {
        assert symmetrizationFactor > 0;
        this.symmetrizationFactor = symmetrizationFactor;
    }

    /**
     * Returns the symmetrization factor for this half-operator.
     **/
    public double getSymmetrizationFactor() {
        return symmetrizationFactor;
    }


    /**
     * Initialize size for fixed-size cells
     **/
    public final double initializeSize(){
        boolean consistent = true;
        boolean first = true;

        currentSize = Double.POSITIVE_INFINITY;

        // find the minimum half-operator size implied by the transistors
        for (NetGraph.NetEdge nea : transistors) {
            final double hsize = nea.width / nea.size;
            if (first) {
                currentSize = hsize;
                first = false;
            } else {
                if (Math.abs(hsize - currentSize) > EPSILON) {
                    currentSize = Math.min(hsize, currentSize);
                    consistent = false;
                }
            }
        }

        if (!consistent && ! JautoUI.quiet) {
            System.out.println("Warning: Size inconsistency found in " +
                               "fixed-sized cell, using most conservative " +
                               "half-operator size: " + currentSize);
            System.out.println(toString());
        }

        previousSize = currentSize;

        return currentSize;
    }

    /**
     * Initialize the width of the transistors in this half-operator to
     * -Infinity.
     **/
    public void resetWidth() {
        if (!subType.isFixedSize()) {
            for (NetGraph.NetEdge nea : transistors) {
                nea.width = Double.NEGATIVE_INFINITY;
            }
        }
    }

    /**
     * Update the current and previous values of the half-operator, and the
     * <code>width</code> and <code>length</code> values for transistors.
     * Sets <code>previousSize</code> to the <code>currentSize</code> and
     * the <code>currentSize</code> to the <code>newSize</code> parameter.
     **/
    public double updateSize(double newSize) {
        if (!subType.isFixedSize()) {
            previousSize = currentSize;
            currentSize = newSize;

            for (NetGraph.NetEdge nea : transistors) {
                nea.width = Math.max(nea.width, currentSize * nea.size);
            }
        }

        return currentSize;
    }

    /**
     * Returns the current "effective" size of the half-operator
     **/
    public double getCurrentSize() {
        return currentSize;
    }

    /**
     * Returns the previous "effective" size of the half-operator
     **/
    public double getPreviousSize() {
        return previousSize;
    }

    /**
     * Returns the length per meter "effective" size of diffusion of
     * transistors in this half-operator attached to the outputNode.  
     **/
    public double getOutputDiffusionLength() {
        assert !Double.isNaN(outputDiffusionLength);
        return outputDiffusionLength;
    }

    /**
     * Sets the length per meter "effective" size of diffusion of
     * transistors in this half-operator attached to the outputNode.  
     **/
    void setOutputDiffusionLength(final double outputDiffusionLength) {
        assert !Double.isNaN(outputDiffusionLength);
        this.outputDiffusionLength = outputDiffusionLength;
    }

    /**
     * Returns the name of the variable associated with this half-operator
     **/
    public String getVariableName() {
        return variableName;
    }

    /**
     * Set the name of the variable associated with this half-operator
     **/
    public String setVariableName(String s){
        assert s.length() <= 10
            : "variable name must contain no more than 10 characters";

        variableName = s;

        return variableName;
    }


    /**
     * Returns the list of pointers to all the fanout global nets of this half-operator
     **/
    public List getListGlobalNets() {
        return outputNet.getGlobalNets();
    }

    
    /**
     * Returns the setting of delay bias for this halfoperator
     **/
    public double getDelayBias() {
        return delayBias;
    }


    public double setDelayBias(double f)
    {
        assert f >= 0.0 : "f = " + f;

        delayBias = f;

        return delayBias;
    }


    public double getStrengthBias()
    {
        return strengthBias;
    }


    public double setStrengthBias(double f)
    {
        assert f > 0.0;

        strengthBias = f;

        return strengthBias;
    }


    public String toString()
    {
        return "Name of output node: " + outputNode.name + "\n" +
            "Name of output net: " + outputNet.canonicalName.getCadenceString() + "\n" +
            "Number of transistors: " + transistors.size() + "\n" +
            "Drive direction: " + driveDirection + "\n";
    }


    public boolean isSharingWith(HalfOperator ho1)
    {
        HashSet<NetGraph.NetEdge> seta = new HashSet<NetGraph.NetEdge>();

        seta.addAll(transistors);
        seta.retainAll(ho1.transistors);

        return !seta.isEmpty();
    }

    public boolean isSameStrengthGroup(HalfOperator ho1)
    {
        return subType.hasSameStrengthGroup(outputNet, driveDirection,
                                            ho1.outputNet, ho1.driveDirection);
    }

    /* Add a depth to the list of depths.  Set floating to true if this depth
     * is not to be emitted. */
    public void addDepths(int depth, boolean floating) {
        final Integer i = new Integer(depth);
        depths.add(i);
        if (!floating) nofloating.add(i);
    }

    /* Return a sorted array of stack depths to be emitted */
    public int[] getDepths() {
        final int[] result = new int[nofloating.size()];
        int j = 0;
        for (Iterator i = nofloating.iterator(); i.hasNext(); ++j) {
            Integer val = (Integer) i.next();
            result[j] = val.intValue();
        }
        return result;
    }

    public int getMaxDepth() {
        Debug.assertTrue(!depths.isEmpty(), "Depths set empty!  Cannot get maximum depth!");
        return ((Integer) depths.last()).intValue();
    }

    public int getMinDepth() {
        Debug.assertTrue(!depths.isEmpty(), "Depths set empty!  Cannot get minimum depth!");
        return ((Integer) depths.first()).intValue();
    }


    private double getEffectiveResistanceFactor() {
        final TechnologyData tech = subType.design.getTechnologyData();

        return driveDirection == DriveDirection.PULL_DOWN ?
            tech.getEffectiveResistanceFactorN(getTransistorType(), 0) :
            tech.getEffectiveResistanceFactorP(getTransistorType(), 0);
    }

    public double getStrength() {
        final TechnologyData tech = subType.design.getTechnologyData();
        return getEffectiveResistanceFactor() * tech.defaultGateLength /
               getCurrentSize();
    }


    // REVIEW: This function seems dodgy.
    public double getWidth(int depth) {
        final TechnologyData tech = subType.design.getTechnologyData();

        if (tech.getUseIntrinsicCap()) {
            final NetGraph.GateInstance gate = outputNode.getGate();

            // Did we match a gate?
            if (gate != null) {
                final HierName portName =
                    gate.getPortName(outputNode.getName());

                final CastDesign castDesign = outputNet.container.design;
                final CellInterface gateCell =
                    castDesign.getCellForGate(gate);
                final Float effectiveResistance = (Float)
                    DirectiveUtils.getHalfOpDirectiveValue(gateCell,
                            DirectiveConstants.EFFECTIVE_RESISTANCE,
                            portName,
                            driveDirection == DriveDirection.PULL_UP,
                            castDesign.getCadencize());

                // Was the effective resistance specified for the gate?
                if (effectiveResistance != null) {
                    final double[] effectiveResistances =
                        driveDirection == DriveDirection.PULL_DOWN ?
                                tech.getEffectiveResistanceFactorN(getTransistorType()) :
                                tech.getEffectiveResistanceFactorP(getTransistorType());
                    return getCurrentSize() * depth
                        * (effectiveResistance.floatValue() /
                           effectiveResistances[0])
                        * (effectiveResistances[depth - 1] /
                           effectiveResistances[getMaxDepth() - 1]);
                }
            }
        }

        JautoMessageCenter messageCenter = subType.design.messageCenter;

        // TODO: move stack limit checks elsewhere

        double resistanceFactorRatio;
        if (driveDirection == DriveDirection.PULL_DOWN) {
            if(depth > tech.stackWarnLimitN){
                String msa = "WARNING";
                String msb = "N-type transistor stack depth out of bound. The maximum allowable is " 
                      + tech.stackWarnLimitN + ".\n";
                String msc = "CellName: " + subType.typeName + "\n"
                      + "NetName: " + outputNet.canonicalName.getCadenceString() + "\n";
                if (! JautoUI.quiet)
                    messageCenter.createMessage(1, 15, msa, msb, msc);
            }
            else{
                // Max depth OK if matched to a gate or straight-chain.
                if(depth == tech.stackWarnLimitN){
                    // If we do this check when generating the netlist,
                    // we could delete the symmetrizationFactor member.
                    if (outputNode.getGate() == null &&
                        transistors.size() > depth * symmetrizationFactor) {
                        String msa = "WARNING";
                        String msb = "N-type transistor stack depth at maximum=" 
                              + tech.stackWarnLimitN + "\n  and not a gate or straight-chain.\n";
                        String msc = "CellName: " + subType.typeName + "\n"
                              + "NetName: " + outputNet.canonicalName.getCadenceString() + "\n";
                        if (! JautoUI.quiet)
                            messageCenter.createMessage(1, 16, msa, msb, msc);
                    }
                }
            }

            resistanceFactorRatio = tech.getEffectiveResistanceFactorN(getTransistorType(), depth-1)
                / tech.getEffectiveResistanceFactorN(getTransistorType(), 0);
        }
        else{ // PULL_UP
            if(depth > tech.stackWarnLimitP){
                String msa = "WARNING";
                String msb = "P-type transistor stack depth out of bound. The maximum allowable is " 
                      + tech.stackWarnLimitP + ".\n";
                String msc = "CellName: " + subType.typeName + "\n"
                      + "NetName: " + outputNet.canonicalName.getCadenceString() + "\n";
                if (! JautoUI.quiet)
                    messageCenter.createMessage(1, 17, msa, msb, msc);
            }
            else{
                // Max depth OK if matched to a gate or straight-chain.
                if(depth == tech.stackWarnLimitP){
                    if(outputNode.getGate() == null &&
                       transistors.size() > depth * symmetrizationFactor){
                        String msa = "WARNING";
                        String msb = "P-type transistor stack depth at maximum=" 
                              + tech.stackWarnLimitP + "\n  and not a gate or straight-chain.\n";
                        String msc = "CellName: " + subType.typeName + "\n"
                              + "NetName: " + outputNet.canonicalName.getCadenceString() + "\n";
                        if (! JautoUI.quiet)
                            messageCenter.createMessage(1, 18, msa, msb, msc);
                    }
                }
            }

            resistanceFactorRatio = tech.getEffectiveResistanceFactorP(getTransistorType(),depth-1)
                / tech.getEffectiveResistanceFactorP(getTransistorType(), 0);
        }

        return getCurrentSize() * depth * resistanceFactorRatio
            / getSymmetrizationFactor(); // width per fold
    }


    public HalfOperator getOpposingHalfOperator() {
        HalfOperator foundHalfOp = null;

        for (Iterator i = outputNet.getListSources().iterator();
             i.hasNext(); ) {
            final NetSource source = (NetSource) i.next();
            if (source.getType() == NetType.HALF_OPERATOR_TRANSISTOR) {
                final HalfOperator halfOp = source.getSource();
                assert halfOp.outputNet == outputNet;
                assert halfOp.subType == subType;
                if (halfOp.driveDirection != driveDirection) {
                    // TODO: When this assertion is verified, just
                    // return here.
                    assert foundHalfOp == null;
                    foundHalfOp = halfOp;
                }
            }
        }

        return foundHalfOp;
    }

    public Set getPrechargeNodes() {
        if (prechargeNodes == null) {
            prechargeNodes = new HashSet();
            for (NetGraph.NetEdge edge : transistors) {

                final NetGraph.GateInstance g1 = edge.source.getGate();
                if (g1 != null && g1.isPrechargePrimitive())
                    prechargeNodes.add(edge.source);

                final NetGraph.GateInstance g2 = edge.drain.getGate();
                if (g2 != null && g2.isPrechargePrimitive())
                    prechargeNodes.add(edge.drain);
            }
            if (prechargeNodes.isEmpty())
                prechargeNodes = Collections.EMPTY_SET;
        }
        return prechargeNodes;
    }

    public static Comparator getComparator() {
        if (comparator == null) {
            comparator = new Comparator() {
                public int compare(Object o1, Object o2) {
                    final HalfOperator h1 = (HalfOperator) o1;
                    final HalfOperator h2 = (HalfOperator) o2;
                    int x = h1.driveDirection - h2.driveDirection;
                    if (x != 0) return x;
                    x = ObjectUtils.compare(h1.subType.typeName,
                                            h2.subType.typeName);
                    if (x != 0) return x;
                    x = ObjectUtils.compare(h1.outputNode,
                                            h2.outputNode);
                    if (x != 0) return x;
                    x = ObjectUtils.compare(h1.outputNet.canonicalName,
                                            h2.outputNet.canonicalName);
                    return x;
                }
                public boolean equals(Object o) {
                    return this == o;
                }
            };
        }
        return comparator;
    }

    public int getTransistorType() {
        if (transistorType == TechnologyData.NO_TRANSISTOR_TYPE) {
            boolean warned = false;
            for (NetGraph.NetEdge edge : transistors) {
                if (transistorType == TechnologyData.NO_TRANSISTOR_TYPE) {
                    transistorType = edge.getTransistorType();
                } else if (transistorType != edge.getTransistorType()) {
                    if (!warned) {
                        warned = true;
                        final String msa = "WARNING";
                        final String msb = "Multiple transistor types found " +
                                           "in a single half-operator.\n";
                        final String msc =
                            "CellName: " + subType.typeName + "\n" +
                            "NetName: " +
                            outputNet.canonicalName.getCadenceString() + "\n";
                        if (! JautoUI.quiet)
                            subType.design.messageCenter
                                      .createMessage(1, 19, msa, msb, msc);
                    }
                    transistorType =
                        Math.max(transistorType, edge.getTransistorType());
                }
            }
        }
        return transistorType;
    }

    /**
     * size = conductance * getConductanceToSizeFactor()
     **/
    private double getConductanceToSizeFactor() {
        final TechnologyData tech = subType.design.getTechnologyData();
        return getEffectiveResistanceFactor() / getStrengthBias() *
               tech.defaultGateLength;
    }

    /**
     * Returns the conductance for a given size
     **/
    public double getConductanceForSize(final double size) {
        return size / getConductanceToSizeFactor();
    }

    /**
     * Returns the size for a given conductance
     **/
    public double getSizeForConductance(final double g) {
        return g * getConductanceToSizeFactor();
    }

    public FunctionTerm getResistanceTerm() {

        final FunctionTerm result;
        if (isVariableFixed()) {
            final double f = getConductanceToSizeFactor();
            result = new FunctionTerm(f / getCurrentSize());
        } else {
            result = new FunctionTerm(FunctionTerm.Type.ONE_OVER_VAR,
                                      getVariableName(), 1.0);
        }

        return result;
    }

    public AdditiveTerms getDiffusionCapacitanceTerm() {
        final TechnologyData tech = subType.design.getTechnologyData();
        final double unitDiffusionCapacitance =
            driveDirection == DriveDirection.PULL_DOWN ?
                tech.getUnitNmosDiffusionCapacitance(getTransistorType()) :
                tech.getUnitPmosDiffusionCapacitance(getTransistorType());
        final double f =
            getOutputDiffusionLength() * unitDiffusionCapacitance;

        final AdditiveTerms result;
        if (isVariableFixed()) {
            result = new AdditiveTerms(new FunctionTerm(f * getCurrentSize()));
        } else {
            result = new AdditiveTerms(
                    new FunctionTerm(FunctionTerm.Type.VAR,
                                     getVariableName(),
                                     f * getSizeForConductance(1.0) *
                                     tech.getWidthRoundingSlope()),
                    new FunctionTerm(f * tech.getWidthRoundingOffset() *
                                     getSymmetrizationFactor()));
        }

        return result;
    }
}
