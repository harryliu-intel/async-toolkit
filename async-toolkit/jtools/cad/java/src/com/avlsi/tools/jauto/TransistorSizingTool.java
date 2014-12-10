/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.*;
import java.util.*;

import com.avlsi.fast.CastDesign;
import com.avlsi.fast.CellType;
import com.avlsi.fast.CellNet;
import com.avlsi.fast.HalfOperator;

import com.avlsi.tools.jauto.NetSource;
import com.avlsi.tools.jauto.NetSink;
import com.avlsi.tools.jauto.FunctionTerm;
import com.avlsi.tools.jauto.JautoUI;

import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.util.container.Pair;
import com.avlsi.util.text.NumberFormatter;

/** Common methods and data for all transistor sizing tools. **/
abstract public class TransistorSizingTool
{
    // The container(owner) of this tool
    JautoMessageCenter      messageCenter;

    // The target design after synthesis, e.g., minimal gate network, etc.
    CastDesign              synthesizedDesign;

    // If there is unsatisfiable delay constraint
    public boolean                  hasUnsatisfiableConstraints;

    // Result output directory
    String                  outputDirectoryName;


    // Tool options

    // Maximum number of subtypes per sizing group
    // NB: This is not really the maximum number per group, as transistor
    // sharing can force more than this number per group.  This is only
    // the maximum in the absence of transistor sharing.
    int                     toolOptionMaxSubtypesPerGroup;

    // Option: delay value per node (second)
    double                  toolOptionUnitDelay;

    // Option: minimun unit delay (second)
    double                  toolOptionMinUnitDelay;

    // Option: size staticizers?
    boolean                 toolOptionSizeStaticizer;

    // Option: enable automatic reset unsatisfiable delay constraints
    boolean                 toolOptionEnableAutoConstraint;

    // Option: threshold value to decide if the size of a half-operator has converged
    double                  toolOptionSizeConvergeThreshold;

    // Option: threshold value for warning of small pre-sizing timing slack
    double                  toolOptionSlackWarningThreshold;

    // Option: maximum and minimum transistor width for gates (not staticizers)
    double                  toolOptionMinWidthP;
    double                  toolOptionMinWidthN;
    double                  toolOptionMaxWidthP;
    double                  toolOptionMaxWidthN;

    /**
     * A warning will be produced if an n-transistor has a width larger
     * than this value.  Negative value disables warning.
     **/
    double toolOptionNmosWarnWidth;

    /**
     * A warning will be produced if a p-transistor has a width larger
     * than this value.  Negative value disables warning.
     **/
    double toolOptionPmosWarnWidth;

    // Option for auto sub-typing algorighm
    double                  toolOptionMinLoadCap; // minimum possible load cap of the whole design (F)
    double                  toolOptionMaxLoadCap; // maximum possible load cap of the whole design (F)
    int                     toolOptionIntervalNumber; // number of intervals used to divide the whole region of

    // Option for internal E*Tau^2 sweep
    boolean                 toolOptionInternalSweep;
    int                     toolOptionInternalSweepNumber;
    double                  toolOptionInternalSweepStep;

    // Option for Tau stepping method in sizing;
    boolean                 toolOptionStepTau;

    // Option for early out on difficult sizing problems
    boolean                 toolOptionEarlyOut;

    // Option: multiplicative weight for dynamic gate capacitance in objective function
    double                  toolOptionDynamicEnergyPenalty = 1;

    // Option: overall method for the sizing program
    // Choices: PATH_BASED_SIZING_METHOD(default), NODE_BASED_SIZING_METHOD
    static final int    PATH_BASED_SIZING_METHOD       = 301;
    static final int    NODE_BASED_SIZING_METHOD       = 302;
    static final int    HYBRID_SIZING_METHOD           = 303;
    int                 toolOptionSizingMethod;


    /**
     * Specifies whether to use equality constraints or intermediate
     * variables.
     **/
    final boolean           useEqualityConstraints;

    // many lists and maps of things
    ArrayList/*<CellType>*/ listAllSubtypes;
    ArrayList/*<CellType>*/ listAllSizingSubtypes;
    ArrayList/*<CellType>*/ listAllFixedSubtypes;
    ArrayList/*<List<CellType>>*/ listAllGroupedSubtypes;

    ArrayList/*<List<FunctionTerm>>*/ listFunctions;

    /**
     * Map from a pair of GlobalNet and HalfOperator to the
     * variable for the delay of that pair.
     **/
    Map/*<Pair<GlobalNet,HalfOperator>,String>*/ delayMap;
    
    ArrayList/*<String>*/   listVariableNames;

    /**
     * Variables that are constrained to the value of an expression or
     * are intermediate variables.
     **/
    ArrayList/*<String>*/   listEqualityVariableNames;

    ArrayList/*<List<String>>*/ listGroupedVariableNames;
    List/*<Double>*/        listVariableLowerBounds;
    List/*<Double>*/        listVariableUpperBounds;
    List/*<Double>*/        listVariableStartValues;

    Map/*<String,List<HalfOperator>>*/ mapVarNameToHO;

    Map/*<String,Double>*/ mapVarNameToSize;


    /** constructor, should only be called from CG extensions **/
    TransistorSizingTool(final boolean useEqualityConstraints)
    {

        listAllSubtypes = new ArrayList/*<CellType>*/();
        listAllFixedSubtypes = new ArrayList/*<CellType>*/();
        listAllSizingSubtypes = new ArrayList/*<CellType>*/();
        listAllGroupedSubtypes = new ArrayList/*<List<CellType>>*/();

        listFunctions = new ArrayList/*<List<FunctionTerm>>*/();

        delayMap =
            new HashMap/*<Pair<GlobalNet,HalfOperator>,String>*/();

        listVariableNames = new ArrayList/*<String>*/();
        listEqualityVariableNames = new ArrayList/*<String>*/();
        listGroupedVariableNames = new ArrayList/*<List<String>>*/();
        listVariableLowerBounds = new ArrayList/*<Double>*/();
        listVariableUpperBounds = new ArrayList/*<Double>*/();
        listVariableStartValues = new ArrayList/*<Double>*/();

        hasUnsatisfiableConstraints = false;

        mapVarNameToHO = new TreeMap/*<String,List<HalfOperator>>*/();
        mapVarNameToSize = new TreeMap/*<String,Double>*/();

        setOptionMinLoadCap(-1);
        setOptionMaxLoadCap(-1);
        setOptionIntervalNumber(-1);

        setOptionMinWidthP(-1);
        setOptionMinWidthN(-1);
        setOptionMaxWidthP(-1);
        setOptionMaxWidthN(-1);

        setOptionNmosWarnWidth(-1.0);
        setOptionPmosWarnWidth(-1.0);

        setOptionSizeConvergeThreshold(-1);

        setOptionMaxSubtypesPerGroup(-1);

        setOptionUnitDelay(-1);
        setOptionMinUnitDelay(-1);

        setOptionSizeStaticizer(false);
        setOptionEnableAutoConstraint(false);


        setOptionInternalSweep(false);
        setOptionInternalSweepNumber(1);
        setOptionInternalSweepStep(0.0);

        setOptionStepTau(false);
        setOptionEarlyOut(false);

        setOptionSizingMethod(PATH_BASED_SIZING_METHOD);

        this.useEqualityConstraints = useEqualityConstraints;
    }


    public ArrayList/*<CellType>*/ setListAllSubtypes(ArrayList/*<CellType>*/ lst1)
    {
        listAllSubtypes = lst1;

        return listAllSubtypes;
    }


    public ArrayList/*<CellType>*/ getListAllSubtypes()
    {
        return listAllSubtypes;
    }

    
    public final boolean setOptionInternalSweep(final boolean b1)
    {
        toolOptionInternalSweep = b1;

        return toolOptionInternalSweep;
    }


    public final boolean getOptionInternalSweep()
    {
        return toolOptionInternalSweep;
    }


    public final int setOptionInternalSweepNumber(final int i1)
    {
        if(i1 <= 0){
            toolOptionInternalSweepNumber = 5;
        }
        else{
            toolOptionInternalSweepNumber = i1;
        }

        return toolOptionInternalSweepNumber;
    }


    public final int getOptionInternalSweepNumber()
    {
        return toolOptionInternalSweepNumber;
    }


    public final double setOptionInternalSweepStep(final double f1)
    {
        if(f1 <= 0.0){
            // FIXME: remove default
            toolOptionInternalSweepStep = 10.0E-12;
        }
        else{
            toolOptionInternalSweepStep = f1;
        }

        return toolOptionInternalSweepStep;
    }


    public final double getOptionInternalSweepStep()
    {
        return toolOptionInternalSweepStep;
    }


    public final boolean setOptionStepTau(final boolean b1)
    {
        toolOptionStepTau = b1;

        return toolOptionStepTau;
    }


    public final boolean getOptionStepTau()
    {
        return toolOptionStepTau;
    }


    public final boolean setOptionEarlyOut(final boolean b1)
    {
        toolOptionEarlyOut = b1;

        return toolOptionEarlyOut;
    }


    public final boolean getOptionEarlyOut()
    {
        return toolOptionEarlyOut;
    }


    public final String setOutputDirectoryName(String s)
    {
        outputDirectoryName = s;

        return outputDirectoryName;
    }


    public final String getOutputDirectoryName()
    {
        return outputDirectoryName;
    }


    public boolean setOptionSizeStaticizer(final boolean size) {
        toolOptionSizeStaticizer = size;
        return toolOptionSizeStaticizer;
    }


    public boolean getOptionSizeStaticizer() {
        return toolOptionSizeStaticizer;
    }


    public boolean setOptionEnableAutoConstraint(final boolean b)
    {
        toolOptionEnableAutoConstraint = b;
        return toolOptionEnableAutoConstraint;
    }


    public boolean getOptionEnableAutoConstraint()
    {
        return toolOptionEnableAutoConstraint;
    }


    public double setOptionMinWidthP(final double f)
    {
        if(f <= 0.0){
            // FIXME: remove default
            toolOptionMinWidthP = 0.48E-6;
        }
        else{
            toolOptionMinWidthP = f;
        }

        return toolOptionMinWidthP;
    }
    
    public double getOptionMinWidthP()
    {
        return toolOptionMinWidthP;
    }


    public double setOptionMinWidthN(final double f)
    {
        if(f <= 0.0){
            // FIXME: remove default
            toolOptionMinWidthN = 0.48E-6;
        }
        else{
            toolOptionMinWidthN = f;
        }

        return toolOptionMinWidthN;
    }
    
    public double getOptionMinWidthN()
    {
        return toolOptionMinWidthN;
    }


    public double setOptionMaxWidthP(double f)
    {
        if(f <= 0.0){
            // FIXME: remove default
            toolOptionMaxWidthP = 100.0E-6;
        }
        else{
            toolOptionMaxWidthP = f;
        }

        return toolOptionMaxWidthP;
    }
    
    public double getOptionMaxWidthP()
    {
        return toolOptionMaxWidthP;
    }
    

    public double setOptionMaxWidthN(double f)
    {
        if(f <= 0.0){
            // FIXME: remove default
            toolOptionMaxWidthN = 100.0E-6;
        }
        else{
            toolOptionMaxWidthN = f;
        }

        return toolOptionMaxWidthN;
    }
    
    public double getOptionMaxWidthN()
    {
        return toolOptionMaxWidthN;
    }

    /**
     * Set the warning width for n-transistors.  If an n-transistor
     * has a width larger than this, a warning will be emitted.
     * Pass a non-positive value to disable the warning.
     *
     * @param nmosWarnWidth
     *        A warning will be emitted for transistors larger than
     *        this width.  Set negative to disable
     **/
    public void setOptionNmosWarnWidth(final double nmosWarnWidth) {
        toolOptionNmosWarnWidth = nmosWarnWidth;
    }

    /**
     * Get the warning width for n-transistors.  
     **/
    public double getOptionNmosWarnWidth() {
        return toolOptionNmosWarnWidth;
    }

    /**
     * Set the warning width for p-transistors.  If a p-transistor
     * has a width larger than this, a warning will be emitted.
     * Pass a non-positive value to disable the warning.
     *
     * @param pmosWarnWidth
     *        A warning will be emitted for transistors larger than
     *        this width.  
     **/
    //@ ensures toolOptionPmosWarnWidth > 0.0;
    public void setOptionPmosWarnWidth(final double pmosWarnWidth) {
        toolOptionPmosWarnWidth = pmosWarnWidth;
    }

    /**
     * Get the warning width for p-transistors.  
     **/
    public double getOptionPmosWarnWidth() {
        return toolOptionPmosWarnWidth;
    }
    

    public double setOptionSizeConvergeThreshold(double f)
    {
        if((f <= 0.0) || (f >= 10.0 * toolOptionMinWidthN)){
            // FIXME: remove default
            toolOptionSizeConvergeThreshold = 0.1E-6;
        }
        else{
            toolOptionSizeConvergeThreshold = f;
        }

        return toolOptionSizeConvergeThreshold;
    }
    

    public double getOptionSizeConvergeThreshold()
    {
        return toolOptionSizeConvergeThreshold;
    }


    public double setOptionSlackWarningThreshold(double f)
    {
        if((f <= 0.0)){
            toolOptionSlackWarningThreshold = 0.0;
        }
        else{
            toolOptionSlackWarningThreshold = f;
        }

        return toolOptionSlackWarningThreshold;
    }
    

    public double getOptionSlackWarningThreshold()
    {
        return toolOptionSlackWarningThreshold;
    }


    public int setOptionIntervalNumber(int i)
    {
        if(i < 0){
            toolOptionIntervalNumber = 1;
        }
        else{
            toolOptionIntervalNumber = i;
        }

        return toolOptionIntervalNumber;
    }

    public int getOptionIntervalNumber()
    {
        return toolOptionIntervalNumber;
    }


    public double setOptionMinLoadCap(double d)
    {
        if(d < 0.0){
            toolOptionMinLoadCap = 0.0;
        }
        else{
            if(d > toolOptionMaxLoadCap){
                toolOptionMinLoadCap = toolOptionMaxLoadCap;
            }
            else{
                toolOptionMinLoadCap = d;
            }
        }

        return toolOptionMinLoadCap;
    }


    public double getOptionMinLoadCap()
    {
        return toolOptionMinLoadCap;
    }

    public double setOptionMaxLoadCap(double d)
    {
        if(d < 0.0){
            // FIXME: remove default
            toolOptionMaxLoadCap = 1.0E-12;
        }
        else{
            if(d < toolOptionMinLoadCap){
                toolOptionMaxLoadCap = toolOptionMinLoadCap;
            }
            else{
                toolOptionMaxLoadCap = d;
            }
        }

        return toolOptionMaxLoadCap;
    }


    public double getOptionMaxLoadCap()
    {
        return toolOptionMaxLoadCap;
    }


    public int setOptionMaxSubtypesPerGroup(int i)
    {
        if (i <= 0){
            toolOptionMaxSubtypesPerGroup = 1;
        }
        else{ //default is to size one subtype at a time
            toolOptionMaxSubtypesPerGroup = i;
        }

        return toolOptionMaxSubtypesPerGroup;
    }


    public int getOptionMaxSubtypesPerGroup()
    {
        return toolOptionMaxSubtypesPerGroup;
    }


    public double setOptionUnitDelay(double d)
    {
        if(d <= 0.0){ // default setting = 80 ps
            // FIXME: remove default
            toolOptionUnitDelay = 80.0E-12;
        }
        else{
            toolOptionUnitDelay = d;
        }

        return toolOptionUnitDelay;
    }

    public double getOptionUnitDelay()
    {
        return toolOptionUnitDelay;
    }


    public double setOptionMinUnitDelay(final double d)
    {
        if(d <= 0.0){ // default setting = 20 ps
            // FIXME: remove default
            toolOptionMinUnitDelay = 20.0E-12;
        }
        else{
            toolOptionMinUnitDelay = d;
        }

        return toolOptionMinUnitDelay;
    }

    public double getOptionMinUnitDelay()
    {
        return toolOptionMinUnitDelay;
    }


    public void setOptionSizingMethod(int sizingMethod)
    {
        assert sizingMethod == PATH_BASED_SIZING_METHOD ||
               sizingMethod == NODE_BASED_SIZING_METHOD;
        toolOptionSizingMethod = sizingMethod;
    }

    public int getOptionSizingMethod()
    {
        assert toolOptionSizingMethod == PATH_BASED_SIZING_METHOD ||
               toolOptionSizingMethod == NODE_BASED_SIZING_METHOD;
        return toolOptionSizingMethod;
    }

    public CastDesign setSynthesizedDesign(CastDesign d)
    {
        synthesizedDesign = d;

        return synthesizedDesign;
    }

    public CastDesign getSynthesizedDesign()
    {
        return synthesizedDesign;
    }

    public double setOptionDynamicEnergyPenalty(double x)
    {
        toolOptionDynamicEnergyPenalty = (x>0) ? x : 1;
        return toolOptionDynamicEnergyPenalty;
    }
    
    public double getOptionDynamicEnergyPenalty()
    {
        return  toolOptionDynamicEnergyPenalty;
    }

    public JautoMessageCenter setMessageCenter(JautoMessageCenter mc)
    {
        messageCenter = mc;

        return messageCenter;
    }


    public JautoMessageCenter getMessageCenter()
    {
        return messageCenter;
    }

    /** must define this in specific CG extension **/
    abstract public String sizeTransistors();

    /**
     * Group variables.  Only CG solver as non-trivial implementation.
     **/
    abstract void groupVariables();

    /**
     * Genereate the objective function(s).
     **/
    abstract void generateObjectiveFunction(Set/*<CellType>*/ setc);

    /**
     * Run the external solver program.
     **/
    abstract String runSolver(int iterationNumber, int groupNumber);

    /**
     * Generate, formulate, output problem, solve, read-back.
     **/
    String runOptimization(ArrayList/*<CellType>*/ listCells,
                           int iterationNumber,
                           int groupNumber)
    {
        listFunctions.clear();
        mapVarNameToHO.clear();
        mapVarNameToSize.clear();

        delayMap.clear();

        listVariableNames.clear();
        listEqualityVariableNames.clear();


        listVariableLowerBounds.clear();
        listVariableUpperBounds.clear();
        listVariableStartValues.clear();

        if(DebugOption.printLevel <= 1){
            System.out.println("Current step: generating objective function");
        }


        // Unfix variables for the cells to be sized
        //iterator of list of CellType
        for (Iterator ita = listCells.iterator(); ita.hasNext(); ){
            CellType cella = (CellType)ita.next(); // get next CellType
            cella.unfixVariables(); // unfix variables related to half-operators

            if(DebugOption.printLevel <= 3){
                System.out.println("CellName=" + cella.typeName + ", Fixed=" + cella.isFixedSize());
            }
        }


        // Generate list of variable groups
        groupVariables();


        // Find all the related leaf cells
        Set/*<CellType>*/ setc = new LinkedHashSet/*<CellType>*/();
        for (Iterator ita = listCells.iterator(); ita.hasNext(); ){
            CellType cella = (CellType)ita.next();

            if(cella.getLevel() == 0){
                setc.add(cella);
            }
            else{
                for (Iterator itb = cella.setSizingHalfOperators.iterator();
                     itb.hasNext(); ) {
                    HalfOperator hoa = (HalfOperator)itb.next();
                    setc.add(hoa.subType);
                }
            }
        }


        // Generate objective function
        generateObjectiveFunction(setc);


        generateVariableBoundsAndStartValues();



        if(DebugOption.printLevel <= 0){
            System.out.println("Current step: generating constraint functions");
        }


        generateMinDelayConstraints(setc, iterationNumber);


        generateConstraints(listCells);

        if (! JautoUI.quiet) {
            System.err.println("\033[1;32m" + JautoUI.getCurrentTime() + 
                               " -- Starting solver." + "\033[0m");
            System.out.println(JautoUI.getCurrentTime() + " -- Starting solver.");
        }

        String result = runSolver(iterationNumber, groupNumber);
        
        if (! JautoUI.quiet) {
            System.err.println("\033[1;32m" + JautoUI.getCurrentTime() + 
                               " -- Finished solver." + "\033[0m");
            System.out.println(JautoUI.getCurrentTime() + " -- Finished solver.");
        }

        if (result != "Done")
            return result;

        if(DebugOption.printLevel <= 0){
            System.out.println("Current step: fixing variables");
        }

        // Fix variables
        for (Iterator ita = listCells.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();
            cella.fixVariables();
        }
        // Done with variable fix


        return "Done";
    }

    /**
     * Create another term of the objective function
     **/
    FunctionTerm newObjectiveTerm(String sta, double f, int instCount,
                                  boolean isFixed, HalfOperator hoa) {
        // chose objective weighting function (higher for dynamic, non-library operators)
        double weight = instCount;
        if (!hoa.isCombinational && !hoa.isLibraryGate)
            weight *= toolOptionDynamicEnergyPenalty;
        
        // create the term
        FunctionTerm terma = new FunctionTerm();
        if(isFixed){ // fixed variable leads to a constant
            f = f * hoa.getCurrentSize();
            terma.type = FunctionTerm.Type.CONSTANT;
            terma.coefficient = f * weight;
        }
        else{ // unfixed variable leads to a linear term
            terma.type = FunctionTerm.Type.VAR;
            terma.variableName_1 = sta;
            terma.coefficient = f * weight * hoa.getSizeForConductance(1.0);
        }
        return terma;
    }

    // Assign variable names to all the half-operators in the design
    // This step should be done every time after sub-typing
    // configuration has been changed
    public void assignVariableNames()
    {
        int varNum = 0;

        // One variable per half-operator.  Combine half-operators that
        // share transistors.

        for (Iterator ita = listAllSubtypes.iterator(); ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();

            if(DebugOption.printLevel <= 3){
                System.out.println("Assigning variable names for cell: " + sta.typeName);
            }

            // halfOpSharings is a partition of the half operators from the
            // cell formed by the transitive closure of the relation
            // HalfOperator.isSharingWith().  
            List/*<List<HalfOperator>>*/ halfOpSharings =
                new ArrayList/*<List<HalfOperator>>*/();

            for (Iterator itb = sta.getListHalfOperators().iterator(); itb.hasNext(); ) {
                HalfOperator hoa = (HalfOperator)itb.next();

                boolean shared = false;
                List/*<HalfOperator>*/ halfOpsSharedWith = null;

                // Go through all half-ops hob in halfOpSharings and see if
                // hoa shares with any of those.
                for (Iterator itc = halfOpSharings.iterator(); itc.hasNext(); ) {
                    List/*<HalfOperator>*/ lstc = (List)itc.next();
                    for (Iterator itd = lstc.iterator(); itd.hasNext(); ){
                        HalfOperator hob = (HalfOperator)itd.next();
                        if(hoa.isSharingWith(hob) ||
                           hoa.isSameStrengthGroup(hob)){
                            if(!shared){
                                // This is the first group that hoa shares
                                // with.
                                if(DebugOption.printLevel <= 3){
                                    System.out.println("Note: Found transistor sharing between half-operators or strength_group directives.");
                                }

                                shared = true;

                                // Remember the group for later and add hoa
                                // to it.
                                halfOpsSharedWith = lstc;
                                lstc.add(hoa);
                            }
                            else{
                                // This is not the first group that hoa
                                // shares with.
                                if(DebugOption.printLevel <= 3){
                                    System.out.println("Note: Found complex transistor sharing between half-operators or strength_group directives.");
                                }

                                // Add this group to the remembered one and delete
                                // this one because it is now merged.
                                halfOpsSharedWith.addAll(lstc);
                                itc.remove();
                            }

                            // As soon as we share with one half-operator
                            // in the group, we have handled the whole
                            // group.  Go to the next one.
                            break;
                        }
                    }
                }

                // If we didn't share with anything, then add half-operator
                // as a new group.
                if(!shared){
                    List/*<HalfOperator>*/ newGroup =
                        new ArrayList/*<HalfOperator>*/();
                    newGroup.add(hoa);
                    halfOpSharings.add(newGroup);
                }

            }
               
            // Assign variable names to all half operators.
            for (Iterator itc = halfOpSharings.iterator(); itc.hasNext(); ) {
                List/*<HalfOperator>*/ lstc = (List)itc.next();

                // Half-operators that share transistors get the same name.
                String sa = "X" + varNum;
                for (Iterator itd = lstc.iterator(); itd.hasNext(); ) {
                    HalfOperator hob = (HalfOperator)itd.next();
                    hob.setVariableName(sa);
                }

                if(DebugOption.printLevel <= 3){
                    System.out.println("\tAdded variable: " + sa);
                }

                varNum++;
            }


        }
    }

    /**
     * Append variable lower, upper bounds and start value to lists
     * <code>listVariableLowerBounds</code>,
     * <code>listVariableUpperBounds</code>, and
     * <code>listVariableStartValues</code> respectively.  Bounds are
     * computed so that transistor sizes will be between
     * <code>toolOptionMinWidthN</code> and
     * <code>toolOptionMaxWidthN</code> for pull-downs and
     * similarly for pull-ups.  Both stack depth and effective
     * resistance are corrected for.
     **/
    private void generateVariableBoundsAndStartValues() {
        for (Iterator ita = listVariableNames.iterator(); ita.hasNext(); ){
            String sta = (String)ita.next();
            List/*<HalfOperator>*/ lstc = (List)mapVarNameToHO.get(sta);
            TechnologyData tdata = synthesizedDesign.getTechnologyData();

            // Compute the max of the min allowed widths and the min of
            // the max allowed widths for each half-operator that uses
            // this variable.  This gives us the safe bounds for the
            // half-operator sizes.
            double minAllowedConductance = Double.NEGATIVE_INFINITY;
            double maxAllowedConductance = Double.POSITIVE_INFINITY;
            for (final Iterator itb = lstc.iterator(); itb.hasNext(); ) {
                final HalfOperator hob = (HalfOperator) itb.next();

                final double minWidth;
                final double maxWidth;
                if (hob.driveDirection ==
                        HalfOperator.DriveDirection.PULL_DOWN) {
                    minWidth = toolOptionMinWidthN;
                    maxWidth = toolOptionMaxWidthN;
                } else {
                    minWidth = toolOptionMinWidthP;
                    maxWidth = toolOptionMaxWidthP;
                }

                double minAllowedSize = Double.NEGATIVE_INFINITY;
                double maxAllowedSize = Double.POSITIVE_INFINITY;

                final int[] depths = hob.getDepths();
                for (int i = 0; i < depths.length; ++i) {
                    final int depth = depths[i];
                    // Transistor size is proportional to half-operator
                    // size, and getWidth(depth) / getCurrentSize() gives
                    // us the proportionality constant.  It would be nice
                    // if there were a getSizeFactor(), but there isn't yet.
                    // The trick of using getWidth(depth) takes care of
                    // both depth and erf correction.
                    minAllowedSize = Math.max(minAllowedSize,
                                              minWidth *
                                              hob.getSymmetrizationFactor() *
                                              hob.getCurrentSize() /
                                              hob.getWidth(depth));
                    // don't use symmetrization factor for max_width constraints
                    maxAllowedSize = Math.min(maxAllowedSize,
                                              maxWidth *
                                              hob.getCurrentSize() /
                                              hob.getWidth(depth));
                }

                // compute minimum size to overpower weakest staticizer
                if (!hob.isCombinational) {
                    double minSize =
                        SizeStaticizers.getMinimumLogicSize(tdata,hob);
                    if (minSize>minAllowedSize) {
                        /*** debugging
                        System.out.println
                            ("NOTE: minWidth of " +
                             hob.outputNet.canonicalName.getCadenceString()
                             + (hob.driveDirection ==
                                HalfOperator.DriveDirection.PULL_DOWN ?
                                "-" : "+") + 
                             " increased from " +
                             NumberFormatter.format(minAllowedSize,3) +
                             " to " +
                             NumberFormatter.format(minSize,3));
                        ***/
                        minAllowedSize = minSize;
                    }
                }

                minAllowedConductance =
                    Math.max(minAllowedConductance,
                             hob.getConductanceForSize(minAllowedSize));
                maxAllowedConductance =
                    Math.min(maxAllowedConductance,
                             hob.getConductanceForSize(maxAllowedSize));
            }
            
            listVariableLowerBounds.add(new Double(minAllowedConductance));
            listVariableUpperBounds.add(new Double(maxAllowedConductance));
            
            // arbitrarily set initial value to twice minAllowedConductance
            listVariableStartValues.add(new Double(2*minAllowedConductance));
        }
    }

    private void generateMinDelayConstraints(final Set/*<CellType>*/ setc,
                                             final int iterationNumber) {
        // Generate special constraints for minimum delay requirement
        // Note: This breaks the posinomial nature of the problem.
        for (Iterator ita = setc.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();

            if(cella.getListHalfOperators().size() > 0){
                for (Iterator itb = cella.getListHalfOperators().iterator();
                     itb.hasNext(); ) {
                    HalfOperator hoa = (HalfOperator)itb.next();
                    if(hoa.minDelay > 0){ // there is minimum delay specified by user
                        if(iterationNumber < 2){
                            String msa = "WARNING";
                            String msb = "Found user-specified minimum delay requirement.\n";
                            String msc = "CellName: " + cella.typeName + "\n"
                                + "HalfOperatorName: " + hoa.outputNet.canonicalName.getCadenceString()
                                + (hoa.driveDirection ==
                                       HalfOperator.DriveDirection.PULL_DOWN ?
                                   "-" : "+") + "\n";

                            messageCenter.createMessage(1, 11, msa, msb, msc);
                        }

                        for (Iterator itc = hoa.outputNet.getGlobalNets().iterator();
                             itc.hasNext(); ) {
                            GlobalNet gna = (GlobalNet)itc.next();
                            List<FunctionTerm> functiona =
                                new ArrayList<FunctionTerm>();

                            FunctionTerm terma = new FunctionTerm();
                            terma.type =
                                FunctionTerm.Type.CONSTRAINT_LESS_THAN;
                            functiona.add(terma);

                            getDelayFunction(gna, hoa, functiona);

                            for (FunctionTerm ft : functiona) {
                                // negate the coefficients, to comply with "<=" constraint type
                                ft.coefficient *= -1.0;
                            }

                            terma = new FunctionTerm();
                            terma.type = FunctionTerm.Type.CONSTANT;
                            terma.coefficient = -1.0 * toolOptionUnitDelay * (cella.getDelay().getNativeDelay(hoa.driveDirection != HalfOperator.DriveDirection.PULL_DOWN) / 100.0) * hoa.minDelay;
                            functiona.add(terma);

                            listFunctions.add(functiona);
                        }
                    }
                }
            }
        }
    }

    public static float findDelayBias(final GlobalNet gn,
                                      final HalfOperator op) {
        float result = Float.POSITIVE_INFINITY;
        for (Iterator i = gn.getListSources().iterator(); i.hasNext(); ) {
            final NetSource src = (NetSource) i.next();
            if (src.getSource() == op) {
                result = Math.min(result, src.getDelayBias());
            }
        }
        return result == Float.POSITIVE_INFINITY ? 1 : result;
    }

    private void generateConstraints(final List/*<CellType>*/ listCells) {
        // Generate the constraints
        List/*<GlobalNet>*/ lste = new ArrayList/*<GlobalNet>*/();
        for (Iterator ita = listCells.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();

            if(DebugOption.printLevel <= 1){
                System.out.println(cella.toString());
            }

            if(cella.getLevel() == 0){ // Leaf-cell

                if(DebugOption.printAll){
                    System.out.println("Number of paths: " +
                                       cella.getSizingPaths().size());
                }

                for (Iterator itb = cella.getSizingPaths().iterator();
                     itb.hasNext(); ) {
                    SizingPath spa = (SizingPath)itb.next();

                    if(!spa.isFragment()){
                        List/*<HalfOperator>*/ lsta = spa.getPath();

                        List<FunctionTerm> functiona =
                            new ArrayList<FunctionTerm>(); // one constraint function per path

                        FunctionTerm terma = new FunctionTerm();
                        terma.type = FunctionTerm.Type.CONSTRAINT_LESS_THAN;
                        functiona.add(terma);

                        int k = lsta.size();

                        if(DebugOption.printLevel <= 1){
                            System.out.println("size of the path: " + k);
                            if(k >= 10){
                                System.out.println(spa.toString());
                            }
                        }

                        if(DebugOption.printAll){
                            System.out.println("Length of the path: " + k);
                        }

                        double totalDelayBias = 0.0;

                        for(int i=0;i<k-1;++i){
                            HalfOperator hoa = (HalfOperator)lsta.get(i);

                            totalDelayBias += hoa.getDelayBias();

                            CellNet cna = hoa.outputNet;

                            assert cna.getGlobalNets().size() <= 1
                                : "Internal CellNet have more than 1 global net.\n" +
                                  "CellName: " + cella.typeName + "\n" +
                                  "NetName: " + cna.canonicalName.getCadenceString();

                            GlobalNet gna = (GlobalNet)cna.getGlobalNets().get(0);

                            getDelayFunction(gna, hoa, functiona); // get the delay expression
                        }

                        HalfOperator hoa = (HalfOperator)lsta.get(k-1);

                        totalDelayBias += hoa.getDelayBias();

                        CellNet cna = spa.getEndNet();

                        lste.clear();

                        //lste.addAll(cna.getGlobalNets());

                        for (Iterator itc = cna.getGlobalNets().iterator(); itc.hasNext(); ) {
                            GlobalNet gna = (GlobalNet)itc.next();
                            final double gnaBias = findDelayBias(gna, hoa);

                            if(lste.isEmpty()){
                                lste.add(gna);
                                continue;
                            }

                            boolean found = false;
                            for (Iterator itd = lste.iterator();
                                 itd.hasNext(); ) {
                                GlobalNet gnb = (GlobalNet)itd.next();
                                if(gnb.isSameTypeAs(gna) &&
                                   gnaBias == findDelayBias(gnb, hoa)){
                                    if(DebugOption.printLevel <= 2){
                                        System.out.println("Global net of same type found");
                                    }
                                    found = true;
                                    if(gna.getWireRC() > gnb.getWireRC()){
                                        found = false;
                                        itd.remove();
                                    }
                                    break;
                                }
                            }
                            if(!found){
                                lste.add(gna);
                            }
                        }

                        for (Iterator itc = lste.iterator(); itc.hasNext(); ) {
                            GlobalNet gna = (GlobalNet)itc.next();

                            List<FunctionTerm> functionb =
                                new ArrayList<FunctionTerm>(functiona);

                            getDelayFunction(gna, hoa, functionb); // get the delay expression

                            // Get extra delay setting in the middle-level cell
                            double extraDelay = DelayCalculator.getExtraDelay(hoa, gna, getOptionUnitDelay());

                            terma = new FunctionTerm();
                            terma.type = FunctionTerm.Type.CONSTANT;
                            terma.coefficient = totalDelayBias
                                * findDelayBias(gna, hoa)
                                * getOptionUnitDelay() + extraDelay;
                            functionb.add(terma);


                            // Check for unsatisfiable delay constraints
                            boolean isAllConstant = true;
                            double f = 0.0;
                            int j = functionb.size();
                            for(int o=1;o<j-1;++o){
                                FunctionTerm termb = functionb.get(o);
                                if(termb.type == FunctionTerm.Type.CONSTANT){
                                    f += termb.coefficient;
                                }
                                else{
                                    isAllConstant = false;
                                }
                            }

                            FunctionTerm termb = functionb.get(j-1);
                            double delayBudget = termb.coefficient;
                            if(!isAllConstant){
                                if (f >= delayBudget) {
                                    // Unsatisfiable constraint
                                    hasUnsatisfiableConstraints = true;

                                    String msa = "ERROR";
                                    String msb = "Unsatisfiable delay constraint: " + delayBudget + " Slack: " + (delayBudget-f) + "\n";
                                    String msc = "Cell Name: " + cella.typeName + "\n"
                                        + "Path information: " + spa.toString() + "\n"
                                        + "Global net information for the last cellnet on the path:" + "\n"
                                        + gna.toString("\t") + "\n";

                                    messageCenter.createMessage(0, 4, msa, msb, msc);

                                    if(toolOptionEnableAutoConstraint){
                                        hasUnsatisfiableConstraints = false;
                                        delayBudget = f + (toolOptionMinUnitDelay * k);
                                        termb.coefficient = delayBudget;

                                        String msd = "WARNING";
                                        String mse = "Jauto has automatically set constraint to: " + delayBudget + " Slack: " + (delayBudget-f) + "\n";
                                        String msf = "";

                                        messageCenter.createMessage(1, 12, msd, mse, msf);
                                    }
                                } else if (delayBudget - f <= toolOptionSlackWarningThreshold) {
                                    String msa = "WARNING";
                                    String msb = "Small delay slack: " + (delayBudget-f) + "\n";
                                    String msc = "CellName: " + cella.typeName + "\n"
                                        + "Path information: " + spa.toString() + "\n"
                                        + "Global net information for the last cellnet on the path:" + "\n"
                                        + gna.toString("\t") + "\n";

                                    messageCenter.createMessage(1, 13, msa, msb, msc);

                                }
                                listFunctions.add(functionb); // add one more constraint 
                            }
                            // End of check for unsatisfiable or small delay constraints
                        }
                    }
                }
            }
            else{ // Non-leaf cell

                for (Iterator itb = cella.getReducedCatPaths().iterator();
                     itb.hasNext(); ) {
                    CatPath cpa = (CatPath)itb.next();

                    if(!cpa.isFragment()){
                        List/*<SizingPath>*/ lsta = cpa.getCatPath();

                        double extraDelay = 0.0;
                        double totalDelayBias = 0.0;

                        List<FunctionTerm> functiona =
                            new ArrayList<FunctionTerm>();

                        FunctionTerm terma = new FunctionTerm();
                        terma.type = FunctionTerm.Type.CONSTRAINT_LESS_THAN;
                        functiona.add(terma);

                        int k = lsta.size();
                        int m = 0;

                        for(int i=0;i<k;++i){
                            SizingPath spa = (SizingPath)lsta.get(i);
                            double pathDelayBias = 0.0;

                            List/*<HalfOperator>*/ lstb = spa.getPath();

                            int l = lstb.size();
                            m += l;

                            for(int j=0;j<l;++j){

                                if(j < (l-1)){
                                    HalfOperator hoa = (HalfOperator)lstb.get(j);

                                    pathDelayBias += hoa.getDelayBias();

                                    CellNet cna = hoa.outputNet;

                                    assert cna.getGlobalNets().size() <= 1
                                        : "Internal CellNet has more than 1 global net.\n" +
                                          "CellName: " + cella.typeName + "\n" +
                                          "NetName: " + cna.canonicalName.getCadenceString();

                                    GlobalNet gna = (GlobalNet)cna.getGlobalNets().get(0);

                                    getDelayFunction(gna, hoa, functiona); // get the delay expression

                                }
                                else{
                                    if(i < (k-1)){
                                        HalfOperator hoa = (HalfOperator)lstb.get(j);
                                        pathDelayBias += hoa.getDelayBias();

                                        CellNet cna = spa.getEndNet();

                                        assert cna.getGlobalNets().size() <= 1
                                            : "Internal CellNet has more than 1 global net.\n" +
                                              "CellName: " + cella.typeName + "\n" +
                                              "NetName: " + cna.canonicalName.getCadenceString();

                                        GlobalNet gna = (GlobalNet)cna.getGlobalNets().get(0);
                                        totalDelayBias += pathDelayBias
                                            * findDelayBias(gna, hoa);

                                        getDelayFunction(gna, hoa, functiona); // get the delay expression

                                        // Get extra delay budget
                                        extraDelay += DelayCalculator.getExtraDelay(hoa, gna, getOptionUnitDelay());
                                    }
                                    else{
                                        HalfOperator hoa = (HalfOperator)lstb.get(j);
                                        pathDelayBias += hoa.getDelayBias();

                                        CellNet cna = cpa.getEndNet();

                                        lste.clear();

                                        for (Iterator itc = cna.getGlobalNets().iterator(); itc.hasNext(); ) {
                                            GlobalNet gna = (GlobalNet)itc.next();
                                            final double gnaBias =
                                                findDelayBias(gna, hoa);

                                            if(lste.isEmpty()){
                                                lste.add(gna);
                                                continue;
                                            }

                                            boolean found = false;
                                            for (Iterator itd = lste.iterator(); itd.hasNext(); ) {
                                                GlobalNet gnb = (GlobalNet)itd.next();
                                                if(gnb.isSameTypeAs(gna) &&
                                                   gnaBias == findDelayBias(gnb, hoa)){
                                                    if(DebugOption.printLevel <= 2){
                                                        System.out.println("Global net of same type found");
                                                    }
                                                    found = true;
                                                    if(gna.getWireRC() > gnb.getWireRC()){
                                                        found = false;
                                                        itd.remove();
                                                    }
                                                    break;
                                                }
                                            }
                                            if(!found){
                                                lste.add(gna);
                                            }
                                        }

                                        for (Iterator itc = lste.iterator(); itc.hasNext(); ) {
                                            GlobalNet gna = (GlobalNet)itc.next();

                                            List<FunctionTerm> functionb =
                                                new ArrayList<FunctionTerm>(functiona);

                                            getDelayFunction(gna, hoa, functionb); // get the delay expression

                                            double totalExtraDelay = extraDelay + DelayCalculator.getExtraDelay(hoa, gna, getOptionUnitDelay());

                                            terma = new FunctionTerm();
                                            terma.type = FunctionTerm.Type.CONSTANT;
                                            terma.coefficient =
                                                (totalDelayBias +
                                                 pathDelayBias *
                                                 findDelayBias(gna, hoa))
                                                * getOptionUnitDelay()
                                                + totalExtraDelay;
                                            functionb.add(terma);


                                            // Check for unsatisfiable delay constraints
                                            boolean isAllConstant = true;
                                            double f = 0.0;
                                            int p = functionb.size();
                                            for(int o=1;o<p-1;++o){
                                                FunctionTerm termb = functionb.get(o);
                                                if(termb.type == FunctionTerm.Type.CONSTANT){
                                                    f += termb.coefficient;
                                                }
                                                else{
                                                    isAllConstant = false;
                                                }
                                            }

                                            FunctionTerm termb = functionb.get(p-1);
                                            double delayBudget = termb.coefficient;
                                            if(!isAllConstant){
                                                if (f >= delayBudget) {
                                                    // Unsatisfiable constraint
                                                    hasUnsatisfiableConstraints = true;

                                                    String msa = "ERROR";
                                                    String msb = "Unsatisfiable delay constraint: " + delayBudget + " Slack: " + (delayBudget-f) + "\n";
                                                    String msc = "Cell Name: " + cella.typeName + "\n"
                                                        + "Path information: " + cpa.toString() + "\n"
                                                        + "Global net information for the last cellnet on the path:" + "\n"
                                                        + gna.toString("\t") + "\n";

                                                    messageCenter.createMessage(0, 4, msa, msb, msc);

                                                    if(toolOptionEnableAutoConstraint){
                                                        hasUnsatisfiableConstraints = false;
                                                        delayBudget = f + (toolOptionMinUnitDelay * m);
                                                        termb.coefficient = delayBudget;

                                                        String msd = "WARNING";
                                                        String mse = "Jauto has automatically set constraint to: " + delayBudget + " Slack: " + (delayBudget-f) + "\n";
                                                        String msf = "";

                                                        messageCenter.createMessage(1, 12, msd, mse, msf);
                                                    }
                                                } else if (delayBudget - f <= toolOptionSlackWarningThreshold) {
                                                    String msa = "WARNING";
                                                    String msb = "Small delay slack: " + (delayBudget-f) + "\n";
                                                    String msc = "Cell Name: " + cella.typeName + "\n"
                                                        + "Path information: " + cpa.toString() + "\n"
                                                        + "Global net information for the last cellnet on the path:" + "\n"
                                                        + gna.toString("\t") + "\n";

                                                    messageCenter.createMessage(1, 13, msa, msb, msc);
                                                }

                                                listFunctions.add(functionb); // add one more constraint 
                                            }
                                            // End of check for unsatisfiable or small delay constraints
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }


    /**
     * Adds the delay function for the pair of GlobalNet and HalfOperator
     * to the list <code>delayFunction</code>.  If this is the first time
     * this pair's delay has been queried, an intermediate variable
     * (which will be handled as an equality constraint) will be added.
     **/
    private void getDelayFunction(final GlobalNet globalNet,
                                  final HalfOperator halfOp,
                                  List<FunctionTerm> delayFunction) {
        if (useEqualityConstraints) {
            final Pair/*<GlobalNet,HalfOperator>*/ k =
                new Pair/*<GlobalNet,HalfOperator>*/(globalNet, halfOp);
            String delayVar = (String) delayMap.get(k);
            if (delayVar == null) {
                delayVar = "D" + delayMap.size();
                delayMap.put(k, delayVar);
                listEqualityVariableNames.add(delayVar);

                List<FunctionTerm> equalityConstraint
                    = new ArrayList<FunctionTerm>();
                final FunctionTerm constraintTerm = new FunctionTerm();
                constraintTerm.type = FunctionTerm.Type.CONSTRAINT_EQUALS;
                constraintTerm.variableName_1 = delayVar;
                equalityConstraint.add(constraintTerm);
                globalNet.getDelayFunction(halfOp, equalityConstraint);
                // add constant term so simplifier is happy
                final FunctionTerm constantTerm = new FunctionTerm();
                constantTerm.type = FunctionTerm.Type.CONSTANT;
                constantTerm.coefficient = 0.0;
                equalityConstraint.add(constantTerm);

                listFunctions.add(equalityConstraint);
            }

            // TODO: save memory by reusing the same term here.
            final FunctionTerm term = new FunctionTerm();
            term.type = FunctionTerm.Type.VAR;
            term.variableName_1 = delayVar;
            term.coefficient = 1.0;
            delayFunction.add(term);
        } else {
            globalNet.getDelayFunction(halfOp, delayFunction);
        }
    }


    /**
     * Initialize the sizes of all the transistors
     **/
    public void initializeSize(boolean skipSizing)
    {
        for (Iterator ita = listAllSubtypes.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();
            List/*<HalfOperator>*/ halfOps = cella.getListHalfOperators();

            if((!cella.isFixedSize()) && (!skipSizing)){
                for (Iterator itb = halfOps.iterator(); itb.hasNext(); ) {
                    HalfOperator hoa = (HalfOperator)itb.next();
                    // FIXME: This is a horrible
                    // Set the size twice so that both previousSize and
                    // currentSize are pi microns.
                    hoa.updateSize(0.314159265E-6);
                    hoa.updateSize(0.314159265E-6);
                }
            }
            else{
                if(DebugOption.printLevel <= 3){
                    System.out.println("Fixed-size cell found:");
                    System.out.println(cella.toString());
                }

                for (Iterator itb = halfOps.iterator(); itb.hasNext(); ) {
                    HalfOperator hoa = (HalfOperator)itb.next();
                    hoa.initializeSize();
                }
            }

            cella.fixVariables();
        }
    }


    void resetWidth() {
        for (Iterator ita = listAllSubtypes.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();
            if (!cella.isFixedSize()) {
                List/*<HalfOperator>*/ halfOps = cella.getListHalfOperators();
                for (Iterator itb = halfOps.iterator(); itb.hasNext(); ) {
                    HalfOperator hoa = (HalfOperator)itb.next();
                    hoa.resetWidth();
                }
            }
        }
    }

    /** 
     * Delete those subtypes without transistors.  Clears
     * <code>listAllSizingSubtypes</code> and adds cells from
     * <code>listAllSubtypes</code> with either paths (if
     * <code>NODE_BASED_SIZING_METHOD</code>) or transistors (if
     * <code>PATH_BASED_SIZING_METHOD</code>).
     **/
    public void reduceSubtypes()
    {
        listAllSizingSubtypes.clear();

        if(toolOptionSizingMethod == NODE_BASED_SIZING_METHOD){
            for (Iterator ita = listAllSubtypes.iterator(); ita.hasNext(); ) {
                CellType sta = (CellType)ita.next();
                if(sta.transistors.isEmpty()){ // no transistors in this sub-type
                    if (! JautoUI.quiet) {
                        System.out.println("************** Removed ****************");
                        System.out.println(sta.toString());
                    }
                }
                else{
                    listAllSizingSubtypes.add(sta);
                }
            }
        }

        if(toolOptionSizingMethod == PATH_BASED_SIZING_METHOD){
            for (Iterator ita = listAllSubtypes.iterator(); ita.hasNext(); ) {
                CellType sta = (CellType)ita.next();
                if(!sta.hasPaths()){ // there are no paths in this sub-type
                    if (! JautoUI.quiet) {
                        System.out.println("************** Removed ****************");
                        System.out.println(sta.toString());
                    }
                }
                else{
                    listAllSizingSubtypes.add(sta);
                }
            }
        }
    }


    // Notes for the future:
    // 1. Size cells near primary outputs first.
    // 2. Size cells with most instances first.

    public void partitionSubtypes()
    {
        for (Iterator ita = listAllSizingSubtypes.iterator(); ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();

            sta.generateSizingHalfOperators();
        }


        List/*<List<CellType>>*/ cellTypeSharings =
            new ArrayList/*<List<CellType>>*/();

        for (Iterator ita = listAllSizingSubtypes.iterator(); ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();

            boolean shared = false;
            List/*<CellType>*/ cellTypesSharedWith = null;

            for (Iterator itc = cellTypeSharings.iterator();
                 itc.hasNext(); ) {
                List/*<CellType>*/ lstc = (List)itc.next();

                for (Iterator itd = lstc.iterator(); itd.hasNext(); ) {
                    CellType stb = (CellType)itd.next();

                    if(sta.isSharingSizingHalfOperatorsWith(stb)){
                        if(!shared){
                            if(DebugOption.printLevel <= 3){
                                System.out.println("Note: Sizing half-operator sharing between cell-types found.");
                            }

                            shared = true;

                            cellTypesSharedWith = lstc;
                            lstc.add(sta);
                        }
                        else{
                            if(DebugOption.printLevel <= 3){
                                System.out.println("Note: Complex half-operator sharing between cell-types found.");
                            }

                            cellTypesSharedWith.addAll(lstc);
                            itc.remove();
                        }

                        break;
                    }
                }
            }

            if(!shared){
                List/*<CellType>*/ newGroup = new ArrayList/*<CellType>*/();
                newGroup.add(sta);
                cellTypeSharings.add(newGroup);
            }
        }


        listAllGroupedSubtypes.clear();

        int numCellsInGroup = 0;
        List/*<CellType>*/ group = new ArrayList/*<CellType>*/();
        for (Iterator itc = cellTypeSharings.iterator(); itc.hasNext(); ) {
            List/*<CellType>*/ lstc = (List)itc.next();

            // TODO: get rid of numCellsInGroup if this assertion holds
            assert numCellsInGroup == group.size();
            assert numCellsInGroup != 0 || group.isEmpty();
            group.addAll(lstc);


            numCellsInGroup += lstc.size();
            if (numCellsInGroup >= toolOptionMaxSubtypesPerGroup) {
                listAllGroupedSubtypes.add(group);
                numCellsInGroup = 0;
                group = new ArrayList/*<CellType>*/();
            }
        }

        assert numCellsInGroup == group.size();
        if (numCellsInGroup != 0)
            listAllGroupedSubtypes.add(group);

        if(DebugOption.printLevel <= 3){
            System.out.println("Subtype partitioning result:");
            for (Iterator ita = listAllGroupedSubtypes.iterator(); ita.hasNext(); ){
                System.out.println("\tGroup:");

                List/*<CellType>*/ lstc = (List)ita.next();

                for (Iterator itb = lstc.iterator(); itb.hasNext(); ){
                    CellType sta = (CellType)itb.next();
                    System.out.println("\t\tCell: " + sta.typeName);
                }

            }
        }

        /**
         * Removing groups with all fixed-size cells
         */

        for (Iterator ita = listAllGroupedSubtypes.iterator(); ita.hasNext(); ) {
            List/*<CellType>*/ lstc = (List)ita.next();

            boolean isAllFixed = true;

            cellsInGroupLoop:
            for (Iterator itb = lstc.iterator(); itb.hasNext(); ){
                CellType sta = (CellType)itb.next();

                for (Iterator itc = sta.setSizingHalfOperators.iterator(); itc.hasNext(); ){
                    HalfOperator hoa = (HalfOperator)itc.next();
                    if(!hoa.subType.isFixedSize()){
                        isAllFixed = false;
                        break cellsInGroupLoop;
                    }
                }

                assert isAllFixed;
                if(DebugOption.printLevel <= 3){
                    System.out.println("Note: Found fixed-size cell: " + sta.typeName);
                }
            }

            if(isAllFixed){
                ita.remove();

                if(DebugOption.printLevel <= 3){
                    System.out.println("Note: Group with all fixed-size cells removed.");
                }
            }
        }


        if(DebugOption.printLevel <= 3){
            System.out.println("Subtype partitioning result after removing groups with all fixed-size cells:");
            for (Iterator ita = listAllGroupedSubtypes.iterator(); ita.hasNext(); ){
                System.out.println("\tGroup:");

                List/*<CellType>*/ lstc = (List)ita.next();

                for (Iterator itb = lstc.iterator(); itb.hasNext(); ) {
                    CellType sta = (CellType)itb.next();
                    System.out.println("\t\tCell: " + sta.typeName);
                }

            }
        }


        /**
         * Look for all fixed-size leaf-cells
         **/

        listAllFixedSubtypes.clear();
        for (Iterator ita = listAllSizingSubtypes.iterator(); ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();

            boolean isAllFixed = true;

            for (Iterator itb = sta.setSizingHalfOperators.iterator(); itb.hasNext(); ){
                HalfOperator hoa = (HalfOperator)itb.next();
                if(!hoa.subType.isFixedSize()){
                    isAllFixed = false;
                    break;
                }
            }

            if(isAllFixed){
                listAllFixedSubtypes.add(sta);
            }
        }

        if(DebugOption.printLevel <= 3){
            System.out.println("NOTE: Number of cells in the list of fixed-size subtypes: " + listAllFixedSubtypes.size());
        }

        JautoUI.dumpSubtypePartitionInformation(synthesizedDesign,
                                                listAllGroupedSubtypes,
                                                listAllFixedSubtypes,
                                                outputDirectoryName);
    }
}
