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

import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.util.text.NumberFormatter;


/** specialized TransistorSizingTool using Conjugate Gradient Solver **/
public class CGTransistorSizingTool extends TransistorSizingTool {

    private long minEndTime;
    private long maxEndTime;
    
    /** constructor **/
    public CGTransistorSizingTool( final long minEndTime,
                                   final long maxEndTime,
                                   final boolean useEqualityConstraints) {
        super(useEqualityConstraints);
        this.minEndTime = minEndTime;
        this.maxEndTime = maxEndTime;
    }

    void groupVariables() {
        listGroupedVariableNames.clear();
        for (Iterator ita = listAllGroupedSubtypes.iterator(); ita.hasNext(); ) {
            List/*<CellType>*/ lsta = (List)ita.next();

            List/*<String>*/ lstb = new ArrayList/*<String>*/();

            Set/*<HalfOperator>*/ setc =
                new LinkedHashSet/*<HalfOperator>*/();
            for (Iterator itb = lsta.iterator(); itb.hasNext(); ) {
                CellType cella = (CellType)itb.next();
                if(cella.getListHalfOperators().size() > 0){
                    setc.addAll(cella.getListHalfOperators());
                }
                else{
                    setc.addAll(cella.setSizingHalfOperators);
                }
            }

            for (Iterator itb = setc.iterator(); itb.hasNext(); ) {
                HalfOperator hoa = (HalfOperator)itb.next();

                if(!hoa.isVariableFixed()){
                    lstb.add(hoa.getVariableName());
                }
            }

            listGroupedVariableNames.add(lstb);
        }
    }

    void generateObjectiveFunction(Set/*<CellType>*/ setc) {
        for (Iterator ita = setc.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();

            // create new FunctionTerm list for this cell
            List/*<FunctionTerm>*/ functiona =
                new ArrayList/*<FunctionTerm>*/();
            FunctionTerm terma = new FunctionTerm();
            terma.type = FunctionTerm.Type.OBJECTIVE; // first term indicates OBJECTIVE
            functiona.add(terma);

            int instCount = cella.getPhysicalInstanceCount();

            assert cella.getLevel() == 0
                : "Invalid cell type, must be leaf cell.";

            List/*<HalfOperator>*/ lsta = cella.getListHalfOperators(); // get list of half-operators

            Set/*<NetGraph.NetEdge>*/ setd =
                new LinkedHashSet/*<NetGraph.NetEdge>*/();
            // iterator of list of half-operators
            for (Iterator itb = lsta.iterator(); itb.hasNext(); ) {
                HalfOperator hoa = (HalfOperator)itb.next(); // get next half-opeartor 
                String sta = hoa.getVariableName(); // get the variable name of the half-operator
                boolean isFixed = hoa.isVariableFixed(); // to see if the variable is fixed

                if (!isFixed) {
                    // a "real" variable and will be updated

                    ArrayList/*<HalfOperator>*/ lstc =
                        (ArrayList) mapVarNameToHO.get(sta);
                    if (lstc != null) {
                        lstc.add(hoa);
                    }
                    else{
                        lstc = new ArrayList/*<HalfOperator>*/();
                        lstc.add(hoa);
                        mapVarNameToHO.put(sta, lstc);

                        // FIXME: remove hardcoded constant
                        Double da = new Double(0.48E-6);
                        mapVarNameToSize.put(sta, da);
                        listVariableNames.add(sta);
                    }

                }

                double ho_size = hoa.getCurrentSize(); // get the current "effective size" of the half-operator
                Set/*<NetGraph.NetEdge>*/ seta = hoa.transistors; // get list of transistors
                double f = 0.0;
                // iterator of list of transistors
                for (Iterator itc = seta.iterator(); itc.hasNext(); ) {
                    NetGraph.NetEdge trana = (NetGraph.NetEdge)itc.next(); // get next transistor
                    if(!setd.contains(trana)){
                        setd.add(trana);
                        f += trana.size; 
                    }
                    /* NOTE!!! 
                        The size of the transistor is defined as a normalized real number
                        with respect to the size of the half-operator
                    */
                }

                // add the new term to objective function
                functiona.add(newObjectiveTerm(sta,f,instCount,isFixed,hoa));
            }

            // add a zero constant term at the end to make the function simplifier happy
            final FunctionTerm constantTerm = new FunctionTerm();
            constantTerm.type = FunctionTerm.Type.CONSTANT;
            constantTerm.coefficient = 0.0;
            functiona.add(constantTerm);

            listFunctions.add(functiona);
            // Done with objective function generation
        }
    }

    String runSolver(int iterationNumber, int groupNumber) {
        if(DebugOption.printLevel <= 0){
            System.out.println("Current step: executing solve");
        }
        // execute built in CG solver
        JautoSolver solver = new JautoSolver();
        solver.solve(listFunctions,
                     listVariableNames,
                     listVariableLowerBounds,
                     listVariableUpperBounds,
                     listVariableStartValues,
                     mapVarNameToSize,
                     minEndTime,maxEndTime);
        resetWidth();

        // Update sizes
        int l = listVariableNames.size();
        for(int i=0;i<l;++i){
            String sta = (String)listVariableNames.get(i);
            Double da = (Double)mapVarNameToSize.get(sta);

            double f = da.doubleValue();

            System.out.println("Variable name: " + sta + "\t\tValue: " + f);

            List/*<HalfOperator>*/ lstc =
                (ArrayList)mapVarNameToHO.get(sta);
            for (Iterator ita = lstc.iterator(); ita.hasNext(); ) {
                HalfOperator hoa = (HalfOperator)ita.next();
                hoa.updateSize(hoa.getSizeForConductance(f));
            }
        }
        // Done with update sizes

        return "Done";
    }

    /**
     * Main sizing flow
     **/
    public String sizeTransistors()
    {
        String                  summary = "";

        double targetTau = getOptionUnitDelay();

        initializeSize(false);

        assignVariableNames();

        reduceSubtypes();

        partitionSubtypes();

        int sweepIndex;
        if(getOptionInternalSweep()){
            sweepIndex = getOptionInternalSweepNumber();
        }
        else{
            sweepIndex = 1;
        }

        double sweepStep = getOptionInternalSweepStep();

        while(sweepIndex > 0){
            double sweepTau = targetTau + sweepStep * (sweepIndex - 1);
            setOptionUnitDelay(sweepTau);

            String result = runOptimization(listAllSizingSubtypes, 0, 0);

            if (getOptionSizeStaticizer()) {
                SizeStaticizers.sizeStaticizers(this);
            }

            summary += JautoUI.reportSizingResults(this);

            sweepIndex -= 1;
        }

        return summary;
    }
}
