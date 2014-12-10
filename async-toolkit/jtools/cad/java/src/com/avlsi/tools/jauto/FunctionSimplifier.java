/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.jauto;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

public class FunctionSimplifier
{

    /**
     * This class cannot be instantiated.
     **/
    private FunctionSimplifier() {
        throw new AssertionError();
    }


    // Not yet used.
    public static List/*<List<FunctionTerm>>*/ normalize
        (List/*<List<FunctionTerm>>*/ functions)
    {
        List/*<List<FunctionTerm>>*/ normalizedFunctions =
            new ArrayList/*<List<FunctionTerm>>*/();

        for (Iterator ita = functions.iterator(); ita.hasNext(); ) {
            List/*<FunctionTerm>*/ function = (List) ita.next();
            int numTerms = function.size();

            assert numTerms >= 3
                : "function must contain at least one function term";

            List/*<FunctionTerm>*/ normalizedFunction =
                new ArrayList/*<FunctionTerm>*/(numTerms);

            FunctionTerm ftma = (FunctionTerm) function.get(0);
            if (ftma.type == FunctionTerm.Type.OBJECTIVE) {
                // Objective, do not normalize
                for (Iterator itb = function.iterator(); itb.hasNext(); ) {
                    FunctionTerm ftmb = (FunctionTerm)itb.next();
                    normalizedFunction.add(new FunctionTerm(ftmb));
                }
            }
            else{ // Constraint, normalize to delay budget
                FunctionTerm ftmb =
                    (FunctionTerm) function.get(numTerms - 1);
                assert ftmb.type == FunctionTerm.Type.CONSTANT
                    : "Delay budget must be constant";
                double delayBudget = ftmb.coefficient;
                assert delayBudget != 0 : "Zero delay budget";

                for (int i = 0; i < numTerms; ++i) {
                    ftmb = (FunctionTerm) function.get(i);
                    FunctionTerm ftmc = new FunctionTerm(ftmb);
                    ftmc.coefficient /= delayBudget;
                    normalizedFunction.add(ftmc);
                }
            }

            normalizedFunctions.add(normalizedFunction);
        }

        return normalizedFunctions;
    }


    public static List/*<FunctionTerm>*/ simplifyOne
        (List/*<FunctionTerm>*/ function) {
        int numTerms = function.size();

        assert numTerms >= 3
            : "function must contain at least one function term";

        List/*<FunctionTerm>*/ simplifiedFunction =
            new ArrayList/*<FunctionTerm>*/(numTerms);
        double constantCoeff = 0.0;

        for (int i = 1; i < numTerms - 1; ++i) {
            FunctionTerm ftma = (FunctionTerm) function.get(i);
            int termType = ftma.type;

            assert termType != FunctionTerm.Type.OBJECTIVE &&
                   termType != FunctionTerm.Type.CONSTRAINT_LESS_THAN &&
                   termType != FunctionTerm.Type.CONSTRAINT_EQUALS
                : "Function terms of type OBJECTIVE, CONSTRAINT_LESS_THAN " +
                  "or CONSTRAINT_EQUALS are not allowed in this region of " +
                  "the function";
            if (termType == FunctionTerm.Type.CONSTANT) {
                constantCoeff += ftma.coefficient;
            }
            else{
                sortedCopyInsert(simplifiedFunction, ftma);
            }
        }

        assert ((FunctionTerm) function.get(0)).type ==
                   FunctionTerm.Type.CONSTRAINT_LESS_THAN ||
               ((FunctionTerm) function.get(0)).type ==
                   FunctionTerm.Type.CONSTRAINT_EQUALS ||
               ((FunctionTerm) function.get(0)).type ==
                   FunctionTerm.Type.OBJECTIVE;
        simplifiedFunction
            .add(0, new FunctionTerm((FunctionTerm) function.get(0)));

        assert ((FunctionTerm) function.get(numTerms - 1)).type == 
                   FunctionTerm.Type.CONSTANT;
        FunctionTerm ftmb =
            new FunctionTerm((FunctionTerm) function.get(numTerms-1));
        ftmb.coefficient -= constantCoeff;
        simplifiedFunction.add(ftmb);

        return simplifiedFunction;
    }

    public static List/*<List<FunctionTerm>>*/ simplify
        (List/*<List<FunctionTerm>>*/ functions)
    {
        List/*<List<FunctionTerm>>*/ simplifiedFunctions =
            new ArrayList/*<List<FunctionTerm>>*/(functions.size());
        for (Iterator ita = functions.iterator(); ita.hasNext(); ) {
            List/*<FunctionTerm>*/ function = (List) ita.next();
            List/*<FunctionTerm>*/ simplifiedFunction =
                simplifyOne(function);

            if (simplifiedFunction.size() > 2) {
                // Ignore "constant constraint" introduced by
                // the fixed-size cells
                simplifiedFunctions.add(simplifiedFunction);
            }
            else{
                // REVIEW: What if ignored constraint is unsatisfiable?
                if(DebugOption.printLevel <= 2){
                    System.out.println("Note: Constant constraint thrown out, possibly due to fixed-size cells");
                    System.out.println("Constraint value: " +
                                       ((FunctionTerm) simplifiedFunction
                                            .get(simplifiedFunction
                                                    .size() - 1))
                                        .coefficient);
                }
            }
        }

        return simplifiedFunctions;
    }


    private static void sortedCopyInsert(List/*<FunctionTerm>*/ function,
                                           FunctionTerm ftm1)
    {
        int numTerms = function.size();
        for (int i = 0; i < numTerms; ++i) {
            FunctionTerm ftma = (FunctionTerm) function.get(i);
            final int c = compare(ftma, ftm1);

            if (c == 0) {
                ftma.coefficient += ftm1.coefficient;

                return;
            }

            if (c > 0) {
                function.add(i, new FunctionTerm(ftm1));

                return;
            }
        }

        function.add(new FunctionTerm(ftm1));
    }


    private static int compare(FunctionTerm ftm1, FunctionTerm ftm2)
    {
        int type1 = ftm1.type;
        int type2 = ftm2.type;

        final int c = type1 - type2;
        if (c != 0)
            return c;

        if (type1 == FunctionTerm.Type.CONSTANT) {
            double coef1 = ftm1.coefficient;
            double coef2 = ftm2.coefficient;

            if(coef1 < coef2){
                return -1;
            }

            if(coef1 > coef2){
                return 1;
            }

            return 0;
        }

        String vn11 = ftm1.variableName_1;
        String vn21 = ftm2.variableName_1;

        if (type1 == FunctionTerm.Type.VAR ||
            type1 == FunctionTerm.Type.ONE_OVER_VAR)
            return vn11.compareTo(vn21);

        if (type1 == FunctionTerm.Type.VAR_OVER_VAR) {
            int i = vn11.compareTo(vn21);

            if(i != 0){
                return i;
            }

            String vn12 = ftm1.variableName_2;
            String vn22 = ftm2.variableName_2;
            return vn12.compareTo(vn22);
        }

        throw new AssertionError("Error: Unknown type of function term!");
    }


    public static String getDerivativeString(List/*<FunctionTerm>*/ function,
                                             String var)
    {
        StringBuffer sb = new StringBuffer();
        int maxTerm = function.size() - 1;
        for (int i = 1; i < maxTerm; ++i) {
            FunctionTerm ftma = (FunctionTerm) function.get(i);

            String termDerivative = ftma.getDerivativeString(var);

            if (!termDerivative.equals("0")) {
                sb.append(termDerivative);
                if (i > 1) {
                    // Not the first term
                    sb.append("+ ");
                }
            }
        }

        return sb.toString();
    }

}
