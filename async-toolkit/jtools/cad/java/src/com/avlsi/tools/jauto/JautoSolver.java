/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.*;
import java.util.*;

import com.avlsi.util.text.NumberFormatter;

/**
 * After getting fed up with the crappiness of Lancelot and the
 * growing complexity of the external C solver, I decided to write a
 * built in conjugate gradient solver which takes advantage of all the
 * assumptions about how Jauto formulates the problem.  The
 * minimization algorithm itself is based on the sw/c/lib/minimize.c
 * (originally from Numerical recipes), and the annealing process is
 * right out of solve.  The functions to get the energy and gradient
 * are just written here as concisely and efficiently as possible.
 * All derivatives are analytic.  This new approach is probably orders
 * of magnitude faster and simpler than all the mucking about we've
 * done with external solvers.  Pity Jauto's data structures are so
 * crappy, but they are easily converted into something efficient.
 *
 * Andrew Lines, Oct 3, 2004
 */
public class JautoSolver {
    /** tuning parameters */
    int min_iterations = 100;
    int max_iterations = 1;
    double initial_weight = 10;
    double final_weight = 1e7;
    double step_weight = 10;
    double iteration_ratio = 2;
    double tolerance = 1e-4;
    double min_gradient = 1e-20;
    double TINY = 1e-10;

    /** problem definition */
    int N;                          // number of variables
    List constraints;               // list of Constraint's
    double [] X;                    // values of log variables
    double [] minX;                 // lower bounds and scaling for variables
    double [] maxV;                 // upper bounds for independent variables
    double [] expX;                 // temporary storage for minX * exp(X)
    double [] energyCoefficients;   // coefficients for energy contribution
    double scaleObjective;          // scales objective function energy
    double scaleConstraint;         // scales constraint energy
    double scaleMinMax;             // scales min/max width energy
    Map NameToIndex;                // maps variable name to index
    long minEndTime = -1;           // real time at which to terminate or -1
    long maxEndTime = -1;           // real time at which to terminate or -1

    /** inner class to represet a single constraint */
    private class Constraint {
        /** constant term */
        double c;
        
        /** C*X terms */ 
        double [] cX;
        int    [] iX;
        
        /** C*X/Y terms */
        double [] cXY;
        int    [] iXY_X,iXY_Y;
        
        /** C/Y terms */ 
        double [] cY;
        int    [] iY;

        /**
         * Construct constraint from the silly FunctionTerm list.  The
         * first term just identifies the constraint type.  The last
         * term is the delay budget.
         */
        Constraint(List listConstraint) {
            // pull out delay budget
            int len = listConstraint.size();
            double budget = ((FunctionTerm) listConstraint.get(len-1)).coefficient;

            // count types of terms
            int numX=0, numXY=0, numY=0;
            for (int j=1; j<len-1; j++) {
                FunctionTerm ftm = (FunctionTerm) listConstraint.get(j);
                if      (ftm.type == FunctionTerm.Type.VAR)          numX++;
                else if (ftm.type == FunctionTerm.Type.VAR_OVER_VAR) numXY++;
                else if (ftm.type == FunctionTerm.Type.ONE_OVER_VAR) numY++;
            }

            // allocate arrays
            cX    = new double[numX];
            iX    = new int[numX];
            cXY   = new double[numXY];
            iXY_X = new int[numXY];
            iXY_Y = new int[numXY];
            cY    = new double[numY];
            iY    = new int[numY];

            // process terms
            c = -1; // for normalized delay budget 
            numX = numXY = numY = 0;
            for (int j=1; j<len-1; j++) {
                FunctionTerm ftm = (FunctionTerm) listConstraint.get(j);
                if (ftm.type == FunctionTerm.Type.CONSTANT)
                    c += ftm.coefficient / budget;
                else if (ftm.type == FunctionTerm.Type.VAR) {
                    iX[numX] = getVariableIndex(ftm.variableName_1);
                    cX[numX] = ftm.coefficient / budget;
                    numX++;
                }
                else if (ftm.type == FunctionTerm.Type.VAR_OVER_VAR) {
                    iXY_X[numXY] = getVariableIndex(ftm.variableName_1);
                    iXY_Y[numXY] = getVariableIndex(ftm.variableName_2);
                    cXY[numXY] = ftm.coefficient / budget;
                    numXY++;
                }
                else if (ftm.type == FunctionTerm.Type.ONE_OVER_VAR) {
                    iY[numY] = getVariableIndex(ftm.variableName_1);
                    cY[numY] = ftm.coefficient / budget;
                    numY++;                                                
                }
            }
        }

        /** print constraint for debugging */
        public String toString() {
            String s = "constraint " + NumberFormatter.format(c,3);
            for (int i=0; i<cX.length; i++)
                s += " + " + NumberFormatter.format(cX[i],3) + "*X[" + iX[i] + "]";
            for (int i=0; i<cY.length; i++)
                s += " + " + NumberFormatter.format(cY[i],3) + "/X[" + iY[i] + "]";
            for (int i=0; i<cXY.length; i++)
                s += " + " + NumberFormatter.format(cXY[i],3) + "*X[" + iXY_X[i] + 
                    "]/X[" + iXY_Y[i] + "]"; 
            return s;
        }

        /** get value of this constraint (with expX) */
        double getConstraint() {
            double value = c;
            for (int i=0; i<cX.length; i++)
                value += cX[i] * expX[iX[i]];
            for (int i=0; i<cXY.length; i++)
                value += cXY[i] * expX[iXY_X[i]] / expX[iXY_Y[i]];
            for (int i=0; i<cY.length; i++)
                value += cY[i] / expX[iY[i]];
            return value;
        }

        /** get energy contribution of this constraint */
        double getEnergy() {
            double value = getConstraint();
            if (value<=0) return 0; // constraint satisified
            return scaleConstraint * value * value;
        }
        
        /** accumulate negative gradient for this constraint (with expX) */
        void getDown(double [] down) {
            double value = getConstraint();
            if (value<=0) return; // constraint satisified
            value = 2 * scaleConstraint * value;
            for (int i=0; i<cX.length; i++)
                down[iX[i]] -= value * cX[i] * expX[iX[i]];
            for (int i=0; i<cXY.length; i++) {
                double e = value * cXY[i] * expX[iXY_X[i]] / expX[iXY_Y[i]];
                down[iXY_X[i]] -= e;
                down[iXY_Y[i]] += e;
            }
            for (int i=0; i<cY.length; i++)
                down[iY[i]] += value * cY[i] / expX[iY[i]];
        }
    }
    
    /** evaluate the energy */
    private double getEnergy(double [] v) {
        double E = 0;

        // cache expX, compute objective and min/max width constraint energy
        for (int i=0; i<N; i++) {
            expX[i] = minX[i] * Math.exp(v[i]); // only do once
            E += scaleObjective * energyCoefficients[i] * expX[i];
            if (v[i]<0) E += scaleMinMax * v[i] * v[i];
            if (v[i]>maxV[i]) E += scaleMinMax * (v[i]-maxV[i]) * (v[i]-maxV[i]);
        }
        
        // constraint energy constribution
        for (Iterator s = constraints.iterator(); s.hasNext(); ) {
            Constraint cons = (Constraint) s.next();
            E += cons.getEnergy();
        }
        return E;
    }

    /** evaluate the derivative */
    private void getDown(double [] v, double [] down) {

        // compute energy and min/max width negative gradient
        for (int i=0; i<N; i++) {
            expX[i] = minX[i] * Math.exp(v[i]); // only do once
            down[i] = -scaleObjective * energyCoefficients[i] * expX[i];
            if (v[i]<0) down[i] -= scaleMinMax * 2 * v[i];
            if (v[i]>maxV[i]) down[i] -= scaleMinMax * 2 * (v[i]-maxV[i]);
        }
        
        // accumulate constraint negative gradient
        for (Iterator s = constraints.iterator(); s.hasNext(); ) {
            Constraint cons = (Constraint) s.next();
            cons.getDown(down);
        }

        // variable upper bound energy contributon

    }

    private class EnergyFunction implements ConjugateGradientSolver.Function {
        /** evaluate the energy */
        public double getValue(double [] v) {
            return getEnergy(v);
        }

        /** evaluate the derivative */
        public void getNegativeGradient(double [] v, double [] down) {
            getDown(v, down);
        }
    }

    /** Misnamed: Evaluates continue condition, not stop condition. **/
    private class TimedStop implements ConjugateGradientSolver.StopCondition {
        private int iterations = 0;
        private final double min_magsqr = min_gradient * min_gradient;
        private final long start_time = new Date().getTime();
        public boolean evaluate(boolean big_step, double mag) {
            ++iterations;
            long time = new Date().getTime() - start_time;
            if (!((maxEndTime<0) || (time<maxEndTime)))
                System.err.println("WARNING: max-solve-hours reached before convergence\n");
            return ((maxEndTime<0) || (time<maxEndTime)) && // max-solve-hours
                (mag >= min_magsqr) &&                      // gradient nonzero
                (big_step &&                                // tolerance
                 (iterations <= max_iterations) ||          // iterations
                 ((minEndTime>=0) && (time<minEndTime)));   // min-solve-hours
        }
    }

    /** get variable index from silly string name */
    int getVariableIndex(String variableName) {
        Integer index = (Integer) NameToIndex.get(variableName);
        return index.intValue();
    }

    /** anneal with given constraint weight */
    private void anneal(double weight,
                        ConjugateGradientSolver solver,
                        ConjugateGradientSolver.StopCondition stop) {
        scaleConstraint = weight / constraints.size();
        scaleMinMax = weight / N;
        int steps = solver.conjugateGradientDescent(X,tolerance,stop);
        if (! JautoUI.quiet) {
            double oldO = scaleObjective;
            double oldC = scaleConstraint;
            double oldM = scaleMinMax;
            scaleObjective = 1;
            scaleConstraint = 0;
            scaleMinMax = 0;
            double OE = getEnergy(X);
            scaleObjective = 0;
            scaleConstraint = 1.0/constraints.size();
            double CE = getEnergy(X);
            scaleConstraint = 0;
            scaleMinMax = 1.0/N;
            double ME = getEnergy(X);
            scaleObjective  = oldO;
            scaleConstraint = oldC;
            scaleMinMax     = oldM;
            System.err.println("Solved N=" + N + 
                               " M=" + constraints.size() + 
                               " Steps=" + steps + 
                               " Weight=" + NumberFormatter.format(weight,3) +
                               " Energy=" + NumberFormatter.format(OE,3) +
                               " Constraint=" + NumberFormatter.format(CE,3) +
                               " MinMax=" + NumberFormatter.format(ME,3));
    }
    }

    /** Main solving function */
    public void solve(List listFunctions,
                      List listVariableNames,
                      List listVariableLowerBounds,
                      List listVariableUpperBounds,
                      List listVariableStartValues,
                      Map  resultMap,
                      long minEndTime,
                      long maxEndTime) {
        // simplify functions
        List functions = FunctionSimplifier.simplify(listFunctions);

        // initialize independent variables data structures
        N = listVariableNames.size();
        X = new double[N];
        minX = new double[N];
        maxV = new double[N];
        expX = new double[N];
        energyCoefficients = new double[N];
        NameToIndex = new HashMap(); // damn silly
        for (int i=0; i<N; i++) {
            String name = (String) listVariableNames.get(i);
            double minx = ((Double) listVariableLowerBounds.get(i)).doubleValue();
            double maxx = ((Double) listVariableUpperBounds.get(i)).doubleValue();
            double x = ((Double) listVariableStartValues.get(i)).doubleValue();
            NameToIndex.put(name,new Integer(i));
            minX[i] = minx;
            maxV[i] = Math.log(maxx/minx);
            if (x<minx) X[i] = 0;
            else X[i] = Math.log(x/minx);
            energyCoefficients[i] = 0;
        }

        // slog through functions to create constaints add energyCoefficients
        constraints = new ArrayList();
        int j, k = 0;
        for (Iterator ita = functions.iterator(); ita.hasNext(); ) {
            List lsta = (List) ita.next();
            j = lsta.size();
            FunctionTerm ftma = (FunctionTerm) lsta.get(0);
            
            // handle objective terms
            if (ftma.type == FunctionTerm.Type.OBJECTIVE) {
                for (int i=1; i<j; i++) {
                    FunctionTerm ftmb = (FunctionTerm) lsta.get(i);
                    if (ftmb.type == FunctionTerm.Type.VAR) {
                        int num = getVariableIndex(ftmb.variableName_1);
                        energyCoefficients[num] += ftmb.coefficient;
                    }
                }
            }
            
            // handle constraint terms
            else if (ftma.type == FunctionTerm.Type.CONSTRAINT_LESS_THAN) {
                Constraint cons = new Constraint(lsta);
                constraints.add(cons);
            }
        }

        // compute minimum energy for normalization purposes
        double minE = 0;
        for (int i=0; i<N; i++) minE += energyCoefficients[i] * minX[i];
        scaleObjective = 1/minE;

        // parameters
        max_iterations = min_iterations + (int) (N*iteration_ratio);
        this.maxEndTime = maxEndTime;

        // create solver
        ConjugateGradientSolver solver =
            new ConjugateGradientSolver(new EnergyFunction());

        // anneal
        for (double w = initial_weight; w <= final_weight; w *= step_weight)
            anneal(w, solver, new TimedStop());

        // final optimization pass
        if (minEndTime>=0) {
            tolerance = TINY;
            this.minEndTime = minEndTime;
            anneal(final_weight, solver, new TimedStop());
        }

        // export results
        for (int i=0; i<N; i++) {
            String name = (String) listVariableNames.get(i);
            expX[i] = minX[i] * Math.exp(X[i]);
            resultMap.put(name, new Double(expX[i]));
        }
    }
}
