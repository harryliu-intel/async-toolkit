/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.util.logging.Logger;
import com.avlsi.tools.jauto.JautoUI;

/**
 * Conjugate gradient descent solver.  This code was originally written because
 * Andrew was "getting fed up with the crappiness of Lancelot and the growing
 * complexity of the external C solver."  The minimization algorithm is from
 * <tt>sw/c/lib/minimize.c</tt> which was in turn based on Numerical Recipes.
 *
 * @author Andrew Lines
 * @author Harry Liu
 **/
public class ConjugateGradientSolver {
    /**
     * Logging object.
     **/
    private static final Logger logger =
        Logger.getLogger("com.avlsi.tools.jauto.ConjugateGradientSolver");

    /**
     * Interface representing a differentiable, scalar-valued, vector function.
     **/
    public interface Function {
        /**
         * Evaluate the function at the specified vector.
         *
         * @param v the vector at which to evalute the function
         * @return the value at the specified point
         **/
        double getValue(double[] v);
        
        /**
         * Calculate the negative of the gradient at the specified vector.
         *
         * @param v the vector at which to evalute the gradient
         * @param negativeGradient the negative gradient
         **/
        void getNegativeGradient(double[] v, double[] negativeGradient);
    }

    /**
     * Interface representing a stop condition for the inner loop of the
     * conjugate gradient descent solver.
     **/
    public interface StopCondition {
        /**
         * Returns <tt>true</tt> if the solver should continue and
         * <tt>false</tt> if it should stop.  This function is called at the
         * beginning of each loop.
         *
         * @param mag the magnitude of the last gradient evaluated
         **/
        boolean evaluate(boolean big_step, double mag);
    }

    /**
     * Rarely changed tunable parameters for the golden section search used by
     * the solver.  To provide different values for the parameters, extend this
     * class and override the appropriate methods.
     **/
    public static class Parameters {
        /**
         * Returns the tolerance as a fraction of the interval bracketing the
         * minimum.
         **/
        public double getTolerance()      { return 1e-4; }

        /**
         * Returns the lower bound on the initial search interval.
         **/
        public double getTiny()           { return 1e-10; }

        /**
         * Returns the upper bound on the number of steps to bracket the
         * minimum.
         **/
        public double getBracketSteps()   { return 100; }

        /**
         * Returns the upper bound on the number of steps.
         **/
        public double getGoldenSteps()    { return 10; }
    }

    /**
     * Numerical constants used for golden section search.
     **/
    private static final double GOLD = 0.5 + Math.sqrt(5) / 2;
    private static final double STEP = 1 - 1 / GOLD;

    /**
     * The function to solve.
     **/
    private final Function func;

    /**
     * Tuning parameters.
     **/
    private final Parameters params;

    /**
     * Used to tune lineMinimum's initial guess.
     **/
    private double initial_interval;

    /**
     * Returns the "energy" of the function.
     *
     * @param v the vector at which to do the evaluation
     * @return the energy of the function
     **/
    protected double getEnergy(double[] v) {
        return func.getValue(v);
    }

    /**
     * Evaluates the derivative of the function.
     *
     * @param v the vector at which to do the evaluation
     * @param down the accumulator into which to add the negative gradient of
     * the function
     **/
    protected void getDown(double [] v, double [] down) {
        for (int i = 0; i < down.length; ++i) down[i] = 0;
        func.getNegativeGradient(v, down);
    }

    /**
     * Find line minimum with golden section search.  Consult Numerical
     * Recipes, section 10.1.
     **/
    protected double lineMinimum(double [] v, double [] dir, double [] vv) {
        double a,b,c,t,fa,fb,fc,ft,quit_interval;
        int i,steps;
        int N = v.length;
        
        // pick two points along dir at 0 and initial_interval
        a = 0;
        fa = getEnergy(v);
        
        b = initial_interval;
        for (i=0; i<N; i++) vv[i] = v[i] + b * dir[i];
        fb = getEnergy(vv);

        // sort them by energy
        if (fb>fa) {
            t = a; a = b; b = t;
            ft = fa; fa = fb; fb = ft;
        }
  
        // attempt to bracket the minimum
        c = a; fc = fa; 
        for (steps=0; steps<params.getBracketSteps(); steps++) {
            c = b + (b - a) * GOLD;
            for (i=0; i<N; i++) vv[i] = v[i] + c * dir[i];
            fc = getEnergy(vv);
            if (fc<fb) {
                a = b; fa = fb;
                b = c; fb = fc;
            }
            else break;
        }
        
        // golden section search for minimum
        quit_interval = params.getTolerance() * Math.abs(a-c);
        for (steps=0; (steps<params.getGoldenSteps()) && (Math.abs(a-c)>quit_interval); steps++) {
            if (Math.abs(b-a)>Math.abs(c-b)) {
                t = b + (a - b) * STEP;
                for (i=0; i<N; i++) vv[i] = v[i] + t * dir[i];
                ft = getEnergy(vv);
                if (ft<=fb) {
                    c = b; fc = fb;
                    b = t; fb = ft;
                }
                else {
                    a = t; fa = ft;
                }
            }
            else  {
                t = b + (c - b) * STEP;
                for (i=0; i<N; i++) vv[i] = v[i] + t * dir[i];
                ft = getEnergy(vv);
                if (ft<fb) {
                    a = b; fa = fb;
                    b = t; fb = ft;
                }
                else {
                    c = t; fc = ft;
                }
            }
        }

        // set v to new minimum at b
        for (i=0; i<N; i++) v[i] = v[i] + b * dir[i];

        // adaptively adjust initial_interval
        b = Math.abs(b);
        initial_interval = 0.75 * initial_interval + 0.25 * b;
        if (initial_interval<params.getTiny()) initial_interval=params.getTiny();

        // return maximum absolute change in any independent variable
        a = 0;
        for (i=0; i<N; i++) if (Math.abs(b*dir[i])>a) a = Math.abs(b*dir[i]);
        return a;
    }

    /**
     * Conjugate gradient descent.  Polak-Ribiere formula.  Resets if it
     * gets stuck.  Consult Numerical Recipes.
     **/
    protected int conjugateGradientDescent(double [] p,
                                           double tolerance,
                                           StopCondition stop) {
        int i;
        int N = p.length;
        double mg,mg1,step,step1,beta,dot;
        
        // allocate scrate vectors
        double [] h = new double[N];
        double [] g = new double[N];
        double [] g1 = new double[N];
        
        // initial setup
        step = 4 * tolerance;
        getDown(p,h);
        for (i=0; i<N; i++) g[i] = h[i];
        mg = 0; for (i=0; i<N; i++) mg += g[i] * g[i];
        if (! JautoUI.quiet)
            logger.info(
                String.format(" 0 E=%.3f dE=%.3f", getEnergy(p), Math.sqrt(mg)));
        
        // main loop 
        for (i=1; stop.evaluate(step>=tolerance,mg); i++) {
            step1 = lineMinimum(p,h,g1);
            step = 0.75*step + 0.25*step1; // exponentially weighted moving average
            getDown(p,g1);
            mg1 = dot = 0;
            for (int j=0; j<N; j++) {
                mg1 += g1[j] * g1[j];
                dot += g[j]  * g1[j];
            }
            beta = (mg1 - dot) / mg;
            if (beta<0) beta = 0;
            for (int j=0; j<N; j++) {
                h[j] = g1[j] + h[j] * beta;
                g[j] = g1[j];
            }
            mg = mg1;
            if (! JautoUI.quiet)
                logger.info(
                    String.format(" %d E=%.3f dE=%.3f Step=%.3f", i, getEnergy(p),
                              Math.sqrt(mg), step1));
        }
        return i-1;
    }

    /**
     * Constructor.
     *
     * @param params tunable parameters
     * @param func the function to solve for
     **/
    public ConjugateGradientSolver(Parameters params, Function func) {
        this.params = params;
        this.func = func;
        this.initial_interval = params.getTiny();
    }

    /**
     * Constructor.  Use default parameters for golden section search.
     *
     * @param func the function to solve for
     **/
    public ConjugateGradientSolver(Function func) {
        this(new Parameters(), func);
    }

    /**
     * Solve.
     *
     * @param X initial guess of the independent variables that will be
     * overwritten by the solution from the solver
     * @param tolerance an upper bound on the maximum absolute change of any
     * independent variable before stopping
     * @param stop additional stop conditions for the solver
     *
     * @return the number of conjugate gradient descent steps
     **/
    public int solve(double[] X, double tolerance, StopCondition stop) {
        return conjugateGradientDescent(X, tolerance, stop);
    }
}
