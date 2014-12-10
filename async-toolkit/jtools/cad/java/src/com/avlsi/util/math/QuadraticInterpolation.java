package com.avlsi.util.math;

import java.util.*;
import java.math.*;
import java.io.*;

/**
 * 1-D quadratic interpolatation with linear extrapolation.  Provides
 * both value and gradient.  Used initially for ASTA to provide a
 * differentiable table model for delays and slews.  Not a b-spline at
 * all, just trims corners with a parabolic curve.  Parameterized by
 * alpha, where alpha=0 approaches linear interpolation, and alpha=0.5
 * is maximum smoothing.  Continuous deriviative.
 **/
public class QuadraticInterpolation {
    private final double[] knots;
    private final double[] values;
    private final double alpha0, alpha1;

    /** Constructor **/
    public QuadraticInterpolation(final double[] x, final double[] y, double alpha) {
        if (x.length != y.length) {
            throw new IllegalArgumentException("x has length " + x.length +
                                               " y has length " + y.length);
        }
        if (x.length < 2) {
            throw new IllegalArgumentException("There must be at least 2 " +
                                               "data points: " + x.length);
        }
        if (alpha<0 || alpha>0.5) {
            throw new IllegalArgumentException("Alpha should be >=0 && <=0.5, but is "
                                               + alpha);
        }
        knots  = x;
        values = y;
        alpha1 = alpha;
        alpha0 = 1-alpha; 
    }

    /** evaluate the Nth derivative at x **/
    public double evaluate(final double x, int deriv) {
        // find closest knot i1
        int i,i0,i1,i2;
        double x0,x1,x2,y0,y1,y2,a,b,c,y,s01,s12,x01,x12,y01,y12;
        for (i=0; i+1<knots.length; i++)
            if ((knots[i+1]+knots[i])/2>x) break;
        i1 = i;
        i0 = i1-1;
        i2 = i1+1;
        if      (i1==0) { i0=0; i1=1; } // linear extrapolation
        else if (i1==knots.length-1) {} // linear extrapolation
        else { // quadratic interpolation
            // get coordinates
            x0 = knots[i0];
            x1 = knots[i1];
            x2 = knots[i2];
            y0 = values[i0];
            y1 = values[i1];
            y2 = values[i2];
            // get slopes
            s01 = (y1-y0)/(x1-x0);
            s12 = (y2-y1)/(x2-x1);
            // choose endpoints at uniform distance from x1
            if (x1-x0>x2-x1) {
                x12 = alpha0*x1 + alpha1*x2;
                y12 = alpha0*y1 + alpha1*y2;
                x01 = x1-(x12-x1);
                y01 = y1+s01*(x01-x1);
            } else {
                x01 = alpha0*x1 + alpha1*x0;
                y01 = alpha0*y1 + alpha1*y0;
                x12 = x1+(x1-x01);
                y12 = y1+s12*(x12-x1);
            }
            if       (x>x12) { i0=i1; i1=i2; } // linear interpolation
            else if  (x<x01) {}                // linear interpolation
            else {
                // parabolic fit to a*x*x+b*x+c
                a = (s12-s01)/(x12-x01)/2;
                b = s01 - 2*a*x01;
                c = y01 - a*x01*x01 - b*x01;
                // evaluate
                if      (deriv==0) y = a*x*x + b*x + c;
                else if (deriv==1) y = 2*a*x + b;
                else if (deriv==2) y = 2*a;
                else               y = 0;
                return y;
            }
        }
        // get coordinates
        x0 = knots[i0];
        x1 = knots[i1];
        y0 = values[i0];
        y1 = values[i1];
        // linear fit to a*x+b
        a = (y1-y0)/(x1-x0);
        b = (y1+y0)/2 - a*(x1+x0)/2;
        // evaluate
        if      (deriv==0) y = a*x + b;
        else if (deriv==1) y = a;
        else               y = 0;
        return y;

    }

    /** evaluate function at x **/
    public double value(final double x) {
        return evaluate(x,0);
    }

    /** evaluate first derivative at x **/
    public double gradient(final double x) {
        return evaluate(x,1);
    }

    /** simple test to plot interpolation/extrapolation **/
    public static void main(String[] args) throws IOException {
        // get alpha
        double alpha = 0.5;
        if (args.length>0) alpha = Double.parseDouble(args[0]);

        // read x y points from stdin
        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        ArrayList lines = new ArrayList();
        String line;
        while ((line = br.readLine()) != null) lines.add(line);

        // parse lines
        int size = lines.size();
        double[] x = new double[size];
        double[] y = new double[size];
        Iterator l=lines.iterator();
        int i=0;
        while (l.hasNext()) {
            String [] s = ((String) l.next()).split("\\s+");
            x[i] = Double.parseDouble(s[0]);
            y[i] = Double.parseDouble(s[1]);
            i++;
        }

        // test interpolation/extrapolation
        final QuadraticInterpolation qs = new QuadraticInterpolation(x, y, alpha);
        final double first = qs.knots[0] - 1;
        final double last = qs.knots[qs.knots.length-1] + 1;
        for (double t = first; t <= last; t += 0.01) {
            System.out.printf("%f %f\n", t, qs.value(t));
        }
    }
}
