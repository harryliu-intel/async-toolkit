package com.avlsi.util.math;

import java.util.*;
import java.math.*;
import java.io.*;

/**
 * 1-D quadratic interpolatation with linear extrapolation.  Provides
 * both value and gradient.  Used initially for ASTA to provide a
 * differentiable table model for delays and slews.
 **/
public class LinearInterpolation {
    private final double[] knots;
    private final double[] values;

    /** Constructor **/
    public LinearInterpolation(final double[] x, final double[] y) {
        if (x.length != y.length) {
            throw new IllegalArgumentException("x has length " + x.length +
                                               " y has length " + y.length);
        }
        if (x.length < 2) {
            throw new IllegalArgumentException("There must be at least 2 " +
                                               "data points: " + x.length);
        }
        knots  = x;
        values = y;
    }

    /** evaluate the Nth derivative at x **/
    public double evaluate(final double x, int deriv) {
        // find closest knot i0
        int i,i0,i1;
        double x0,x1,x2,y0,y1,y2,a,b,c,y;
        if (x<knots[0]) { i0=0; i1=1; }
        else if (x>knots[knots.length-1]) { i0 = knots.length-2; i1=knots.length-1; }
        else {
            for (i=0; i+1<knots.length-1; i++)
                if (x>=knots[i] && x<knots[i+1]) break;
            i0 = i;
            i1 = i+1;
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
        final LinearInterpolation qs = new LinearInterpolation(x, y);
        final double first = qs.knots[0] - 1;
        final double last = qs.knots[qs.knots.length-1] + 1;
        for (double t = first; t <= last; t += 0.01) {
            System.out.printf("%f %f\n", t, qs.value(t));
        }
    }
}
