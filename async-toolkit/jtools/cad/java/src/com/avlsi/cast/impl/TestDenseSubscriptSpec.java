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

package com.avlsi.cast.impl;

import java.io.PrintWriter;
import java.io.StringWriter;

import com.avlsi.test.AbstractTestCase;

/**
 * Test case for DenseSubscriptSpec
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class TestDenseSubscriptSpec extends AbstractTestCase {
    public void test() {
        // array[a..b,c..d,e..f]
        // Make an array a[1..3,-3..0,10..11]
        // this has 3 * 4 * 2 = 24 elements

        // we will go manually through the order [1,-3,10], [1,-3,11], ...
        // and ensure that the positions are correct.

        final int a = 1;
        final int b = 30;
        final int c = -30;
        final int d = -30;
        final int e = 10;
        final int f = 110;

        final DenseSubscriptSpec spec = new DenseSubscriptSpec(new Range[]{
            new Range(a, b), new Range(c, d), new Range(e, f)
        });

        int n = 0;

        for (int i = a; i <= b; ++i)
            for (int j = c; j <= d; ++j)
                for (int k = e; k <= f; ++k) {
                    final int[] idx = new int[]{i, j, k};
                    final int pos = spec.positionOf(idx);

                    if (pos != n) {
                        final StringWriter sw = new StringWriter();
                        final PrintWriter ps = new PrintWriter(sw);

                        ps.println("Error for " + n);
                        ps.println("got " + pos);
                        for (int s = 0; s < 3; ++s)
                            ps.print(idx[s] + " ");
                        ps.println();

                        assertTrue(false, sw.toString());
                    }

                    final int[] inverseIdx = spec.indexOf(n);

                    boolean err = false;
                    for (int s = 0; s < 3; ++s)
                        if (inverseIdx[s] != idx[s])
                            err = true;

                    if (err) {
                        final StringWriter sw = new StringWriter();
                        final PrintWriter ps = new PrintWriter(sw);

                        ps.println("bad inverse");
                        ps.print("got: ");
                        for (int s = 0; s < 3; ++s)
                            ps.print(inverseIdx[s] + " ");
                        ps.println();
                        ps.print("wanted: ");
                        for (int s = 0; s < 3; ++s)
                            ps.print(idx[s] + " ");
                        ps.println();

                        assertTrue(false, sw.toString());
                    }

                    ++n;
                }
    }

    public static void main(String[] args) {
        AbstractTestCase.testOne(new TestDenseSubscriptSpec());
    }

}
