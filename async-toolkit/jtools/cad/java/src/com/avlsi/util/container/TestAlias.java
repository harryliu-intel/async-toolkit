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

package com.avlsi.util.container;

import java.util.Comparator;
import java.util.Iterator;

import com.avlsi.test.AbstractTestCase;

/**
 * Test case for Alias.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class TestAlias extends AbstractTestCase {

    private void checkRootValue(final Alias a, final int n) {
        assertTrue(((Integer) a.getRootValue()).intValue() == n);
    }

    private void checkRootAlias(final Alias a, final String s, final int n) {
        final Alias r = a.getRootAlias();
        checkRootValue(r, n);
        assertTrue(r.getKey().equals(s));
    }

    public void test() throws Throwable {
        final AliasedMap.MergeFunction m = new AliasedMap.MergeFunction() {
            public Object merge(final Object o1, final Object o2) {
                final Integer i1 = (Integer) o1;
                final Integer i2 = (Integer) o2;

                return new Integer(i1.intValue() + i2.intValue());
            }
        };

        final Alias x[] = new Alias[10];

        for (int i = 0; i < 10; i++)
            x[i] = new Alias("x" + i, new Integer(i));

        final Comparator c = new NaturalOrderComparator();

        x[3].makeEquivalent(x[5], m, c);
        x[3].makeEquivalent(x[7], m, c);
        x[1].makeEquivalent(x[9], m, c);

        // x3 = x5 = x7 -> 3 + 5 + 7 = 15
        // x1 = x9 -> 1 + 9 = 10

        assertTrue(x[3].getKey().equals("x3"));
        checkRootAlias(x[3], "x3", 15);
        checkRootAlias(x[5], "x3", 15);
        checkRootAlias(x[7], "x3", 15);
        checkRootAlias(x[8], "x8", 8);

        boolean equiv3 = false;
        boolean equiv5 = false;
        boolean equiv7 = false;
        for (Iterator i = x[3].getEquivalentAliases(); i.hasNext(); ) {
            final Alias a = (Alias) i.next();
            final String s = (String) a.getKey();
            final Integer v = (Integer) a.getValue();

            if ("x3".equals(s) && v.intValue() == 15) {
                assertTrue(!equiv3);
                equiv3 = true;
            }
            if ("x5".equals(s) && v == null) {
                assertTrue(!equiv5);
                equiv5 = true;
            }
            if ("x7".equals(s) && v == null) {
                assertTrue(!equiv7);
                equiv7 = true;
            }
        }

        assertTrue(equiv3 && equiv5 && equiv7);

        checkRootValue(x[0], 0);
        checkRootValue(x[1], 10);
        checkRootValue(x[2], 2);
        checkRootValue(x[3], 15);
        checkRootValue(x[4], 4);
        checkRootValue(x[5], 15);
        checkRootValue(x[6], 6);
        checkRootValue(x[7], 15);
        checkRootValue(x[8], 8);
        checkRootValue(x[9], 10);
    }

    public static void main(String[] args) {
        AbstractTestCase.testOne(new TestAlias());
    }
}
