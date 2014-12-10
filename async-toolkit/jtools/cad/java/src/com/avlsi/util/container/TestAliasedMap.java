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
import com.avlsi.util.debug.Debug;

/**
 * Test of AliasedMap.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class TestAliasedMap extends AbstractTestCase {

    private static final class MyMergeFunction
        implements AliasedMap.MergeFunction
    {
        public Object merge(final Object oldValue, final Object newValue)
        {
            final Integer i1 = (Integer) oldValue;
            final Integer i2 = (Integer) newValue;

            if (i1 == null)
                return i2;
            else if (i2 == null)
                return i1;
            else
                return new Integer(i1.intValue() + i2.intValue());
        }
    }

    private static final class MyComparator implements Comparator {
        public int compare(final Object o1, final Object o2) {
            final String s1 = (String) o1;
            final String s2 = (String) o2;

            return s1.compareTo(s2);
        }
    }

    public void test() throws Throwable {
        final AliasedMap asm
            = new AliasedMap(new MyMergeFunction(), new MyComparator());

        final String sone = "one";
        final String stwo = "two";
        final String sthree = "three";
        final String sfour = "four";

        final Integer ione = new Integer(1);
        final Integer itwo = new Integer(2);
        final Integer ithree = new Integer(3);
        final Integer ifour = new Integer(4);

        // test equivalencing bare keys
        asm.makeEquivalent(sone, stwo);
        Debug.assertTrue(asm.equivalent(sone, stwo));
        Debug.assertTrue(asm.getValue(sone) == null);
        Debug.assertTrue(asm.getValue(stwo) == null);

        // now give two some data
        asm.addData(stwo, ithree);
        Debug.assertTrue(asm.getValue(sone) == ithree);
        Debug.assertTrue(asm.getValue(stwo) == ithree);

        // give three some data
        asm.addData(sthree, ithree);
        Debug.assertTrue(asm.getValue(sthree) == ithree);

        // give four some data
        asm.addData(sfour, ifour);
        Debug.assertTrue(asm.getValue(sfour) == ifour);

        // merge two and three
        asm.makeEquivalent(stwo, sthree);

        // check that one, two and three are together
        final Object o = asm.getValue(sone);
        Debug.assertTrue(asm.getValue(stwo) == o);
        Debug.assertTrue(asm.getValue(sthree) == o);
        Debug.assertTrue(o.equals(new Integer(6)));

        // check that four is still alone
        Debug.assertTrue(asm.getValue(sfour) == ifour);

        // check getAliases
        int c = 0;
        final Iterator i4 = asm.getAliases(sfour);
        while (i4.hasNext()) {
            ++c;
            i4.next();
        }

        Debug.assertTrue(c == 1);

        c = 0;
        final Iterator i1 = asm.getAliases(sone);
        while (i1.hasNext()) {
            ++c;
            i1.next();
        }

        Debug.assertTrue(c == 3);
    }

    public static void main(String[] args)
    {
        AbstractTestCase.testOne(new TestAliasedMap());
    }
}
