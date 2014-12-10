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
 * Test of AliasedSet.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class TestAliasedSet extends AbstractTestCase {

    private static final class MyComparator implements Comparator {
        public int compare(final Object o1, final Object o2) {
            final String s1 = (String) o1;
            final String s2 = (String) o2;

            return s1.compareTo(s2);
        }
    }

    public void test() throws Throwable {
        final AliasedSet asm = new AliasedSet(new MyComparator());

        final String sone = "one";
        final String stwo = "two";
        final String sthree = "three";
        final String sfour = "four";

        // test equivalencing
        asm.makeEquivalent(sone, stwo);
        assertTrue(asm.contains(sone));
        assertTrue(asm.contains(stwo));
        assertTrue(asm.areEquivalent(sone, stwo));

        // add three
        asm.add(sthree);
        assertTrue(asm.contains(sthree));

        // add four
        asm.add(sfour);
        assertTrue(asm.contains(sfour));

        // merge two and three
        asm.makeEquivalent(stwo, sthree);

        // check that one, two and three are together
        assertTrue(asm.areEquivalent(sone, stwo));
        assertTrue(asm.areEquivalent(sone, sthree));
        assertTrue(asm.areEquivalent(stwo, sthree));

        // check that four is still alive
        assertTrue(asm.contains(sfour));

        // check getAliases
        int c = 0;
        final Iterator i4 = asm.getAliases(sfour);
        while (i4.hasNext()) {
            ++c;
            i4.next();
        }

        assertTrue(c == 1);

        c = 0;
        final Iterator i1 = asm.getAliases(sone);
        while (i1.hasNext()) {
            ++c;
            i1.next();
        }

        assertTrue(c == 3);
    }

    public static void main(String[] args)
    {
        AbstractTestCase.testOne(new TestAliasedMap());
    }
}
