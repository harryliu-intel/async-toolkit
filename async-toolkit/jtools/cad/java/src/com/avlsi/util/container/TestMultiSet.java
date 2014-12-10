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

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import com.avlsi.test.AbstractTestCase;

/**
 * Test of MultiSet
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class TestMultiSet extends AbstractTestCase {
    /**
     * Multi set being tested.
     **/
    private final MultiSet ms = new MultiSet(new NaturalOrderComparator());

    /**
     * Set, with same members as multiset, but no count info.
     **/
    private final Set s = new HashSet();

    /**
     * Add the object to the set, and the multiset.  Check that it is in
     * the multiset.  Check that the set and multiset have the same
     * elements.  Check that the multiset's elements are in order.
     **/
    private void add(final Object obj) {
        ms.add(obj);
        s.add(obj);

        assertTrue(ms.find(obj) != null);

        // check that every set elem is in the multiset
        for (final Iterator i = s.iterator(); i.hasNext(); ) {
            final Object o = i.next();
            assertTrue(ms.find(o) == o);
        }

        // check that every multiset elem is in the set
        // and that the multiset elements are in order
        for (final Iterator i = ms.iterator(); i.hasNext(); ) {
            final Object o = i.next();
            assertTrue(s.contains(o));
        }
    }

    private void dontFind(final Object o) {
        assertTrue(ms.find(o) == null);
    }

    private void doFind(final Object o) {
        assertTrue(ms.find(o).equals(o));
    }

    private void doFindN(final Object o, final int n) {
        int cnt = 0;
        for (final Iterator i = ms.findAll(o); i.hasNext(); ) {
            ++cnt;
            assertTrue(i.next().equals(o));
        }
        assertTrue(cnt == n);
    }

    private void checkSorted() {
        // and that the multiset elements are in order
        Comparable old = null;
        for (final Iterator i = ms.iterator(); i.hasNext(); ) {
            final Object o = i.next();
            assertTrue(s.contains(o));

            if (old != null)
                assertTrue(old.compareTo(o) <= 0);

            old = (Comparable) o;
        }
    }

    public void test() {
        final MultiSet ms = new MultiSet(new NaturalOrderComparator());
        add("f"); dontFind("a");
        add("a");   doFind("f");
        add("e");   doFind("e");
        add("d"); dontFind("w");
        add("c"); dontFind("s");
        add("b");   doFind("d");
        add("b");

        doFindN("b", 2);
        ms.sort();
        checkSorted();
        doFind("c");
    }

    public static void main(String args[]) {
        AbstractTestCase.testOne(new TestMultiSet());
    }

}
