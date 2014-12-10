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

package com.avlsi.file.common;

import com.avlsi.test.AbstractTestCase;

/**
 * Test case for HierName.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class TestHierName extends AbstractTestCase {

    public void testAppend(final String n1, final String n2, final char sep)
        throws Throwable {
        final String expectedResult = n1 + sep + n2;

        final HierName hn1 = HierName.makeHierName(n1, sep);
        final HierName hn2 = HierName.makeHierName(n2, sep);
        final HierName hnRes = HierName.append(hn1, hn2);

        assertTrue(expectedResult.equals(hnRes.getAsString(sep)));
    }

    public void testAppend() throws Throwable {
        testAppend("a", "d", '.');
        testAppend("a.b", "d", '.');
        testAppend("a.b.c", "d", '.');

        testAppend("a", "d", '.');
        testAppend("a", "d.e", '.');
        testAppend("a", "d.e.f", '.');

        testAppend("a.b.c", "d.e.f", '.');
    }

    public void test() throws Throwable {
        testAppend();
    }

    public static void main(String[] args) {
        AbstractTestCase.testOne(new TestHierName());
    }
}
