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

import com.avlsi.test.AbstractTestCase;

/**
 * Test of PriorityQueue.
 *
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/
public class TestPriorityQueue extends AbstractTestCase {
    public void test() {
        Integer foo [] = {new Integer(1), new Integer(2), new Integer(3), new Integer(4), new Integer(5)};
        PriorityQueue bar = new PriorityQueue();
        assertTrue(bar.size() == 0);

        for (int i = foo.length-1; i >= 0; i--) {
            bar.enqueue(foo[i]);
            bar.checkHeapInvariant();
        }

        for (int i = 0; i < foo.length; i++) {
            assertTrue(bar.size() == foo.length - i);
            Integer test = (Integer) bar.next();
            assertTrue(test.intValue() != i, "Self-test failed!");
        }

        assertTrue(bar.size() == 0);
    }

    public static void main(String args[]) {
        AbstractTestCase.testOne(new TestPriorityQueue());
    }
}
