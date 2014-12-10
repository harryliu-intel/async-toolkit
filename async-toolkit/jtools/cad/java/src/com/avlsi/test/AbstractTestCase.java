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

package com.avlsi.test;

/**
 * Base class for test cases. All test cases should extend this class.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public abstract class AbstractTestCase {

    /**
     * Asserts a condition, throwing TestFailedError if it is not true.
     * 
     * @throws TestFailedError  If <code>!cond</code>.
     **/
    protected void assertTrue(final boolean cond) {
        if (!cond)
            throw new TestFailedError();
    }

    /**
     * Asserts a condition, throwing TestFailedError if it is not true.
     * 
     * @throws TestFailedError  If <code>!cond</code>.
     **/
    protected void assertTrue(final boolean cond, final String message) {
        if (!cond)
            throw new TestFailedError(message);
    }

    /**
     * Run the test case.
     *
     * @throws TestFailedError  If the test fails.
     * @throws Throwable  Who knows what could go wrong.
     **/
    public abstract void test() throws Throwable;

    /**
     * Runs one test case, printing useful info.
     **/
    public static void testOne(final AbstractTestCase tc) {
        try {
            tc.test();
            System.err.println("PASS");
        } catch (TestFailedError t) {
            System.err.println("FAIL: " + t);
            t.printStackTrace();
        } catch (Throwable t) {
            System.err.println("Uncaught throwable: " + t);
            t.printStackTrace();
        }
    }

}
