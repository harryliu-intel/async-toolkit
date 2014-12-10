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
 * Run all of our test cases.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class TestEverything {

    public static void main(String[] args) {

        final AbstractTestCase[] tests = new AbstractTestCase[] {
            new com.avlsi.cast.impl.TestDenseSubscriptSpec(),
            new com.avlsi.cell.TestExclusiveNodeSet(),
            new com.avlsi.file.common.TestHierName(),
            new com.avlsi.io.TestPositionStackReader(),
            new com.avlsi.io.TestStreamLexer(),
            new com.avlsi.prs.ProductionRule.TestCElementComplement(),
            new com.avlsi.util.bool.TestBooleans.TestAtomic(),
            new com.avlsi.util.bool.TestBooleans.TestDNFCNF(),
            new com.avlsi.util.bool.TestBooleans.Test3CNF(),
            new com.avlsi.util.bool.TestBooleans.TestDeep(),
            new com.avlsi.util.bool.TestBooleans.TestOrder(),
            new com.avlsi.util.container.TestAlias(),
            new com.avlsi.util.container.TestAliasedMap(),
            new com.avlsi.util.container.TestAliasedSet(),
            new com.avlsi.util.container.TestCollectionUtils(),
            new com.avlsi.util.container.TestMultiSet(),
            new com.avlsi.util.container.TestPriorityQueue(),
            new com.avlsi.util.recalc.TestRecalc(),
            new com.avlsi.tools.tsim.TestChannelBundle(),
            new com.avlsi.tools.tsim.TestChannelOutputBundle()
        };

        int nFail = 0;        // # test failures
        int nError = 0;       // # uncaught throwables
        int nPass = 0;        // # passed tests

        final int nTests = tests.length;

        for (int i = 0; i < tests.length; ++i) {
            try {
                tests[i].test();
                ++nPass;
            } catch (TestFailedError e) {
                System.out.println("FAILED: " + tests[i]);
                System.out.println(e);
                e.printStackTrace();
                ++nFail;
            } catch (Throwable t) {
                System.out.println("ERROR: " + tests[i]);
                System.out.println(t);
                t.printStackTrace();
                ++nError;
            }
        }

        if (nPass == nTests) {
            System.out.println("All " + nTests + " tests passed!");
        } else {
            System.out.println();
            System.out.println(nTests + " tests");
            System.out.println(nPass + " passed");
            System.out.println(nFail + " failed");
            System.out.println(nError + " uncaught throwables");
        }
    }
}
