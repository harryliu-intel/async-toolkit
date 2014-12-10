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

package com.avlsi.util.bool;

import java.util.ArrayList;

import com.avlsi.file.common.HierName;
import com.avlsi.test.AbstractTestCase;
import com.avlsi.test.TestFailedError;

/**
 * Class for testing the BooleanExpression types.
 *
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/

public class TestBooleans {
    public static void main(String [] args) {
        TestAtomic t = new TestAtomic();
        TestDNFCNF t2 = new TestDNFCNF();
        Test3CNF t3 = new Test3CNF();
        TestDeep t4 = new TestDeep();
        TestOrder t5 = new TestOrder();
        AbstractTestCase.testOne(t);
        AbstractTestCase.testOne(t2);
        AbstractTestCase.testOne(t3);
        AbstractTestCase.testOne(t4);
        AbstractTestCase.testOne(t5);
    }

    /**
     * Tests HierNameAtomicBooleanExpressions.
     **/ 
    public static class TestAtomic extends AbstractTestCase {
        public void test() throws TestFailedError, Throwable {
            HierNameAtomicBooleanExpression foo = new
                HierNameAtomicBooleanExpression(true,
                        HierName.makeHierName("a/b/c",'/'));
            assertTrue(foo.equals(foo.negated().negated()));
        }
    }


    /**
     * Tests dnf, cnf conversions.
     **/
    public static class TestDNFCNF extends AbstractTestCase {
        public void test() throws TestFailedError, Throwable {
            HierNameAtomicBooleanExpression a = new
                HierNameAtomicBooleanExpression(true,
                        HierName.makeHierName("a", '/'));
            HierNameAtomicBooleanExpression b = new
                HierNameAtomicBooleanExpression(true,
                        HierName.makeHierName("b", '/'));
            HierNameAtomicBooleanExpression c = new
                HierNameAtomicBooleanExpression(true,
                        HierName.makeHierName("c", '/'));
            HierNameAtomicBooleanExpression d = new
                HierNameAtomicBooleanExpression(true,
                        HierName.makeHierName("d", '/'));

            ArrayList temp = new ArrayList();
            temp.add(a);
            temp.add(b);
            OrBooleanExpression o1 = new OrBooleanExpression(true, temp);

            temp.clear();
            temp.add(c);
            temp.add(d);
            OrBooleanExpression o2 = new OrBooleanExpression(true, temp);

            temp.clear();
            temp.add(o1);
            temp.add(o2);
            AndBooleanExpression a1 = new AndBooleanExpression(true, temp);
            /* a1 == (a | b) & (c | d) */
            OrBooleanExpressionInterface dnf1 = a1.DNFForm();
            /* dnf1 == (a & c) | (a & d) | (b & c) | (b & d) */
            temp.clear();

            temp.clear();
            temp.add(a);
            temp.add(c);
            AndBooleanExpression a2 = new AndBooleanExpression(true, temp);

            temp.clear();
            temp.add(a);
            temp.add(d);
            AndBooleanExpression a3 = new AndBooleanExpression(true, temp);

            temp.clear();
            temp.add(b);
            temp.add(c);
            AndBooleanExpression a4 = new AndBooleanExpression(true, temp);

            temp.clear();
            temp.add(b);
            temp.add(d);
            AndBooleanExpression a5 = new AndBooleanExpression(true, temp);

            temp.clear();
            temp.add(a2);
            temp.add(a3);
            temp.add(a4);
            temp.add(a5);
            OrBooleanExpression dnf2 = new OrBooleanExpression(true, temp);

            assertTrue(dnf2.equals(dnf1));
        }
    }

    public static class Test3CNF extends AbstractTestCase {
        public void test() throws TestFailedError, Throwable {
            HierNameAtomicBooleanExpression a = new
                HierNameAtomicBooleanExpression(true,
                        HierName.makeHierName("a", '/'));
            HierNameAtomicBooleanExpression b = new
                HierNameAtomicBooleanExpression(true,
                        HierName.makeHierName("b", '/'));
            HierNameAtomicBooleanExpression c = new
                HierNameAtomicBooleanExpression(false,
                        HierName.makeHierName("c", '/'));

            ArrayList temp = new ArrayList();

            // a & b & ~c #> c+
            temp.add(a);
            temp.add(b);
            temp.add(c);
            AndBooleanExpression x = new AndBooleanExpression(false, temp);
            temp.clear();
            temp.add(a.negated());
            temp.add(b.negated());
            temp.add(c.negated());
            OrBooleanExpression y = new OrBooleanExpression(true, temp);
            assertTrue(y.equals(x.CNFForm().DNFForm()));
        }
    }
    public static class TestDeep extends AbstractTestCase {
        public void test() throws TestFailedError, Throwable {
            HierNameAtomicBooleanExpression a = new
                HierNameAtomicBooleanExpression(true,
                        HierName.makeHierName("a", '/'));
            HierNameAtomicBooleanExpression b = new
                HierNameAtomicBooleanExpression(true,
                        HierName.makeHierName("b", '/'));
            HierNameAtomicBooleanExpression c = new
                HierNameAtomicBooleanExpression(false,
                        HierName.makeHierName("c", '/'));

            ArrayList temp = new ArrayList();

            temp.add(a);
            temp.add(b);
            temp.add(c);
            AndBooleanExpression x = new AndBooleanExpression(true, temp);
            temp.clear();
            temp.add(a.negated());
            temp.add(b.negated());
            temp.add(c.negated());
            OrBooleanExpression y = new OrBooleanExpression(true, temp);
            temp.clear();
            temp.add(y.negated());
            temp.add(x.negated());
            AndBooleanExpression z = new AndBooleanExpression(true, temp);
            temp.clear();
            temp.add(a);
            temp.add(b);
            temp.add(c);
            temp.add(y);
            AndBooleanExpression z1 = new AndBooleanExpression(true, temp);
            assertTrue(z1.equals(z.CNFForm()));
        }
    }
    public static class TestOrder extends AbstractTestCase {
        public void test() throws TestFailedError, Throwable {
            HierName ha = HierName.makeHierName("a");
            HierName hb = HierName.makeHierName("b");
            HierName hc = HierName.makeHierName("c");
            HierName hd = HierName.makeHierName("d");
            HierName he = HierName.makeHierName("e");
            HierName hf = HierName.makeHierName("f");
            HierNameAtomicBooleanExpression aba = new HierNameAtomicBooleanExpression(true, ha);
            HierNameAtomicBooleanExpression abb = new HierNameAtomicBooleanExpression(true, hb);
            HierNameAtomicBooleanExpression abc = new HierNameAtomicBooleanExpression(true, hc);
            HierNameAtomicBooleanExpression abd = new HierNameAtomicBooleanExpression(true, hd);
            HierNameAtomicBooleanExpression abe = new HierNameAtomicBooleanExpression(true, he);
            HierNameAtomicBooleanExpression abf = new HierNameAtomicBooleanExpression(true, hf);
            ArrayList ccod = new ArrayList();  ccod.add(abc); ccod.add(abd);
            AndBooleanExpression cod = new AndBooleanExpression(true, ccod);
            ArrayList ceof = new ArrayList();  ceof.add(abe); ceof.add(abf);
            AndBooleanExpression eof = new AndBooleanExpression(true, ceof);
            ArrayList cta = new ArrayList(); cta.add(cod); cta.add(eof);
            OrBooleanExpression ta = new OrBooleanExpression(true, cta);
            ArrayList ctop = new ArrayList(); ctop.add(aba); ctop.add(abb); ctop.add(ta);
            AndBooleanExpression top = new AndBooleanExpression(true, ctop);
            // top == ("a"&"b"&(("c"&"d")|("e"&"f")))
            assertTrue(top.DNFForm().toString().equals("((\"a\"&\"b\"&\"c\"&\"d\")|(\"a\"&\"b\"&\"e\"&\"f\"))"));
        }
    }
}
