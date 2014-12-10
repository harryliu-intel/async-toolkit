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

package com.avlsi.io;

import java.io.StringReader;

import com.avlsi.test.AbstractTestCase;

/**
 * This is a description of the class
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class TestStreamLexer extends AbstractTestCase {

    public void test() throws Throwable {
        final String s = "1e1 2. .3 0.0 3.14 1e-9 1e137 "
            + " abc1 3def->boo  ";
        final double[] ds
            = new double[]{1e1, 2., .3, 0.0, 3.14, 1e-9, 1e137};

        final StreamLexer lex = new StreamLexer(new StringReader(s));

        for (int i = 0; i < ds.length; ++i)
            assertTrue(lex.readDouble() == ds[i]);

        assertTrue(lex.readIdentifier().equals("abc1"));
        assertTrue(lex.readInt() == 3);
        assertTrue(lex.readIdentifier().equals("def"));
        assertTrue(lex.readSymbol("->").equals("->"));
        assertTrue(lex.readIdentifier().equals("boo"));
    }

    public static void main(String[] args) throws Throwable {
        new TestStreamLexer().test();
        System.out.println("PASSED");
    }
}
