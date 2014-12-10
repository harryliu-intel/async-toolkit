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

import java.io.Reader;
import java.io.StringReader;

import com.avlsi.test.AbstractTestCase;

/**
 * Tests of the <code>PositionStackReader</code> class
 *
 * @see PositionStackReader
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class TestPositionStackReader extends AbstractTestCase {

    public void test() throws Throwable {
        final StringBuffer sb = new  StringBuffer();

        for (char ch = 'a'; ch <= 'z'; ++ch)
            sb.append(ch);
        for (char ch = 'A'; ch <= 'Z'; ++ch)
            sb.append(ch);
        for (char ch = '0'; ch <= '9'; ++ch)
            sb.append(ch);

        final String s = sb.toString();
        final PositionStackReader psr
            = new PositionStackReader(new StringReader(s));

        assertTrue(psr.read() == 'a');
        assertTrue(psr.read() == 'b');
        assertTrue(psr.read() == 'c');

        psr.savePosition();

        assertTrue(psr.read() == 'd');
        assertTrue(psr.read() == 'e');

        psr.restorePosition();

        assertTrue(psr.read() == 'd');
        assertTrue(psr.read() == 'e');
        assertTrue(psr.read() == 'f');

        psr.savePosition();

        assertTrue(psr.read() == 'g');
        
        psr.savePosition();

        assertTrue(psr.read() == 'h');

        psr.discardPosition();

        assertTrue(psr.read() == 'i');

        psr.restorePosition();

        assertTrue(psr.read() == 'g');
        assertTrue(psr.read() == 'h');
        assertTrue(psr.read() == 'i');
        assertTrue(psr.read() == 'j');

        psr.savePosition();
        psr.savePosition();

        assertTrue(psr.read() == 'k');

        psr.restorePosition();

        assertTrue(psr.read() == 'k');

        psr.restorePosition();

        assertTrue(psr.read() == 'k');
        assertTrue(psr.read() == 'l');

        psr.savePosition();

        assertTrue(psr.read() == 'm');

        psr.savePosition();

        assertTrue(psr.read() == 'n');

        psr.savePosition();

        assertTrue(psr.read() == 'o');

        psr.discardPosition();
        psr.discardPosition();
        psr.discardPosition();

        assertTrue(psr.read() == 'p');

        psr.savePosition();

        assertTrue(psr.read() == 'q');

        psr.savePosition();

        assertTrue(psr.read() == 'r');

        psr.savePosition();

        assertTrue(psr.read() == 's');

        psr.restorePosition();
        psr.restorePosition();

        assertTrue(psr.read() == 'r');
        assertTrue(psr.read() == 's');
        assertTrue(psr.read() == 't');

        psr.restorePosition();

        assertTrue(psr.read() == 'q');

        psr.savePosition();
        psr.savePosition();
        psr.discardPosition();
        psr.discardPosition();

        assertTrue(psr.read() == 'r');

        psr.savePosition();
        psr.savePosition();
        psr.restorePosition();
        psr.restorePosition();

        assertTrue(psr.read() == 's');

        psr.skip(3);

        // skip t, u, v from stream

        assertTrue(psr.read() == 'w');
        assertTrue(psr.read() == 'x');

        psr.savePosition();

        assertTrue(psr.read() == 'y');
        assertTrue(psr.read() == 'z');
        assertTrue(psr.read() == 'A');

        psr.restorePosition();
        psr.savePosition();

        assertTrue(psr.read() == 'y');

        psr.restorePosition();

        assertTrue(psr.read() == 'y');
        assertTrue(psr.read() == 'z');

        // save position before A for a while

        psr.savePosition();

        assertTrue(psr.read() == 'A');

        char[] chs = null;
        int n = -1;

        // test array read from stream
        chs = new char[3];
        n = psr.read(chs, 0, chs.length);

        assertTrue(n == 3);
        assertTrue(chs[0] == 'B');
        assertTrue(chs[1] == 'C');
        assertTrue(chs[2] == 'D');

        // keep A stored
        psr.restorePosition();
        psr.savePosition();

        // test read from buf / stream

        chs = new char[6];
        n = psr.read(chs, 0, chs.length);

        assertTrue(n == 6);
        assertTrue(chs[0] == 'A');
        assertTrue(chs[1] == 'B');
        assertTrue(chs[2] == 'C');
        assertTrue(chs[3] == 'D');
        assertTrue(chs[4] == 'E');
        assertTrue(chs[5] == 'F');

        // keep A stored
        psr.restorePosition();
        psr.savePosition();

        // skip from buf
        psr.skip(3);

        assertTrue(psr.read() == 'D');

        // keep A stored
        psr.restorePosition();
        psr.savePosition();

        assertTrue(psr.read() == 'A');

        // keep A stored
        psr.restorePosition();
        psr.savePosition();

        chs = new char[64];
        n = psr.read(chs, 0, chs.length);

        assertTrue(n == 36);
        for (char ch = 'A'; ch <= 'Z'; ++ch)
            assertTrue(chs[ch - 'A'] == ch);

        for (char ch = '0'; ch <= '9'; ++ch)
            assertTrue(chs[ch - '0' + 26] == ch);

        assertTrue(psr.read() == -1);
        assertTrue(psr.read() == -1);

        assertTrue(psr.read(chs, 0, chs.length) == -1);

    }

    public static void main(String[] args) throws Throwable {
        new TestPositionStackReader().test();
        System.err.println("SUCCESS");
    }
}
