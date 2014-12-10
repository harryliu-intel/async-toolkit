/*
 *      TestChannelBundle.java - test cases for ChannelBundle
 *
 *      Copyright 2002-2003 Fulcrum Microsystems, Inc.  All rights reserved.
 *
 *      $Id$
 */

package com.avlsi.tools.tsim;

import com.avlsi.test.AbstractTestCase;
import java.util.ArrayList;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import com.avlsi.test.TestFailedError;

/**
 * Test cases for ChannelBundle
 *
 * @author Patrick Pelletier
 */

public class TestChannelBundle extends AbstractTestCase {
    public void test() throws TestFailedError, Throwable {
        Random r = new Random(28);
        for (int i = 0; i < 300; i++) {
            ArrayList ns = new ArrayList();
            int bits = 63;
            while (bits > 0) {
                int n = 1 + r.nextInt(Math.min(bits, 30));
                ns.add(new Integer(n));
                bits -= n;
            }

            long expected = 0;
            BigInteger[] slices = new BigInteger[ns.size()];
            BigInteger[] bigNs = new BigInteger[ns.size()];
            int j = 0;
            for (Iterator it = ns.iterator(); it.hasNext(); j++) {
                Integer n = (Integer) it.next();
                int possible = (1 << n.intValue());
                int x = r.nextInt(possible);
                bigNs[j] = new BigInteger(Integer.toString(possible));
                slices[j] = new BigInteger(Integer.toString(x));
                expected <<= n.intValue();
                expected += (long)x;
            }

            BigInteger result = ChannelBundle.constructToken(slices, bigNs);
            if (expected != result.longValue()) {
                System.err.println("Got 0x" + result.toString(16) +
                                   " but expected 0x" +
                                   Long.toHexString(expected));
                for (int k = 0; k < slices.length; k++)
                    System.err.println("0x" + bigNs[k].toString(16) + ": 0x" +
                                       slices[k].toString(16));
            }
            assertTrue(expected == result.longValue());

            BigInteger[] newSlices =
                ChannelBundle.deconstructToken(result, bigNs);
            for (int k = 0; k < slices.length; k++)
                assertTrue(slices[k].equals(newSlices[k]));
        }
    }

    public static final String _version =
        "$Id$";
}
