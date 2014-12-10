/*
 *      TestChannelOutputBundle.java - test cases for ChannelBundle
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
 * Test cases for ChannelOutputBundle
 *
 * @author Patrick Pelletier
 */

public class TestChannelOutputBundle extends AbstractTestCase {
    private class DummyOutputChannel implements ChannelOutput {
        private final BigInteger possible;
        private final BigInteger expectedValue;

        public DummyOutputChannel(BigInteger possible,
                                  BigInteger expectedValue) {
            this.possible = possible;
            this.expectedValue = expectedValue;
        }

        public long getSendTime() {
            return 0;
        }

        public long getCycleTime() {
            return 0;
        }

        public BigInteger getNumPossibleValues() {
            return possible;
        }
        
        public long send(Message m, long deviceTime)
            throws InterruptedException {
            assertTrue(m.getValue().equals(expectedValue));
            return 0;
        }
        
        public boolean probeSend(long time) {
            return true;
        }

        public boolean probeSend() {
            return true;
        }

        public boolean checklessProbeSend(long time) {
            return true;
        }

        public boolean checklessProbeSend() {
            return true;
        }

        public void setWriteWaiter(WaiterInterface waiter) {
        }

        public void clearWriteWaiter() {
        }

        public void setName(String name) {
        }

        public String getName() {
            return null;
        }
        
        public void destroy() {
        }
    }

    public void test() throws TestFailedError, Throwable {
        Random r = new Random(160);
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

            ChannelOutput[] channels = new ChannelOutput[slices.length];
            for (int k = 0; k < slices.length; k++)
                channels[k] = new DummyOutputChannel(bigNs[k], slices[k]);

            ChannelOutput guineaPig = new ChannelOutputBundle(channels);
            guineaPig.send(new Message(expected), 0);
        }
    }

    public static final String _version =
        "$Id$";
}
