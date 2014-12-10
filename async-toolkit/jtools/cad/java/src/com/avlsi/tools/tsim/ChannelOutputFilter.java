/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.util.debug.Debug;

import java.math.BigInteger;

/**
 * <p> Filters output bits of an underlying <code>ChannelOutput</code> according
 * to a specified mask.  See also <code>ChannelInputUnfilter</code>. </p>
 *
 * @author Mike Davies
 * @author Dan Daly
 * @author Jesse Rosenstock
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public final class ChannelOutputFilter 
    implements ChannelOutput, WaiterInterface, TSimDebug {

    /** Masked output channel **/
    private final ChannelOutput chan;

    /** Mask **/
    BigInteger mask;

    /** name **/
    private String name;

    /** WaiterInterface waiting to write to this channel. **/
    private WaiterInterface writeWaiter;

    public ChannelOutputFilter(final ChannelOutput c, final BigInteger m) {
        this(c,m,null);
    }

    public ChannelOutputFilter(final ChannelOutput c, final BigInteger m,
                               final String name) {
        chan = c;
        mask = m;
        if (DEBUG)
            Debug.assertTrue(mask.signum() != -1, "Mask has infinite number of 1's.");
        this.name = name;
    }

    // ChannelOutput support.  Most of it is simple pass-through

    public final void clearWriteWaiter() {
        chan.clearWriteWaiter();
        writeWaiter = null;
    }

    public final void destroy() {
        chan.destroy();
    }

    public final long getCycleTime() {
        return chan.getCycleTime();
    }

    public final long getSendTime() {
        return chan.getSendTime();
    }

    /** Returns the bundle name **/
    public final String getName() {
        return name;
    }

    public BigInteger getNumPossibleValues() {
        return BigInteger.valueOf(2).pow(mask.bitCount());
    }

    public boolean probeSend(long time) {
        return chan.probeSend(time);
    }

    public boolean probeSend() {
        return chan.probeSend();
    }

    public boolean checklessProbeSend(long time) {
        return chan.checklessProbeSend(time);
    }

    public boolean checklessProbeSend() {
        return chan.checklessProbeSend();
    }
    
    /** 
     * @todo Undocumented.
     * @exception InterruptedException When?
     **/
    public void send(final Message m)
        throws InterruptedException {
        this.send(m, 0);
    }

    /** 
     * @todo Undocumented.
     * @exception InterruptedException When?
     **/
    public long send(final Message m, final long deviceTime)
        throws InterruptedException {
        final BigInteger tok = packBits(mask,m.getValue());
        return chan.send(m.clone(packBits(mask,m.getValue())),
                         deviceTime);
    }

    public void setName(final String name) {
        this.name = name;
    }

    public void setWriteWaiter(WaiterInterface waiter) {
        this.writeWaiter = waiter;
        chan.setWriteWaiter(this);
    }

    /**
     * If all subchannels are now ready, wake up the process waiting on us.
     **/
    public void wakeUp() {
        Debug.assertTrue(writeWaiter != null);
        if (probeSend())
            writeWaiter.wakeUp();
    }

    /*** 
     * Pack bits of a BigInteger according to the specified mask. 
     * Mask must be non-negative (i.e. must have finite number of 1's).
     ***/
    public static BigInteger packBits(final BigInteger m, final BigInteger v) {
        BigInteger pv = BigInteger.ZERO;
        int i=0;
        for (int j=0; j<m.bitCount(); j++) {
            while (!m.testBit(i)) ++i;
            if (v.testBit(i)) pv = pv.setBit(j);
            ++i;
        }
        return pv;
    }

} // end of final class ChannelOutputFilter

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
