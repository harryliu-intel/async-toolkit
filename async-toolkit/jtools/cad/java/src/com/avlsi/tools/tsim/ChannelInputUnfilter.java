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
 * <p> Class for performing the complement function of what
 * <code>ChannelOutputFilter</code> provides: unpacks the tokens on a channel
 * according to the specified mask. </p>
 *
 * @author Mike Davies
 * @author Jesse Rosenstock
 * @author Mike Atkin
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public final class ChannelInputUnfilter
    implements ChannelInput, WaiterInterface {

    /** Input channel **/
    private final ChannelInput chan;

    /** Unpacking mask **/
    private final BigInteger mask;

    /** WaiterInterface waiting to read from this channel. **/
    private WaiterInterface readWaiter;

    /** Channel name **/
    private String name;

    public ChannelInputUnfilter(final ChannelInput c, final BigInteger m) {
        this(c, m, null);
    }

    public ChannelInputUnfilter(final ChannelInput c, final BigInteger m,
                                final String name) {
        this.chan = c;
        this.mask = m;
        this.name = name;
        this.readWaiter = null;
    }

    // ChannelInput support.  Most of it is simple pass-through

    public void clearReadWaiter() {
        chan.clearReadWaiter();
        readWaiter = null;
    }

    public void destroy() {
        chan.destroy();
    }

    /** Assumes that the cycle time of the first channel is representative. **/
    public long getCycleTime() {
        return chan.getCycleTime();
    }

    /** Returns the max receive time for the bunch of channels **/
    public long getReceiveTime() {
        return chan.getReceiveTime();
    }

    /** Returns the bundle name **/
    public String getName() {
        return name;
    }

    public BigInteger getNumPossibleValues() {
        return BigInteger.valueOf(2).pow(mask.bitCount());
    }

    /**
     * Ready to receive if there's data waiting on all internal
     * channels.  This isn't synchronized.
     **/
    public boolean checklessProbeReceive(long time) {
        return chan.checklessProbeReceive(time);
    }

    /**
     * Ready to receive if there's data waiting on all internal
     * channels.  This isn't synchronized.
     **/
    public boolean checklessProbeReceive() {
        return chan.checklessProbeReceive();
    }
    
    /**
     * Ready to receive if there's data waiting on all internal
     * channels.  This isn't synchronized.
     **/
    public boolean probeReceive(long time) {
        return chan.probeReceive(time);
    }

    /**
     * Ready to receive if there's data waiting on all internal
     * channels.  This isn't synchronized.
     **/
    public boolean probeReceive() {
        return chan.probeReceive();
    }

    /**
     * @return <var>null</var> if <code>probeReceive()</code> is false.
     **/
    public Message probeValue() {
        final Message m = chan.probeValue();
        return m.clone(unpackBits(mask, m.getValue()));
    }

    public Message receive() throws InterruptedException {
        final Message m = chan.receive();
        return m.clone(unpackBits(mask,m.getValue()));
    }

    public Message receive(long deviceTime) throws InterruptedException {
        final Message m = chan.receive(deviceTime);
        return m.clone(unpackBits(mask,m.getValue()));
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setReadWaiter(WaiterInterface waiter) {
        readWaiter = waiter;
        chan.setReadWaiter(this);
    }

    /**
     * Waits until all bundled channels have a message waiting.
     **/
    public void waitForMessage() throws InterruptedException {
        ChannelInput [] in = { chan };
        final Wait w = new Wait(in, null);
        w.select();
    }

    /**
     * If all subchannels are now ready, wake up the process waiting on us.
     **/
    public void wakeUp() {
        Debug.assertTrue(readWaiter != null);
        if (probeReceive())
            readWaiter.wakeUp();
    }

    /**
     * Unpacks the given BigInteger according to the specified mask
     * (zeros are filled in where the mask is zero).  Mask must be
     * non-negative (i.e. must have finite number of one's.)
     **/
    public static BigInteger unpackBits(final BigInteger m, BigInteger v) {
        BigInteger upv = BigInteger.ZERO;
	int i=0;
        for (int j=0; j<m.bitCount(); j++) {
	    while (!m.testBit(i)) ++i;
            if (v.testBit(j)) upv = upv.setBit(i);
	    ++i;
        }
        return upv;
    }

} // end of final class ChannelInputUnfilter

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
