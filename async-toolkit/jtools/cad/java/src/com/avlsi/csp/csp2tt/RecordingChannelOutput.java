/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.csp2tt;

import java.math.BigInteger;

import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.Message;
import com.avlsi.tools.tsim.WaiterInterface;

/**
 * ChannelOutput implementation that records
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class RecordingChannelOutput implements ChannelOutput {
    private BigInteger value;
    private int numSends;
    private final int radix;
    private final int width;
    private final BigInteger numPossibleValues;
    private String name;

    /**
     * Class constructor.
     *
     * <pre><jml>
     *   public normal_behavior
     *     requires radix >= 2;
     *     requires width >= 1;
     * </jml></pre>
     **/
    public RecordingChannelOutput(String name, int radix, int width) {
        this.value = null;
        this.numSends = 0;
        this.radix = radix;
        this.width = width;
        this.numPossibleValues = BigInteger.valueOf(radix).pow(width);
        this.name = name;
    }

    public long getSendTime() {
        return 0;
    }

    public long getCycleTime() {
        return 0;
    }

    public BigInteger getNumPossibleValues() {
        return numPossibleValues;
    }

    public long send(Message m, long deviceTime) {
        value = m.getValue();
        numSends++;
        return 0;
    }

    /**
     * Clears <code>value</code> and <code>numSends</code>.
     **/
    public void startNewCycle() {
        value = null;
        numSends = 0;
    }

    /**
     * Returns the last value sent since {@link #startNewCycle()} was
     * called, or null if none.
     **/
    public BigInteger getValue() {
        return value != null ? value.mod(numPossibleValues) : null;
    }

    /**
     * Returns the number of calls to {@link #send} since
     * {@link #startNewCycle()} was called.
     *
     * <pre><jml>
     *   public normal_behavior
     *     ensures \result >= 0;
     * </jml></pre>
     **/
    public int getNumSends() {
        return numSends;
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
        // do nothing
    }

    public void clearWriteWaiter() {
        // do nothing
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
    
    public void destroy() {
        // do nothing
    }

    public int getRadix() {
        return radix;
    }

    public int getWidth() {
        return width;
    }
}
