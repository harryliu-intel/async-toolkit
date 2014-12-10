/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.csp2tt;

import java.math.BigInteger;

import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.Message;
import com.avlsi.tools.tsim.WaiterInterface;

/**
 * ChannelInput implementation that provides a fixed value.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class PlayingChannelInput implements ChannelInput {
    private BigInteger value;
    private int numReceives;
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
    public PlayingChannelInput(String name, int radix, int width) {
        this.value = null;
        this.numReceives = 0;
        this.radix = radix;
        this.width = width;
        this.numPossibleValues = BigInteger.valueOf(radix).pow(width);
        this.name = name;
    }

    public long getReceiveTime() {
        return 0;
    }

    public long getCycleTime() {
        return 0;
    }

    public BigInteger getNumPossibleValues() {
        return numPossibleValues;
    }

    public Message receive(long deviceTime) {
        numReceives++;
        return new Message(value);
    }

    public Message receive() {
        numReceives++;
        return new Message(value);
    }

    /**
     * Sets <code>this.value</code> to <code>value mod
     * numPossibleValues</code> and clears <code>numReceives</code>.
     **/
    public void startNewCycle(final BigInteger value) {
        this.value = value.mod(numPossibleValues);
        this.numReceives = 0;
    }

    public BigInteger getValue() {
        return value;
    }

    /**
     * Returns the number of calls to {@link #receive} since
     * {@link #startNewCycle} was called.
     *
     * <pre><jml>
     *   public normal_behavior
     *     ensures \result >= 0;
     * </jml></pre>
     **/
    public int getNumReceives() {
        return numReceives;
    }

    public boolean probeReceive(long time) {
        return true;
    }

    public boolean probeReceive() {
        return true;
    }

    public Message probeValue() {
        return new Message(value);
    }

    public boolean checklessProbeReceive(long time) {
        return true;
    }

    public boolean checklessProbeReceive() {
        return true;
    }

    public void waitForMessage() {
        // do nothing
    }

    public void setReadWaiter(WaiterInterface waiter) {
        // do nothing
    }

    public void clearReadWaiter() {
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
