/*
 *      BitBucketChannel.java - A ChannelOutput that goes nowhere
 *
 *      Copyright 2002 Fulcrum Microsystems, Inc.  All rights reserved.
 *
 *      $Id$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;

public class BitBucketChannel implements ChannelOutput {
    /** Peeks at the time on the next message to be sent **/
    public long getSendTime() {
        return 42;
    }

    /** Get the cycle time of this particular channel. **/
    public long getCycleTime() {
        return 37;
    }

    /**
     * Get the number of different values this channel can transmit.
     * If it's an even power of 2, it's equal to 2^bitwidth.  If the
     * channel can transmit any arbitrary BigInteger, this value is
     * -1.
     **/
    public BigInteger getNumPossibleValues() {
        return BigInteger.valueOf(-1);
    }

    /**
     * send a message.  Blocks until other end receives.
     **/
    public long send(Message m, long deviceTime) throws InterruptedException {
        return deviceTime + getCycleTime();
    }

    /**
     * deprecated  Replaced by send(Message, long, long)
     **/
    public void send(Message m) throws InterruptedException {
    }

    /**
     * Can we send without blocking?.  Should now be entirely accurate
     * and only return true if the full send cycle can be completed.
     **/
    public boolean probeSend(long time) {
        return true;
    }
    public boolean probeSend() {
        return true;
    }

    /**
     * Can we send without blocking?.  Should now be entirely accurate
     * and only return true if the full send cycle can be completed.
     * Does NOT check that the thread on the end of the channel.
     **/
    public boolean checklessProbeSend(long time) {
        return true;
    }
    public boolean checklessProbeSend() {
        return true;
    }

    /**
     * Set an object used for waiting in select().  Public only due to the
     * nature of interfaces.  Should only be used by WaiterInterfaces.
     *
     * @see WaiterInterface
     **/
    public void setWriteWaiter(WaiterInterface waiter) {
    }

    /**
     * Set an object used for waiting in select().  Public only due to the
     * nature of interfaces.  Should only be used by Wait.
     *
     * @see Wait
     **/
    public void setWriteWaiter(Wait waiter) {
    }

    /**
     * Clear an object used for waiting in select().  Public only due to the
     * nature of interfaces.  Should only be used by Wait.
     *
     * @see Wait
     **/
    public void clearWriteWaiter() {
    }

    private String name = "BitBucketChannel";

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    /**
     * Makes this channel unusable.  Should also try to make it GCable,
     * and disable any interactions it might have with other objects.
     **/
    public void destroy() {
    }

    public final static String _version =
        "$Id$";
}
