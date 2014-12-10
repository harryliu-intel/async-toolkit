/*
 * Copyright 2001, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;

/**
 * <p> Class interfacing tsim to (arrays of) E1ofN channels in dsim.  This
 * is actually a wrapper for either a slackless/delayless converter or
 * a similar converter with internal buffering (via BufferedChannel). </p>
 *
 * <p> NodeReadChannel conveys tokens from dsim to tsim. </p>
 *
 * <p> In cast components of multibundle channels are:
 * <pre>
 * name[i].e
 * name[i].d[j]
 * </pre>
 * one-bundle channels omit the i. </p>
 *
 * @author Aaron Denney
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 23 July 2002.
 **/

public final class NodeReadChannel implements ChannelInput, NodeChannel, Statusable, ChannelStatus {

    private final ChannelInput wrapped;

    /** Creates slackless converter. **/
    public NodeReadChannel(String name, int N, int M) {
        this(0, name, N, M);
    }

    /** Creates slackless converter. **/
    public NodeReadChannel(String name, int N) {
        this(0, name, N);
    }

    /** Creates slackless converter. **/
    public NodeReadChannel(String name, RailList list, int M,
                           int index, boolean startNow) {
        this(0, name, list, M, index, startNow);
    }

    /**
     * Constructor.
     *
     * Standard 32 bit channel is (Name, 4, 16).
     *
     * @param name  Base name of the channel.
     * @param N     wires per enable.
     * @param M     bundles of wire.
     * @param slack amount of slack in buffered part of channel.  If
     *              this is 0, the non-buffered converter is wrapped.
     *
     * @throws IllegalSlackException  If slack is negative.
     **/
    public NodeReadChannel(int slack, String name, int N, int M) {
        this(slack, name, new RailList(N), M,-1, true);
    }

    public NodeReadChannel(int slack, int ffLatency, int bbLatency,
                           int fbLatency, int bfLatency, String name,
                           int N, int M) {
        this(slack, ffLatency, bbLatency, fbLatency, fbLatency, bfLatency,
             ffLatency + bbLatency + fbLatency + bfLatency,
             ffLatency + bbLatency + fbLatency + bfLatency,
             name, N, M);
    }

    public NodeReadChannel(int slack, int ffLatency, int bbLatency,
                           int fbNeutral, int fbValid, int bfLatency,
                           int cycleTimeIn, int cycleTimeOut,
                           String name, int N, int M) {
        this(slack, ffLatency, bbLatency, fbNeutral, fbValid, bfLatency,
             cycleTimeIn, cycleTimeOut, name, new RailList(N), M, -1, true);
    }

    /**
     * Constructor.
     *
     * @param name  Base name of the channel.  Components are 
     *              name + ".e", and name + ".d[" + {1,...,N} + "]".
     * @param N     wires per (1) enable.
     * @param slack amount of slack in buffered part of channel.  If
     *              this is 0, the non-buffered converter is wrapped.
     *
     * @throws IllegalSlackException  If slack is negative.
     **/
    public NodeReadChannel(int slack, String name, int N) {
        this(slack, name, new RailList(N), 1,-1, true);
    }

    public NodeReadChannel(int slack, String name, RailList list,
                           int M, int index, boolean startNow) {
        this(slack, 1, 1, 1, 1, name, list, M, index, startNow);
    }

    /**
     * @throws IllegalSlackException  If slack is negative.
     **/
    public NodeReadChannel(int slack, int ffLatency, int bbLatency,
                           int fbLatency, int bfLatency, String name,
                           RailList list, int M, int index, boolean startNow) {
        this(slack, ffLatency, bbLatency, fbLatency, fbLatency, bfLatency,
             ffLatency + bbLatency + fbLatency + bfLatency,
             ffLatency + bbLatency + fbLatency + bfLatency,
             name, list, M, index, startNow);
    }

    /**
     * @throws IllegalSlackException  If slack is negative.
     **/
    public NodeReadChannel(int slack, int ffLatency, int bbLatency,
                           int fbNeutral, int fbValid, int bfLatency,
                           int cycleTimeIn, int cycleTimeOut,
                           String name, RailList list, int M, int index,
                           boolean startNow) {
        if (slack == 0) {
            this.wrapped = new SlacklessNodeReadChannel(name, list, M,
                                                        index, startNow);
        }
        else {
            // This will throw IllegalSlackException if slack <= 0
            this.wrapped = new BufferedNodeReadChannel(
                    slack, ffLatency, bbLatency, fbNeutral, fbValid,
                    bfLatency, cycleTimeIn, cycleTimeOut, name, list, M,
                    index, startNow);
        }
    }

    /** Returns wrapped *NodeReadChannel.  Warning: returns by reference. **/
    public ChannelInput unwrap() {
        return wrapped;
    }

    ///////////////////////  Channel interface.

    public long getReceiveTime() { 
        return wrapped.getReceiveTime();
    }

    public BigInteger getNumPossibleValues() {
        return wrapped.getNumPossibleValues();
    }

    public long getCycleTime() {
        return wrapped.getCycleTime();
    }

    public Message receive(long deviceTime) throws InterruptedException {
        return wrapped.receive(deviceTime);
    }

    public Message receive() throws InterruptedException {
        return wrapped.receive();
    }

    public Message probeValue() {
        return wrapped.probeValue();
    }

    public boolean checklessProbeReceive(long time) {
        return wrapped.checklessProbeReceive(time);
    }

    public boolean checklessProbeReceive() {
        return wrapped.checklessProbeReceive();
    }

    public boolean probeReceive(long time) {
        return wrapped.probeReceive(time);
    }

    public boolean probeReceive() {
        return wrapped.probeReceive();
    }

    public void waitForMessage() throws InterruptedException {
	wrapped.waitForMessage();
    }

    public void setReadWaiter(WaiterInterface waiter) {
        wrapped.setReadWaiter(waiter);
    }

    public void clearReadWaiter() {
        wrapped.clearReadWaiter();
    }

    public void setName(String name) {
        wrapped.setName(name);
    }

    public String getName() {
        return wrapped.getName();
    }

    public void destroy() {
        wrapped.destroy();
    }


    public void printStatus() {
        if (wrapped instanceof Statusable)
            ((Statusable)wrapped).printStatus();
    }

    public String toString() {
        return getName();
    }

    public int getCapacity() {
        return ((ChannelStatus) wrapped).getCapacity();
    }

    public int getAvailable() {
        return ((ChannelStatus) wrapped).getAvailable();
    }

    public String getVerboseStatus() {
        return ((ChannelStatus) wrapped).getVerboseStatus();
    }
} // end of final class NodeReadChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */

