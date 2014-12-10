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
 * <p> NodeWriteChannel conveys tokens from tsim to dsim. </p>
 *
 * <p> In cast components of multibundle channels are:
 * <pre>
 * name[i].e
 * name[i].d[j]
 * </pre>
 * one-bundle channels omit the i. </p>
 *
 * @author Aaron Denney
 * @version $Date$
 *
 * @review kiniry - Under review beginning 23 July 2002.
 **/

public final class NodeWriteChannel implements ChannelOutput, NodeChannel, Statusable, ChannelStatus {

    private final ChannelOutput wrapped;

    /** Creates slackless converter. **/
    public NodeWriteChannel(String name, int N, int M) {
        this(0, name, N, M);
    }
    
    /** Creates slackless converter. **/
    public NodeWriteChannel(String name, int N) {
        this(0, name, new RailList(N), 1, -1, true);
    }
    
    /** Creates slackless converter. **/
    public NodeWriteChannel(String name, RailList list,
                            int M, int index, boolean startNow) {
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
    public NodeWriteChannel(int slack, String name, int N, int M) {
        this(slack, name, new RailList(N), M, -1, true);
    }

    public NodeWriteChannel(int slack, int ffLatency, int bbLatency,
                            int fbLatency, int bfLatency, int cycleTimeIn,
                            int cycleTimeOut, String name, int N, int M) {
        this(slack, ffLatency, bbLatency, fbLatency, bfLatency,
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
    public NodeWriteChannel(int slack, String name, int N) {
        this(slack,name,new RailList(N),1,-1,true);
    }

    /**
     * @throws IllegalSlackException  If slack is negative.
     **/
    public NodeWriteChannel(int slack, String name, RailList list,
                            int M, int index, boolean startNow) {
        this(slack, 1, 1, 1, 1, 4, 4, name, list, M, index, startNow);
    }

    public NodeWriteChannel(int slack, int ffLatency, int bbLatency,
                            int fbLatency, int bfLatency, int cycleTimeIn,
                            int cycleTimeOut, String name, RailList list,
                            int M, int index, boolean startNow) {
        if (slack == 0) {
            this.wrapped = new SlacklessNodeWriteChannel(name, list, M,
                                                         index, startNow);
        }
        else {
            // This will throw IllegalSlackException if slack <= 0
            this.wrapped = new BufferedNodeWriteChannel(slack, ffLatency,
                                                        bbLatency, fbLatency,
                                                        bfLatency, cycleTimeIn,
                                                        cycleTimeOut, name,
                                                        list, M, index,
                                                        startNow);
        }
    }

    /** Returns wrapped *NodeWriteChannel.  Warning: returns by reference. **/
    public ChannelOutput unwrap() {
        return wrapped;
    }

    ///////////////////////  ChannelOutput interface.

    public long getSendTime() { return wrapped.getSendTime(); }
    
    public BigInteger getNumPossibleValues() {
        return wrapped.getNumPossibleValues();
    }

    public long getCycleTime() {
        return wrapped.getCycleTime();
    }

    public long send(Message m, long deviceTime)
            throws InterruptedException {
        return wrapped.send(m, deviceTime);
    }

    public boolean probeSend(long time) {
        return wrapped.probeSend(time);
    }

    public boolean probeSend() {
        return wrapped.probeSend();
    }

    public boolean checklessProbeSend(long time) {
        return wrapped.checklessProbeSend(time);
    }

    public boolean checklessProbeSend() {
        return wrapped.checklessProbeSend();
    }

    public void clearWriteWaiter() {
        wrapped.clearWriteWaiter();
    }

    public void setWriteWaiter(WaiterInterface w) {
        wrapped.setWriteWaiter(w);
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
} // end of final class NodeWriteChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */

