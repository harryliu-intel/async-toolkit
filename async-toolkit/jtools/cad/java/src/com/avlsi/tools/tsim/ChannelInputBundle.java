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
 * <p> Bundles up an array of <code>ChannelInput</code>s so that they can be
 * used as one <code>ChannelInput</code>.  Each <code>receive()</code> receives
 * exactly one token from each of the subchannels.  The subchannels always stay
 * in step with each other. </p>
 *
 * @author Kim Wallmark
 * @author Dan Daly
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public final class ChannelInputBundle
    implements ChannelInput, WaiterInterface {

    /** Code assumes that there is at least one element of this array. **/
    private final ChannelInput[] bundledChans;
    /** Caching of channel "widths". **/
    private final BigInteger[] Ns;
    private       String name;
    /** WaiterInterface waiting to read from this channel. **/
    private       WaiterInterface readWaiter;

    public ChannelInputBundle(final ChannelInput[] bundledChans) {
        this(bundledChans, null);
    }

    public ChannelInputBundle(final ChannelInput[] bundledChans,
                              final String name) {
        Debug.assertTrue(bundledChans.length > 1,
                     "bundling fewer than two channels together is silly");
        this.bundledChans = bundledChans;
        this.name = name;

        Ns = new BigInteger[bundledChans.length];
        for (int i=0; i<bundledChans.length; i++) {
            Ns[i] = bundledChans[i].getNumPossibleValues();
            if (Ns[i].signum() != 1) {
                throw new IllegalArgumentException(bundledChans[i].getName() +
             " has getNumPossibleValues() = " + Ns[i] + ", which is illegal!");
            }
        }

        this.readWaiter = null;
    }

    /**
     * Uses the tokens of messages to construct the token for the
     * returned Message.  The time of the constructed Message is the
     * latest time in messages.  The rest of the information is just
     * copied from the first Message.
     **/
    private Message constructMessage(final Message[] messages) {
        Message result = null;

        Debug.assertTrue(messages.length == bundledChans.length,
                     "length mismatch: " + messages.length + "!=" +
                     bundledChans.length);

        long lastTime = 0;

        final BigInteger[] tokens = new BigInteger[messages.length];
        for (int i=0; i<messages.length; i++) {
            tokens[i] = messages[i].getValue();
            if (messages[i].getTime() > lastTime) {
                lastTime = messages[i].getTime();
            }
        }

        final BigInteger token = ChannelBundle.constructToken(tokens, Ns);
        result = messages[0].clone(token);
        result.setTime(lastTime);

        return result;
    }

    // ChannelInput support.  Most of it is simple pass-through

    public void clearReadWaiter() {
        for (int i=0; i<bundledChans.length; i++) {
            bundledChans[i].clearReadWaiter();
        }
        readWaiter = null;
    }

    public void destroy() {
        for (int i=0; i<bundledChans.length; i++) {
            bundledChans[i].destroy();
        }
    }

    /** Assumes that the cycle time of the first channel is representative. **/
    public long getCycleTime() {
        return bundledChans[0].getCycleTime();
    }

    /** Returns the max receive time for the bunch of channels **/
    public long getReceiveTime() {
        long max = -1;
        for (int i=0; i<bundledChans.length; i++) {
            if (bundledChans[i].getReceiveTime() > max) 
                max = bundledChans[i].getReceiveTime();
        }
        return max;
    }

    /** Returns the bundle name **/
    public String getName() {
        return name;
    }

    public BigInteger getNumPossibleValues() {
        BigInteger accum = Ns[0];
        for (int i=1; i<Ns.length; i++) {
            accum = accum.multiply(Ns[i]);
        }
        return accum;
    }

    /**
     * Ready to receive if there's data waiting on all internal
     * channels.  This isn't synchronized.
     **/
    public boolean checklessProbeReceive(long time) {
        for (int i=0; i<bundledChans.length; i++) {
            if (! bundledChans[i].checklessProbeReceive(time))
                return false;
        }
        return true;
    }

    /**
     * Ready to receive if there's data waiting on all internal
     * channels.  This isn't synchronized.
     **/
    public boolean checklessProbeReceive() {
        for (int i=0; i<bundledChans.length; i++) {
            if (! bundledChans[i].checklessProbeReceive())
                return false;
        }
        return true;
    }
    
    /**
     * Ready to receive if there's data waiting on all internal
     * channels.  This isn't synchronized.
     **/
    public boolean probeReceive(long time) {
        for (int i=0; i<bundledChans.length; i++) {
            if (! bundledChans[i].probeReceive(time))
                return false;
        }
        return true;
    }

    /**
     * Ready to receive if there's data waiting on all internal
     * channels.  This isn't synchronized.
     **/
    public boolean probeReceive() {
        for (int i=0; i<bundledChans.length; i++) {
            if (! bundledChans[i].probeReceive())
                return false;
        }
        return true;
    }

    /**
     * Returns null if probeReceive() is false.
     *
     * TODO: implement caching for this and receive()
     **/
    public Message probeValue() {
        final Message[] messages = new Message[bundledChans.length];
        for (int i=0; i<bundledChans.length; i++) {
            messages[i] = bundledChans[i].probeValue();
            if (messages[i] == null) return null;
        }

        return constructMessage(messages);
    }

    public Message receive() throws InterruptedException {
        final Message[] messages = new Message[bundledChans.length];
        for (int i=0; i<bundledChans.length; i++) {
            messages[i] = bundledChans[i].receive();
        }

        return constructMessage(messages);
    }

    public Message receive(long deviceTime) throws InterruptedException {
        final Message[] messages = new Message[bundledChans.length];
        for (int i=0; i<bundledChans.length; i++) {
            messages[i] = bundledChans[i].receive(deviceTime);
        }

        return constructMessage(messages);
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setReadWaiter(WaiterInterface waiter) {
        readWaiter = waiter;
        for (int i=0; i<bundledChans.length; i++) {
            bundledChans[i].setReadWaiter(this);
        }
    }

    /**
     * Waits until all bundled channels have a message waiting.
     **/
    public void waitForMessage() throws InterruptedException {
        final Wait w = new Wait(bundledChans, null);
        while (w.nonEmpty()) {
            w.select();
            w.removeReady();
        }
    }

    /**
     * If all subchannels are now ready, wake up the process waiting on us.
     **/
    public void wakeUp() {
        Debug.assertTrue(readWaiter != null);
        if (probeReceive())
            readWaiter.wakeUp();
    }

} // end of final class ChannelInputBundle

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
