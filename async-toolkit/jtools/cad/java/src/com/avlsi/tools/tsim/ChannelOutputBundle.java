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
 * <p> Bundles up an array of <code>ChannelOutput</code>s so that they can be
 * used as one <code>ChannelOutput</code>.  Each <code>send()</code> sends
 * exactly one token on each of the subchannels.  The subchannels always stay in
 * step with each other. </p>
 *
 * @author Kim Wallmark
 * @author Dan Daly
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public final class ChannelOutputBundle
    implements ChannelOutput, WaiterInterface {

    /** Code assumes that there is at least one element of this array. **/
    private final ChannelOutput[] bundledChans;
    /** Caching of channel "widths". **/
    private final BigInteger[] Ns;
    private       String name;
    /** WaiterInterface waiting to write to this channel. **/
    private       WaiterInterface writeWaiter;

    public ChannelOutputBundle(final ChannelOutput[] bundledChans) {
        this(bundledChans, null);
    }

    public ChannelOutputBundle(final ChannelOutput[] bundledChans,
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
    }

    // ChannelOutput support.  Most of it is simple pass-through

    public void clearWriteWaiter() {
        for (int i=0; i<bundledChans.length; i++) {
            bundledChans[i].clearWriteWaiter();
        }
        writeWaiter = null;
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

    /** Returns the max send time of all the bundled channels **/
    public long getSendTime() {
        long max = -1;
        for (int i=0; i<bundledChans.length; i++) {
            if (bundledChans[i].getSendTime() > max) 
                max = bundledChans[i].getSendTime();
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

    public boolean probeSend(long time) {
        for (int i=0; i<bundledChans.length; i++) {
            if (! bundledChans[i].probeSend(time))
                return false;
        }
        return true;
    }

    public boolean probeSend() {
        for (int i=0; i<bundledChans.length; i++) {
            if (! bundledChans[i].probeSend())
                return false;
        }
        return true;
    }

    public boolean checklessProbeSend(long time) {
        for (int i=0; i<bundledChans.length; i++) {
            if (! bundledChans[i].checklessProbeSend(time))
                return false;
        }
        return true;
    }

    public boolean checklessProbeSend() {
        for (int i=0; i<bundledChans.length; i++) {
            if (! bundledChans[i].checklessProbeSend())
                return false;
        }
        return true;
    }
    
    public void send(final Message m)
        throws InterruptedException {
        this.send(m, 0);
    }

    public long send(final Message m, final long deviceTime)
        throws InterruptedException {
        final BigInteger[] tokens =
            ChannelBundle.deconstructToken(m.getValue(), Ns);

        long lastTime = 0;
        for (int i=0; i<bundledChans.length; i++) {
            final long time =
                bundledChans[i].send(m.clone(tokens[i]),
                                     deviceTime);
            if (time > lastTime) {
                lastTime = time;
            }
        }

        return lastTime;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public void setWriteWaiter(WaiterInterface waiter) {
        this.writeWaiter = waiter;
        for (int i=0; i<bundledChans.length; i++) {
            bundledChans[i].setWriteWaiter(this);
        }
    }

    /**
     * If all subchannels are now ready, wake up the process waiting on us.
     **/
    public void wakeUp() {
        Debug.assertTrue(writeWaiter != null);
        if (probeSend())
            writeWaiter.wakeUp();
    }

} // end of final class ChannelOutputBundle

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
