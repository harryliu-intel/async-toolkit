/*
 * Copyright 2001, 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.Event;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.NodeWatcher;
import com.avlsi.tools.dsim.WakeAt;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.debug.Debug;

/**
 * <p> Class representing (arrays of) E1ofN channels with a
 * <code>BufferedChannel</code> reading from them. </p>
 *
 * <p> Only can be used in one of its roles at once. </p>
 *
 * <p> In cast components of multibundle channels are:
 * <pre>
 * name[i].e
 * name[i].d[j]
 * </pre>
 * one-bundle channels omit the [i]. </p>
 *
 * <p> Expressed as as pseudo-CSP, the hand shaking expansion is:
 * <pre>
 *   x = 0
 * ; cycle_status = VALID
 * ; *[ &lt;,i:N: [ #L[i] -&gt; x += #L[i]? * v ; L[i].e- ] &gt;
 *    ; R!x
 *    ; x = 0
 *    ; cycle_status = NEUTRAL
 *    ; &lt;,i:N: [ ~#L[i] -&gt; L[i].e+ ] &gt;
 *    ; cycle_status = VALID
 *    ]
 * </pre>
 *
 * @concurrency Synchronization lock protects cycle_status and waitingOnChannel.
 *
 * @author Aaron Denney
 * @author Jesse Rosenstock
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 **/

public final class BufferedNodeReadChannel extends BufferedNodeChannel
    implements ChannelInput, TSimDebug {

    /**
     * True if any data rails are undefined.
     * 
     * <pre><jml>
     *     private invariant
     *         haveUndefinedRails <==>
     *             (\exists int i, j, k; 0 <= i && i < rlist.getLength() &&
     *                                   0 <= j && j < M &&
     *                                   0 <= k && k < rlist.get(i);
     *                                rail[i][j][k].getValue()
     *                                    == Node.VALUE_U);
     * </jml></pre>
     **/
    private boolean haveUndefinedRails;
    
    /**
     * Number of enables rails that are high.  We do not need a
     * <code>haveUndefinedEnables</code> flag because we control the enables
     * so they will never be undefined.
     * 
     * <pre><jml>
     *     private invariant
     *         numEnablesSet ==
     *             (\num_of int i, j; 0 <= i && i < rlist.getLength() &&
     *                                0 <= j && j < M;
     *                      enable[i][j].getValue() == Node.VALUE_1);
     * </jml></pre>
     **/
    private int numRailsSet;

    /** Time at which data became valid **/
    private long validTime;
    
    /**
     * Number of subchannels that exist.
     * 
     * <pre><jml>
     *     private invariant
     *         numChannels == rlist.getLength() * M;
     * </jml></pre>
     **/
    private final int numChannels;

    /**
     * Accumulator for value from the data rails.  When a data rail is
     * raised, its place value is added to this, and its enable is dropped.
     **/
    private BigInteger railsValue;

    /**
     * List of enables that need to be transitioned when we change
     * <code>cycle_status</code>.
     **/
    private final ArrayList enablesToTransition;

    /**
     * Legacy constructor, creates channel with slack 1
     *
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeReadChannel(String name, int N, int M) {
        this(1, name, N, M);
    }

    /**
     * Legacy constructor, creates channel with slack 1
     *
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeReadChannel(String name, int N) {
        this(1, name, N);
    }

    /**
     * Legacy constructor, creates channel with slack 1
     *
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeReadChannel(String name, RailList list, int M,
                                   int index, boolean startNow) {
        this(1, name, list, M, index, startNow);
    }

    /**
     * Constructor.
     *
     * Standard 32 bit channel is (Name, 4, 16).
     *
     * @param name  Base name of the channel.
     * @param N     wires per enable.
     * @param M     bundles of wire
     * @param slack amount of slack in buffered part of channel
     *
     * @throws IllegalSlackException  If slack is nonpositive.
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeReadChannel(int slack, String name, int N, int M) {
        this(slack, name, new RailList(N), M,-1, true);
    }

    protected void instantiate() {
        setEnables(Node.VALUE_0);
        numRailsSet = 0;
        watchRails();
        if (resetNode != null) {
            resetNodeWatcher = new ResetNodeWatcher();
            resetNode.addWatch(resetNodeWatcher);
        } else
            System.err.println("Warning:  No reset node to watch");
    }

    /**
     * Constructor.
     *
     * @param name Base name of the channel.  Components are 
     *             name + ".e", and name + ".d[" + {1,...,N} + "]".
     * @param N    wires per (1) enable.
     * @param slack amount of slack in buffered part of channel
     *
     * @throws IllegalSlackException  If slack is nonpositive.
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeReadChannel(int slack, String name, int N) {
        this(slack, name, new RailList(N), 1,-1, true);
    }

    /**
     * @throws IllegalSlackException  If slack is nonpositive.
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeReadChannel(int slack, String name, RailList list,
                                   int M, int index, boolean startNow) {
        this(slack, 1, 1, 1, 1, name, list, M, index, startNow);
    }

    public BufferedNodeReadChannel(int slack, int ffLatency, int bbLatency,
                                   int fbLatency, int bfLatency, String name,
                                   RailList list, int M, int index,
                                   boolean startNow) {
        this(slack, ffLatency, bbLatency, fbLatency, fbLatency, bfLatency,
             ffLatency + bbLatency + fbLatency + bfLatency,
             ffLatency + bbLatency + fbLatency + bfLatency,
             name, list, M, index, startNow);
    }

    public BufferedNodeReadChannel(int slack, int ffLatency, int bbLatency,
                                   int fbNeutral, int fbValid, int bfLatency,
                                   int cycleTimeIn, int cycleTimeOut,
                                   String name, RailList list, int M, int index,
                                   boolean startNow) {
        super(slack, ffLatency, bbLatency, fbNeutral, fbValid, bfLatency,
              cycleTimeIn, cycleTimeOut, name, list, M, index, startNow);
        this.haveUndefinedRails = true;
        this.numRailsSet = 0;
        this.numChannels = rlist.getLength() * M;
        this.enablesToTransition = new ArrayList(numChannels);
    }
    ///////////////////////  Channel interfaces.

    // Explicitly not synchronized.  Might deadlock select if it were.
    public boolean probeReceive(long time) {
        return bc.probeReceive(time);
    }

    // Explicitly not synchronized.  Might deadlock select if it were.
    public boolean probeReceive() {
        return bc.probeReceive();
    }

    public boolean checklessProbeReceive(long time) {
        return bc.checklessProbeReceive(time);
    }

    public boolean checklessProbeReceive() {
        return bc.checklessProbeReceive();
    }

    public long getReceiveTime() {
        return bc.getReceiveTime();
    }
    
    public Message probeValue() {
        return bc.probeValue();
    }

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public void waitForMessage() throws InterruptedException {
	bc.waitForMessage();
    }

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public Message receive(long deviceTime) throws InterruptedException {
        Message rv = bc.receive(deviceTime);
        synchronized (this) {
            if (waitingOnChannel) {
                moveToken(validTime);
                waitingOnChannel = false;
                railsValue   = BigInteger.ZERO;
                cycle_status = NEUTRAL;
            }
        }
        return rv;
    }

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public Message receive() throws InterruptedException {
        return this.receive(0);
    }

    public synchronized void setReadWaiter(WaiterInterface waiter) {
        bc.setReadWaiter(waiter);
    }

    public synchronized void clearReadWaiter() {
        bc.clearReadWaiter();
    }

    /**
     * Assumes rails are valid, moves the data value from them into
     * the buffered channel.  Also completes step 2 of the dsim cycle.
     *
     * @bug Potentially bogus handling of InterruptedException.
     **/
    protected void moveToken(long time) {
        try {
            long sendTime =
                bc.checklessSend(new Message(railsValue, time), time);
            setEnables(Node.VALUE_0, sendTime + fbValid);
        } catch (InterruptedException e) {
            // FIXME: there should be something better to do with this
            Debug.assertTrue(false);
        }
    }

    private final class ResetNodeWatcher implements NodeWatcher {
        public void nodeChanged(Node node, long time) {
            if (DEBUG)
                 assert node == resetNode
                     : "ResetNodeWatcher called on non-Reset node " + node;
            if (cycle_status != BEGIN)
                System.out.println ("Warning: Reset called after starting "+
                                    "channel");
            final byte value = node.getValue();
            if (value == Node.VALUE_1) {
                // some of the enables maybe aliased to data rails, so we
                // update the status before raising the enables
                cycle_status = VALID;
                railsValue = BigInteger.ZERO;
            }
            setEnables(value);
        }
    }

    private final class DataRailNodeWatcher implements NodeWatcher {
        private final BigInteger[] railPlaceValue;

        private DataRailNodeWatcher(final BigInteger[] railPlaceValue) {
            this.railPlaceValue = railPlaceValue;
        }

        private void updateRailsValue() {
            for (int i = 0; i < railPlaceValue.length; ++i) {
                railsValue = railsValue.add(railPlaceValue[i]);
            }
        }

        public void nodeChanged(Node node, long time) {
            if (DEBUG)
                assert isRail(node)
                    : "Called on node we don't manage: " + node;
            // Don't do anything during reset
            if (cycle_status == BEGIN)
                return;
            else if (cycle_status == DESTROYED)
                throw new IllegalStateException("Channel " + getName() +
                        " has been destroyed");

            final byte value = node.getValue();

            // update haveUndefinedRails
            if (haveUndefinedRails) {
                computeRailState();
                if (value == Node.VALUE_1) updateRailsValue();
            } else {
                switch (value) {
                    case Node.VALUE_0:
                        numRailsSet -= railPlaceValue.length;
                        break;
                    case Node.VALUE_1:
                        numRailsSet += railPlaceValue.length;
                        updateRailsValue();
                        break;
                    case Node.VALUE_U:
                        haveUndefinedRails = true;
                        break;
                }
            }

            switch (cycle_status) {
              case BEGIN:
              case VALID:
                if (waitingOnChannel) { break; }
                if (railsValid()) {
                    synchronized (BufferedNodeReadChannel.this) {
                        // if reader will be unblocked after this, set the
                        // enabling node for critical path analysis
                        if (bc.isReaderBlocked()) setEnablingNode(node);

                        // If there's room in the channel

                        // NOTE: probeSend() is explicitly *not* timed,
                        // because we want to act on any token (even a 
                        // future one).  moveToken() will take care of timing.
                        if (bc.probeSend()) {
                            moveToken(time);
                            railsValue = BigInteger.ZERO;
                            cycle_status = NEUTRAL;
                        }
                        else {
                            validTime = time;
                            waitingOnChannel = true;
                        }
                    }
                }
                break;
              case NEUTRAL:
                if (railsNeutral()) {
                    setEnables(Node.VALUE_1, time + fbNeutral);
                    cycle_status = VALID;
                }
                break;
              case DESTROYED:
                throw new IllegalStateException("Channel " + getName() +
                        " has been destroyed");
              default:
                Debug.assertTrue(false);
            }
        }
    }

    private void watchRails() {
        final MultiMap uniqueRails =
            new MultiMap(new HashMap(), MultiMap.ARRAY_LIST_FACTORY);
        BigInteger multiplier = BigInteger.ONE;
        for (int k=0;k< rlist.getLength();k++) {
            final int jmax = rlist.get(k);
            final BigInteger n = BigInteger.valueOf(jmax);
            for (int i = 0; i < M; i++) {
                for (int j = 0; j < jmax; j++) {
                    final BigInteger placeValue =
                        BigInteger.valueOf(j).multiply(multiplier);
                    uniqueRails.put(rail[k][i][j], placeValue);
                }
                multiplier = multiplier.multiply(n);
            }
        }
        for (Iterator i = uniqueRails.keySet().iterator(); i.hasNext(); ) {
            final Node rail = (Node) i.next();
            final Collection placeValues = uniqueRails.get(rail);
            rail.addWatch(
                new DataRailNodeWatcher(
                    (BigInteger[]) placeValues.toArray(new BigInteger[0])));
        }
    }

    private boolean isRail(Node n) {
        for (int k=0;k< rlist.getLength();k++) {
            for (int i = 0; i < M; i++) {
                for (int j = 0; j < rlist.get(k); j++) {
                    if (n == rail[k][i][j]) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private boolean railsValid() {
        return numRailsSet == numChannels && !haveUndefinedRails;
    }

    private boolean railsNeutral() {
        return numRailsSet == 0 && !haveUndefinedRails;
    }

    private void computeRailState() {
        haveUndefinedRails = false;
        numRailsSet = 0;
        for (int k=0;k<rlist.getLength();k++) {
            for (int i = 0; i < M; i++) {
                for (int j = 0; j < rlist.get(k); j++) {
                    byte value = rail[k][i][j].getValue();
                    if (value == Node.VALUE_U) {
                        haveUndefinedRails = true;
                        return;
                    } else if (value == Node.VALUE_1) {
                        ++numRailsSet;
                    }
                }
            }
        }
    }

    /**
     * Only for use when timing doesn't matter yet (ie, only during
     * reset).
     **/
    public void setEnables(byte value) {
        //System.out.println("SetEnables leng= "+rlist.getLength()+" M= "+M);
        for( int k=0;k< rlist.getLength();k++) {
            for (int i = 0; i < M; i++) {
                enable[k][i].setValueAndEnqueueDependents(value);
            }
        }
        //System.out.println("Enables set on "+name);
    }

    /**
     * Sets the enables to the specified value at the specified time.
     **/
    public void setEnables(byte value, long time) {
        for (int k = 0; k < rlist.getLength(); k++) {
            for (int i = 0; i < M; i++) {
                enable[k][i].scheduleTimeOrRandom(value, time,
                                                  getEnablingNode());
            }
        }
    }
} // end of final class BufferedNodeReadChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
