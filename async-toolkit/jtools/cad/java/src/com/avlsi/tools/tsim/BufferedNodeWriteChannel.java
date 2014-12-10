/*
 * Copyright 2001, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.Event;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.NodeWatcher;
import com.avlsi.tools.dsim.WakeAt;
import com.avlsi.tools.tsim.RailList;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.debug.Debug;

/**
 * <p> Class representing (arrays of) E1ofN channels with a
 * <code>BufferedChannel</code> writing to them. </p>
 *
 * <p> Only can be used in one of its roles at once. </p>
 *
 * <p> In cast components of multibundle channels are
 * <code>
 * name[i].e
 * name[i].d[j]
 * </pre>
 * one-bundle channels omit the [i]. </p>
 *
 * <p> Expressed as as pseudo-CSP, the hand shaking expansion is:
 * <pre>
 *   cycle_status = VALID
 * ; *[ &lt;,i:N: [ R[i].e ] &gt;
 *    ; L?x
 *    ; &lt;,i:N: R[i].d[f(i, x)]+ &gt;
 *    ; cycle_status = NEUTRAL
 *    ; &lt;,i:N: [ ~R[i].e -&gt; R[i].d[f(i, x)]- ] &gt;
 *    ; cycle_status = VALID
 *    ]
 * </pre>
 *
 * @concurrency Synchronization lock protects cycle_status and waitingOnChannel.
 * @bug jmr  There is no lock used in send().
 *
 * @author Aaron Denney
 * @author Jesse Rosenstock
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 **/

public final class BufferedNodeWriteChannel extends BufferedNodeChannel
    implements ChannelOutput, TSimDebug {

    /**
     * The rail indexes that have been raised.
     * 
     * <pre><jml>
     *     private invariant
     *         raisedRails != null &&
     *         raisedRails.length == rlist.getLength() &&
     *         (\forall int i; 0 <= i && i < raisedRails.length;
     *                         raisedRails[i].length == M);
     *     private invariant
     *         cycle_status == NEUTRAL ==>
     *             (\forall int i, j; 0 <= i && i < rlist.getLength() &&
     *                                0 <= j && j < M;
     *                                rails[i][j][raisedRails[i][j]]
     *                                    .getValue() == Node.VALUE_1);
     *     private invariant
     *         cycle_status == NEUTRAL ==>
     *             (\forall int i, j, k; 0 <= i && i < rlist.getLength() &&
     *                                   0 <= j && j < M &&
     *                                   0 <= k && k < rlist.get(i) &&
     *                                   k != raisedRails[i][j];
     *                                rails[i][j][raisedRails[i][j]]
     *                                    .getValue() == Node.VALUE_0);
     * </jml></pre>
     **/
    private final int[][] raisedRails;

    /**
     * True if any enables are undefined.
     *
     * <pre><jml>
     *     private invariant
     *         haveUndefinedEnables <==>
     *             (\exists int i, j; 0 <= i && i < rlist.getLength() &&
     *                                0 <= j && j < M;
     *                      enable[i][j].getValue() == Node.VALUE_U);
     * </jml></pre>
     **/
    private boolean haveUndefinedEnables;

    /**
     * Number of enables that are high.  Only meaningful if
     * <code>haveUndefinedEnables</code> is false.
     *
     * <pre><jml>
     *     private invariant
     *         !haveUndefinedEnables ==>
     *             numEnablesSet ==
     *                 (\num_of int i, j; 0 <= i && i < rlist.getLength() &&
     *                                    0 <= j && j < M;
     *                          enable[i][j].getValue() == Node.VALUE_1);
     * </jml></pre>
     **/
    private int numEnablesSet;

    /**
     * Number of enables that exist.
     *
     * <pre><jml>
     *     private invariant
     *         numEnables == rlist.getLength() * M;
     * </jml></pre>
     **/
    private int numEnables;

    /** Time at which all enables were set **/
    private long enableTime;

    /**
     * Number of enables that have been gone low since the last time data was
     * sent.
     **/
    private int numEnablesReset;

    /**
     * Legacy constructor, creates channel with slack 1
     *
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeWriteChannel(String name, int N, int M) {
        this(1, name, N, M);
    }
    
    /**
     * Legacy constructor, creates channel with slack 1
     *
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeWriteChannel(String name, int N) {
        this(1, name, new RailList(N), 1, -1, true);
    }
    
    /**
     * Legacy constructor, creates channel with slack 1
     *
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeWriteChannel(String name, RailList list,
                                    int M, int index, boolean startNow) {
        this(1, name, list, M, index, startNow);
    }
    
    /**
     * Constructor.
     *
     * Standard 32 bit channel is (Name, 4, 16).
     *
     * @param name Base name of the channel.
     * @param N    wires per enable.
     * @param M    bundles of wire.
     * @param slack amount of slack in buffered part of channel
     *
     * @throws IllegalSlackException  If slack is nonpositive.
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeWriteChannel(int slack, String name, int N, int M) {
        this(slack, name, new RailList(N), M, -1, true);
    }

    protected void instantiate() {
        dropAllRails();
        watchEnables();
        if (resetNode != null) {
            resetNodeWatcher = new ResetNodeWatcher();
            resetNode.addWatch(resetNodeWatcher);
        } else
            System.out.println("Warning: No reset node to watch.");
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
    public BufferedNodeWriteChannel(int slack, String name, int N) {
        this(slack,name,new RailList(N),1,-1,true);
    }

    /**
     * @throws IllegalSlackException  If slack is nonpositive.
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeWriteChannel(int slack, String name, RailList list,
                                    int M, int index, boolean startNow) {
        this(slack, 1, 1, 1, 1, 4, 4, name, list, M, index, startNow);
    }

    public BufferedNodeWriteChannel(int slack, int ffLatency, int bbLatency,
                                    int fbLatency, int bfLatency,
                                    int cycleTimeIn, int cycleTimeOut,
                                    String name, RailList list, int M,
                                    int index, boolean startNow) {
        super(slack, ffLatency, bbLatency, fbLatency, bfLatency, cycleTimeIn,
              cycleTimeOut, name, list, M, index, startNow);
        this.raisedRails = new int[rlist.getLength()][M];
        this.haveUndefinedEnables = true;
        this.numEnablesSet = 0;
        this.numEnables = rlist.getLength() * M;
        this.numEnablesReset = 0;
    }

    ///////////////////////  Channel interfaces.

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public long send(Message m, long deviceTime)
            throws InterruptedException {
        long timestamp = bc.send(m, deviceTime);
        if (waitingOnChannel) {
            moveToken(enableTime);
            waitingOnChannel = false;
            cycle_status = NEUTRAL;
        }
        return timestamp;
    }

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public void send(Message m) throws InterruptedException {
        this.send(m, 0);
    }

    public boolean probeSend(long time) {
        return bc.probeSend(time);
    }

    public boolean probeSend() {
        return bc.probeSend();
    }

    public boolean checklessProbeSend(long time) {
        return bc.checklessProbeSend(time);
    }

    public boolean checklessProbeSend() {
        return bc.checklessProbeSend();
    }

    public long getSendTime() {
        return bc.getSendTime();
    }

    public synchronized void clearWriteWaiter() {
        bc.clearWriteWaiter();
    }

    public synchronized void setWriteWaiter(WaiterInterface w) {
        bc.setWriteWaiter(w);
    }

    /**
     * Assumes a message is waiting in the BufferedChannel, moves it
     * onto the rails, completing step 1 of the dsim cycle.
     *
     * @bug Potentially bogus handling of InterruptedException.
     **/
    protected void moveToken(long time) {
        try {
            Message m = bc.checklessReceive(time + bc.bfLatency);
            setRails(m, m.getTime());

            if (DEBUG) {
                Debug.log("moveToken: time " + time
                          + " bfLatency " + bc.bfLatency
                          + " m.time " + m.getTime());
            }
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
                System.out.println(
                    "Warning: Reset called after starting channel");
            if (node.getValue() == Node.VALUE_0) {
                dropAllRails();
            }
        }
    }

    private final class EnableNodeWatcher implements NodeWatcher {
        /**
         * The number of channels in the bundle for which this node is an
         * enable.
         **/
        private final int aliasCount;

        /**
         * Pairs of indices used to determine which rails should be dropped if
         * this enable goes low.  There might be more than one, because enables
         * can be aliased.
         **/
        private final int[] rails;

        private EnableNodeWatcher(final int aliasCount, final int[] rails) {
            this.aliasCount = aliasCount;
            this.rails = rails;
        }

        public void nodeChanged(Node node, long time) {
            if (DEBUG)
                assert isEnable(node)
                    : "Called on node we don't manage: " + node;

            final byte value = node.getValue();
            if (haveUndefinedEnables)
                computeEnableState();
            else {
                switch (value) {
                    case Node.VALUE_0:
                        numEnablesSet -= aliasCount;
                        break;
                    case Node.VALUE_1:
                        numEnablesSet += aliasCount;
                        break;
                    case Node.VALUE_U:
                        haveUndefinedEnables = true;
                        break;
                }
            }
            assert haveUndefinedEnables ||
                   (0 <= numEnablesSet && numEnablesSet <= numEnables);

            switch (cycle_status) {
              case BEGIN:
                // Fallthrough
              case VALID:
                if (waitingOnChannel) { break; }
                if (areEnablesSet()) {
                    synchronized (BufferedNodeWriteChannel.this) {
                        // if writer will be unblocked after this, set the
                        // enabling node for critical path analysis
                        if (bc.isWriterBlocked()) setEnablingNode(node);

                        // If there's a token waiting to write, take it.

                        // NOTE: probeReceive() is explicitly *not* timed,
                        // because we want to act on any token (even a 
                        // future one).  moveToken() will take care of timing.
                        if (bc.probeReceive()) {
                            moveToken(time);
                            cycle_status = NEUTRAL;
                        } else {
                            enableTime = time;
                            waitingOnChannel = true;
                        }
                    }
                }
                break;
              case NEUTRAL:
                if (value == Node.VALUE_0) {
                    for (int i = 0; i < rails.length; i += 2) {
                        dropRails(rails[i], rails[i + 1], time + bc.bfLatency);
                    }
                    numEnablesReset += aliasCount;
                }
                if (areEnablesClear()) {
                    numEnablesReset = 0;
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

    private boolean areEnablesSet() {
        return numEnablesSet == numEnables && !haveUndefinedEnables;
    }

    private boolean areEnablesClear() {
        return numEnablesReset == numEnables && !haveUndefinedEnables;
    }

    private void computeEnableState() {
        numEnablesSet = 0;
        haveUndefinedEnables = false;
        for (int k=0;k<rlist.getLength();k++) {
            for (int i = 0; i < M; i++) {
                byte value = enable[k][i].getValue();
                switch (value) {
                    case Node.VALUE_1:
                        numEnablesSet++;
                        break;
                    case Node.VALUE_U:
                        haveUndefinedEnables = true;
                        // We don't care about the count if there are any
                        // undefined, so just return.
                        return;
                    // Do nothing if Node.VALUE_0
                }
            }
        }
    }

    private void watchEnables() {
        final MultiMap uniqueEnables =
            new MultiMap(new HashMap(), MultiMap.ARRAY_LIST_FACTORY);
        for (int k = 0; k < rlist.getLength(); k++) {
            for (int i = 0; i < M; i++) {
                uniqueEnables.put(enable[k][i], new int[] { k, i });
            }
        }

        for (Iterator i = uniqueEnables.keySet().iterator(); i.hasNext(); ) {
            final Node enable = (Node) i.next();
            final Collection rails = uniqueEnables.get(enable);
            final int indices[] = new int[rails.size() * 2];
            int k = 0;
            for (Iterator j = rails.iterator(); j.hasNext(); k += 2) {
                final int[] rail = (int[]) j.next();
                indices[k] = rail[0];
                indices[k + 1] = rail[1];
            }
            enable.addWatch(new EnableNodeWatcher(rails.size(), indices));
        }
    }

    private boolean isEnable(Node n) {
        for (int k=0;k<rlist.getLength();k++) {
            for (int i = 0; i < M; i++) {
                if (n == enable[k][i]) {
                    return true;
                }
            }
        }
        return false;
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

    /**
     * Drops all rails.  Used during instantiation and reset.
     **/
    private void dropAllRails() {
        for (int k=0;k<rlist.getLength();k++) {
            for(int i = 0; i < M; i++) {
                for(int j = 0; j < rlist.get(k); j++) {
                    rail[k][i][j].setValueAndEnqueueDependents(Node.VALUE_0);
                }
            }
        }
    }

    /**
     * Drops only the rails that <code>raisedRails</code> says are 
     * raised.
     **/
    private void dropRails(long time) {
        for (int i = 0; i < raisedRails.length; ++i)
            for (int j = 0; j < raisedRails[i].length; ++j)
                dropRails(i, j, time);
    }

    private void dropRails(int i, int j, long time) {
        rail[i][j][raisedRails[i][j]]
            .scheduleTimeOrRandom(Node.VALUE_0, time, getEnablingNode());
    }

    /**  Doesn't modify m in any way, but uses its timestamp. **/
    private void setRails(Message m, long time) {
        BigInteger b = m.getValue();
        long actualTime = Math.max(m.getTime(), time);
        for (int loop=0; loop<rlist.getLength(); loop++) {
            setRails(b.mod(rlist.getBigNpowM(loop)), loop, actualTime);
            b = b.divide(rlist.getBigNpowM(loop));
        }
    }
                   
    private void setRails(BigInteger b, int index, long time) {
        BigInteger [] accrems = new BigInteger [2];
        int line;

        accrems[0] = b.mod(rlist.getBigNpowM(index));
        //System.out.print("Setting "+name+" to: ");
        for(int i = 0; i < M; i++) {
            accrems = accrems[0].divideAndRemainder(rlist.getBigN(index));
            line    = accrems[1].intValue();
            raisedRails[index][i] = line;
            rail[index][i][line].scheduleTimeOrRandom(Node.VALUE_1, time,
                                                      getEnablingNode());
        }
    }

} // end of final class BufferedNodeWriteChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
