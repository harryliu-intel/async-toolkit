/*
 * Copyright 2001, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;

import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.Event;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.NodeWatcher;
import com.avlsi.tools.dsim.WakeAt;
import com.avlsi.tools.tsim.RailList;
import com.avlsi.util.debug.Debug;

/**
 * <p> Class representing E1ofN channels. </p>
 *
 * <p> Only can be used in one of its roles at once. </p>
 *
 * <p> In cast components of multibundle channels are:
 * <pre>
 * name[i].e
 * name[i].d[j]
 * </pre>
 * one-bundle channels omit the i. </p>
 *
 * @concurrency Synchronization lock protects sleep_status and selector.
 *
 * @author Aaron Denney
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public final class SlacklessNodeWriteChannel extends SlacklessNodeChannel
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
     *         sleep_status == NEUTRAL ==>
     *             (\forall int i, j; 0 <= i && i < rlist.getLength() &&
     *                                0 <= j && j < M;
     *                                rail[i][j][raisedRails[i][j]]
     *                                    .getValue() == Node.VALUE_1);
     *     private invariant
     *         sleep_status == NEUTRAL ==>
     *             (\forall int i, j, k; 0 <= i && i < rlist.getLength() &&
     *                                   0 <= j && j < M &&
     *                                   0 <= k && k < rlist.get(i) &&
     *                                   k != raisedRails[i][j];
     *                                rail[i][j][raisedRails[i][j]]
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

     /** Reference to DSim **/
     private DSim sim = DSim.get();
    
     /**
     * Constructor.
     *
     * Standard 32 bit channel is (Name, 4, 16).
     *
     * @param name Base name of the channel.
     * @param N    wires per enable.
     * @param M    bundles of wire.
     *
     **/
    public SlacklessNodeWriteChannel(String name, int N, int M) {
        this(name, new RailList(N), M ,-1,true);
    }

    protected void instantiate() {
        dropAllRails(0);
        watchEnables();
	if (resetNode != null) {
            resetNodeWatcher = new ResetNodeWatcher();
            resetNode.addWatch(resetNodeWatcher);
        } else	
	    System.out.println("Warning: No reset node was found.");
    }

    /**
     * Constructor.
     *
     * @param name Base name of the channel.  Components are 
     *             name + ".e", and name + ".d[" + {1,...,N} + "]".
     * @param N    wires per (1) enable.
     **/
    public SlacklessNodeWriteChannel(String name, int N) {
        this(name,new RailList(N),1, -1,true);
    }

    public SlacklessNodeWriteChannel(String name, RailList list, int M,
                                     int index, boolean startNow) {
        super(name, list, M, index, startNow);
        this.raisedRails = new int[rlist.getLength()][M];
        this.haveUndefinedEnables = true;
        this.numEnablesSet = 0;
        this.numEnables = rlist.getLength() * M;
    }
    ///////////////////////  Channel interfaces.
    // Channel output.

    //XXX:  Not sure that this is correct for a slackless
    //channel in all cases
    public long getSendTime() {
        return sim.getTime();
    }

    /**
     * Ignores cycle time.
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public long send(Message m, long deviceTime)
        throws InterruptedException {
        m = m.updateMessageTime(deviceTime);
        send(m);
        return sim.getTime();
    }

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public synchronized void send(Message m) throws InterruptedException {
        sleepTilEnablesSet();

        if(m.getTime() > sim.getTime()) {
	    if (sim.getVerbose())
	        System.err.println("SNWC: Sleeping for DSim: " + m.getTime() +
			" > " + sim.getTime());
            new WakeAt(m.getTime(), this).sleepTil();
        }

        // set the lines.
	if (sim.getVerbose())
	    System.err.println("Setting rails");
        // FIXME: 1 is a hack; should be replaced by proper timing offset
        setRails(m,1);
        sleep_status = NEUTRAL;

        DigitalScheduler.get().decThreadCount();
        while (sleep_status == NEUTRAL) {
            this.wait();
        }
    }

    public boolean probeSend(long time) {
        throw new AssertionError("Correct implementation not known");
    }

    public boolean probeSend() {
        return areEnablesSet();
    }

    public boolean checklessProbeSend(long time) {
        throw new AssertionError("Correct implementation not known");
    }

    public boolean checklessProbeSend() {
        return probeSend();
    }
    
    public synchronized void clearWriteWaiter() {
        Debug.assertTrue(sleep_status == SELECT);
        Debug.assertTrue(selector != null);
        selector = null;
        sleep_status = NONE;
    }

    public synchronized void setWriteWaiter(WaiterInterface w) {
        Debug.assertTrue(sleep_status == NONE);
        Debug.assertTrue(selector == null);
        sleep_status = SELECT;
        selector = w;
    }

    private final class ResetNodeWatcher implements NodeWatcher {
        public synchronized void nodeChanged(Node node, long time) {
            if (DEBUG)
                assert node == resetNode
                     : "ResetNodeWatcher called on non-Reset node " + node;
            if (sleep_status != NONE) {
                System.out.println("Warning: Reset called while sleeping in "+
                                   "channel!");
            }
            if (node.getValue() == Node.VALUE_0) {
                // FIXME: 1 is a hack; should be replaced by proper
                // timing offset
                dropAllRails(1);
            }
        }
    }

    private final class EnableNodeWatcher implements NodeWatcher {
        private final int aliasCount;

        private EnableNodeWatcher(final int aliasCount) {
            this.aliasCount = aliasCount;
        }

        /**
         * @throws IllegalStateException
         *         If the channel has been destroyed.
         **/
        public synchronized void nodeChanged(Node node, long time) {
            if (DEBUG)
                assert isEnable(node)
                    : "Called on node we don't manage: " + node;

            if (haveUndefinedEnables)
                computeEnableState();
            else {
                final byte value = node.getValue();
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

            switch (sleep_status) {
              case VALID:
                if (areEnablesSet()) {
                    wakeUp();
                }
                break;
              case SELECT:
                if (areEnablesSet()) {
                    selector.wakeUp();
                }
                break;
              case NEUTRAL:
                if (areEnablesClear()) {
                    // FIXME: 1 is a hack; should be replaced by
                    // proper timing offset
                    dropRails(1);
                    wakeUp();
                }
              case NONE:
                break;
              case DESTROYED:
                throw new IllegalStateException("Channel " + getName() +
                        " has been destroyed");
              default:
                Debug.assertTrue(false);
            }
        }
    }

    private synchronized void wakeUp() {
        Debug.assertTrue(sleep_status != NONE);
        DigitalScheduler.get().incThreadCount();
        sleep_status = NONE;
        notify();
    }

    private boolean areEnablesSet() {
        return numEnablesSet == numEnables && !haveUndefinedEnables;
    }

    private boolean areEnablesClear() {
        return numEnablesSet == 0 && !haveUndefinedEnables;
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

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    private void sleepTilEnablesSet() throws InterruptedException {
	if (sim.getVerbose())
	    System.err.println("SNWC: Sleeping for enables");
        if (areEnablesSet()) { 
	    if (sim.getVerbose())
	        System.err.println("SNWC: Enables were set");
	    return;
	}
        sleep_status = VALID;
        DigitalScheduler.get().decThreadCount();
        while(sleep_status == VALID || !areEnablesSet()) {
            this.wait();
        }
	if (sim.getVerbose())
	    System.err.println("SNWC: Done sleeping");
    }

    private void watchEnables() {
        int[][] aliasCount = new int[rlist.getLength()][M];
        boolean[][] dontWatch = new boolean[rlist.getLength()][M];

        for (int k=0;k< rlist.getLength();k++) {
            for (int i = 0; i < M; i++) {
                for (int kk = 0; kk <= k; kk++) {
                    int max = kk == k ? i : M;
                    for (int ii = 0; ii < max; ii++) {
                        if (enable[k][i] == enable[kk][ii]) {
                            dontWatch[k][i] = true;
                            aliasCount[k][i]++;
                            aliasCount[kk][ii]++;
                        }
                    }
                }
            }
        }

        enableNodeWatchers = new NodeWatcher[rlist.getLength()][M];
        for (int k=0;k< rlist.getLength();k++) {
            for (int i = 0; i < M; i++) {
                if (!dontWatch[k][i]) {
                    enableNodeWatchers[k][i] = 
                        new EnableNodeWatcher(aliasCount[k][i] + 1);
                    enable[k][i].addWatch(enableNodeWatchers[k][i]);
                }   
            }
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
     * 
     * If delay is 0, should behave like the old dropRails().
     **/
    private void dropAllRails(int delay) {
        for (int k=0;k<rlist.getLength();k++) {
            for(int i = 0; i < M; i++) {
                for(int j = 0; j < rlist.get(k); j++) {
                    rail[k][i][j].scheduleDelay(Node.VALUE_0, delay);
                }
            }
        }
    }

    /**
     * Drops only the rails that <code>raisedRails</code> says are 
     * raised.
     **/
    private void dropRails(int delay) {
        for (int i = 0; i < raisedRails.length; ++i)
            for (int j = 0; j < raisedRails[i].length; ++j)
                rail[i][j][raisedRails[i][j]]
                    .scheduleDelay(Node.VALUE_0, delay);
    }

    /** If delay is 0, should behave like the old setRails(). **/
    private void setRails(Message m, long delay) {
        BigInteger b = m.getValue();
        long offset = Math.max(m.getTime(), sim.getTime()+delay);
        for (int loop=0;loop<rlist.getLength();loop++) {
            setRails(b.mod(rlist.getBigNpowM(loop)),loop,offset);
            b = b.divide(rlist.getBigNpowM(loop));
        }
    }

    private void setRails(BigInteger b, int index, long offset) {
        BigInteger [] accrems = new BigInteger [2];
        int line;

	if (sim.getVerbose())
	    System.err.println("Setting rails for " + b.toString());
        accrems[0] = b.mod(rlist.getBigNpowM(index));
        for(int i = 0; i < M; i++) {
            accrems = accrems[0].divideAndRemainder(rlist.getBigN(index));
            line    = accrems[1].intValue();
            raisedRails[index][i] = line;
            rail[index][i][line].scheduleTimeOrRandom(Node.VALUE_1, offset);
        }
    }

} // end of final class SlacklessNodeWriteChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
