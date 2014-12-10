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

import com.avlsi.util.debug.Debug;

/**
 * <p> Class representing (arrays of) E1ofN channels. </p>
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
 * @concurrency Synchronization lock protects sleep_status and selector.
 *
 * @author Aaron Denney
 * @version $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public final class SlacklessNodeReadChannel extends SlacklessNodeChannel
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
     * Number of data rails that are high.  Only meaningful if
     * <code>haveUndefinedRails</code> is false.
     * 
     * <pre><jml>
     *     private invariant
     *         !haveUndefinedRails ==>
     *             numRailsSet ==
     *                 (\num_of int i, j, k; 0 <= i && i < rlist.getLength() &&
     *                                       0 <= j && j < M &&
     *                                       0 <= k && k < rlist.get(i);
     *                                        rail[i][j][k].getValue() ==
     *                                            Node.VALUE_1);
     * </jml></pre>
     **/
    private int numRailsSet;

    /**
     * Number of subchannels that exist.
     * 
     * <pre><jml>
     *     private invariant
     *         numChannels == rlist.getLength() * M;
     * </jml></pre>
     **/
    private final int numChannels;

    /** Reference to dsim **/
    private DSim sim;

    /**
     * Constructor.
     *
     * Standard 32 bit channel is (Name, 4, 16).
     *
     * @param name Base name of the channel.
     * @param N    wires per enable.
     * @param M    bundles of wire
     *
     **/
    public SlacklessNodeReadChannel(String name, int N, int M) {
        this(name, new RailList(N), M,-1, true);
    }

    protected void instantiate() {
        sim = DSim.get();
        watchRails();
        if (resetNode != null) {
            resetNodeWatcher = new ResetNodeWatcher();
            resetNode.addWatch(resetNodeWatcher);
        } else {
            System.out.println("Warning: No reset node was found.");
            setEnables(Node.VALUE_1, 0);
        }
    }

    /**
     * Constructor.
     *
     * @param name Base name of the channel.  Components are 
     *             name + ".e", and name + ".d[" + {1,...,N} + "]".
     * @param N    wires per (1) enable.
     **/
    public SlacklessNodeReadChannel(String name, int N) {
        this(name, new RailList(N), 1,-1, true);
    }

    public SlacklessNodeReadChannel(String name, RailList list, int M,
                                    int index, boolean startNow) {
        super(name, list, M, index, startNow);
        this.haveUndefinedRails = true;
        this.numRailsSet = 0;
        this.numChannels = rlist.getLength() * M;
    }
    ///////////////////////  Channel interfaces.
    // Channel input.
    //
    //
    //XXX:  Not sure if this is correct for a
    //slackless channel in all cases
    public long getReceiveTime() {
        return sim.getTime();
    }

    public boolean probeReceive(long time) {
        throw new AssertionError("Correct implementation not known.");
    }

    public boolean probeReceive() {
        return railsValid();
    }

    public boolean checklessProbeReceive(long time) {
        throw new AssertionError("Correct implementation not known.");
    }

    public boolean checklessProbeReceive() {
        return probeReceive();
    }
    public Message probeValue() {
        if (railsValid()) {
            return new Message(railsValue(), DSim.get().getTime());
        }
        return null;
    }

    /**
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public Message receive(long deviceTime) throws InterruptedException {
        if (deviceTime > sim.getTime()) {
	    if (sim.getVerbose())
	        System.err.println("SNRC: Sleeping for DSim: " + deviceTime +
			" > " + sim.getTime());
            new WakeAt(deviceTime, this).sleepTil();
        }
        return receive();
    }

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public synchronized void waitForMessage() throws InterruptedException {
	sleepTilRailsValid();
    }

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public synchronized Message receive () throws InterruptedException {
        sleepTilRailsValid();

        BigInteger value = railsValue();

        // FIXME: 1 is a hack; should be replaced by proper timing offset
        setEnables(Node.VALUE_0, 1);
	if (sim.getVerbose())
	    System.err.println("Got value " + value.toString());

	sleepTilRailsNeutral();

	if (sim.getVerbose())
	    System.err.println("SNRC: Done waiting for rails neutral.");

        return new Message(value, sim.getTime());
    }

    // How will selecting work?
    public synchronized void clearReadWaiter() {
        Debug.assertTrue(sleep_status == SELECT);
        selector = null;
        sleep_status=NONE;
    }

    public synchronized void setReadWaiter(WaiterInterface w) {
        Debug.assertTrue(sleep_status == NONE);
        selector = w;
        sleep_status = SELECT;
    }

    private final class ResetNodeWatcher implements NodeWatcher {
        public void nodeChanged(Node node, long time) {
            if (sim.getVerbose())
                System.err.println("SNRC node changed, sleep_status " + sleep_status);
            if (DEBUG)
                 assert node == resetNode
                     : "ResetNodeWatcher called on non-Reset node " + node;
            if (sleep_status != NONE) {
                System.out.println("Warning: Reset called while sleeping in "+
                                   "channel!");
            }
            setEnables(node.getValue(), 0);
        }
    }

    private final class DataRailNodeWatcher implements NodeWatcher {
        public void nodeChanged(Node node, long time) {
            if (sim.getVerbose())
                System.err.println("SNRC node changed, sleep_status " + sleep_status);
            if (DEBUG)
                assert isRail(node)
                    : "Called on node we don't manage: " + node;

            if (haveUndefinedRails)
                computeRailState();
            else {
                final byte value = node.getValue();
                switch (value) {
                    case Node.VALUE_0:
                        numRailsSet--;
                        break;
                    case Node.VALUE_1:
                        numRailsSet++;
                        break;
                    case Node.VALUE_U:
                        haveUndefinedRails = true;
                        break;
                }
            }

            switch (sleep_status) {
              case VALID:
                if (railsValid()) {
                    wakeUp();
                }
                break;
              case SELECT:
                if (railsValid()) {
                    selector.wakeUp();
                }
                break;
              case NEUTRAL:
                if (railsNeutral()) {
                    // FIXME: 1 is a hack; should be replaced by
                    // proper timing offset
                    setEnables(Node.VALUE_1, 1);
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

    private void watchRails() {
        final NodeWatcher dataRailNodeWatcher = new DataRailNodeWatcher();
        for (int k=0;k< rlist.getLength();k++) {
            for (int i = 0; i < M; i++) {
                for (int j = 0; j < rlist.get(k); j++) {
                    rail[k][i][j].addWatch(dataRailNodeWatcher);
                }
            }
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
        numRailsSet = 0;
        haveUndefinedRails = false;
        for (int k=0;k<rlist.getLength();k++) {
            for (int i = 0; i < M; i++) {
                for (int j = 0; j < rlist.get(k); j++) {
                    byte value = rail[k][i][j].getValue();
                    switch (value) {
                        case Node.VALUE_1:
                            numRailsSet++;
                            break;
                        case Node.VALUE_U:
                            haveUndefinedRails = true;
                            // We don't care about the count if there are any
                            // undefined, so just return.
                            return;
                        // Do nothing if Node.VALUE_0
                    }
                }
            }
        }
    }

    /** This should behave like the old setEnables() when delay is 0. **/
    public void setEnables(byte value, int delay) {
	// System.err.println("Setting enables for " + name + " to " + value);
        for( int k=0;k< rlist.getLength();k++) {
            for (int i = 0; i < M; i++) {
                enable[k][i].scheduleDelayOrRandom(value, delay);
            }
        }
    }

    /**
     * Decodes this bundle of lines into a BigInteger.
     * 
     * Nonsense answers when !railsValid();
     **/
    private BigInteger railsValue() {
        BigInteger num = BigInteger.ZERO;
        BigInteger multfactor  = BigInteger.ONE;
        for(int k=0;k<rlist.getLength();k++) {
            BigInteger numk = railsValue(k).multiply(multfactor);
            num = numk.add(num);
            multfactor = multfactor.multiply(rlist.getBigNpowM(k));
        }
        return num;
    }
    
    private BigInteger railsValue(int index) {
        BigInteger rv = BigInteger.ZERO;
        BigInteger multiplier = BigInteger.ONE;
        BigInteger temp;
        for (int i = 0; i < M; i++) {
            for (int j = 0; j < rlist.get(index); j++) {
                int val = rail[index][i][j].getValue();
                if (val == Node.VALUE_1) {
                    temp = BigInteger.valueOf(j).multiply(multiplier);
                    rv = rv.add(temp);
                    break;
                }
            }
            multiplier = multiplier.multiply(rlist.getBigN(index));
        }
        return rv;
    }

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    private void sleepTilRailsValid() throws InterruptedException {
	if (sim.getVerbose())
	    System.err.println("SNRC: Sleeping til rails valid");
        if (railsValid()) {
	    if (sim.getVerbose())
	        System.err.println("SNRC: Rails were valid");
	    return;
	}
        sleep_status = VALID;
        DigitalScheduler.get().decThreadCount();
        while(sleep_status == VALID || !railsValid()) {
            this.wait();
        }
	if (sim.getVerbose()) 
	    System.err.println("SNRC: Done sleeping");
    }

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    private void sleepTilRailsNeutral() throws InterruptedException {
        if (railsNeutral()) {
            // FIXME: 1 is a hack; should be replaced by proper timing offset
            setEnables(Node.VALUE_1, 1);
            return;
	}
        sleep_status = NEUTRAL;
        DigitalScheduler.get().decThreadCount();
        while (sleep_status == NEUTRAL || !railsNeutral()) {
            this.wait();
	}
    }

    private synchronized void wakeUp() {
        Debug.assertTrue(sleep_status != NONE);
        DigitalScheduler.get().incThreadCount();
        sleep_status = NONE;
        notify();
    }

} // end of final class SlacklessNodeReadChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
