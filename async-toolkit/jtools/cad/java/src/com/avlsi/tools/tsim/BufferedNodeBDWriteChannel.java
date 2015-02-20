/*
 * Copyright 2001, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/tools/tsim/BufferedNodeWriteChannel.java#1 $
 * $DateTime: 2014/12/10 04:47:55 $
 * $Author: rliu68 $
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
 * <p> Class representing a BD channel with a
 * <code>BufferedChannel</code> writing to them. </p>
 *
 * <p> Only can be used in one of its roles at once. </p>
 *
 * <p> In cast components of channels are
 * <code>
 * name.C.a
 * name.C.q
 * name.D[]
 * </pre>
 * </p>
 *
 * <p> Expressed as pseudo-CSP, the conversion is:
 * <pre>
    function resetNodes() = (R.C.q-; R.D=0);
    *[[~R.C.a]; wait(toData); L?R.D; wait(fromData); R.C.q+;
      [ R.C.a]; wait(toData); L?R.D; wait(fromData); R.C.q-
     ]
 * </pre>

 *
 * @concurrency Synchronization lock protects cycle_status and waitingOnChannel.
 * @bug jmr  There is no lock used in send().
 *
 * @author Harry Liu
 * @version $Revision: #1 $ $Date: 2014/12/10 $
 **/

public final class BufferedNodeBDWriteChannel extends BufferedNodeBDChannel
    implements ChannelOutput, TSimDebug {

    private byte wantAckState;

    /**
     * Constructor.
     *
     * @param slack amount of slack in buffered part of channel
     * @param toData Req to data delay
     * @param fromData Data to ack delay
     * @param W width of the channel
     * @param name Base name of the channel
     *
     * @throws IllegalSlackException  If slack is nonpositive.
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeBDWriteChannel(int slack, int toData, int fromData,
                                      int ffLatency, int bbLatency,
                                      int fbLatency, int bfLatency,
                                      int cycleTimeIn, int cycleTimeOut,
                                      String name, int W, boolean startNow) {
        super(slack, toData, fromData, ffLatency, bbLatency, fbLatency,
              bfLatency, cycleTimeIn, cycleTimeOut, name, W, startNow);
    }

    protected void instantiate() {
        ackWatcher = new AckNodeWatcher();
        ack.addWatch(ackWatcher);
        if (resetNode != null) {
            resetNodeWatcher = new ResetNodeWatcher();
            resetNode.addWatch(resetNodeWatcher);
        } else {
            System.out.println("Warning: No reset node to watch.");
        }
        wantAckState = Node.VALUE_0;
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
            moveToken(dataTime);
            waitingOnChannel = false;
            cycle_status = WAIT_ACK;
            wantAckState = invert(wantAckState);
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
            setData(m, m.getTime());
            req.scheduleTime(invert(wantAckState), m.getTime() + fromData,
                             getEnablingNode());

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
            //if (cycle_status != BEGIN)
            //    System.out.println(
            //        "Warning: Reset called after starting channel");
            final byte value = node.getValue();
            if (value == Node.VALUE_0) {
                req.setValueAndEnqueueDependents(Node.VALUE_0);
                zeroData();
            } else if (value == Node.VALUE_1) {
                cycle_status = WAIT_ACK;
            }
        }
    }

    private final class DataEvent implements Event {
        private final long time;
        private int index;
        private DataEvent(final long time) {
            this.time = time;
            this.index = -1;
        }
        public void fire() {
            if (cycle_status == WAIT_DATA) {
                synchronized (BufferedNodeBDWriteChannel.this) {
                    if (bc.isWriterBlocked()) setEnablingNode(ack);
                    final long t = DigitalScheduler.get().getTime();
                    if (bc.probeReceive()) {
                        moveToken(t);
                        cycle_status = WAIT_ACK;
                        wantAckState = invert(wantAckState);
                    }
                    else {
                        dataTime = t;
                        waitingOnChannel = true;
                    }
                }
            }
        }
        public void setIndex(int index) {
            this.index = index;
        }
        public int getIndex() {
            return index;
        }
        public long getTime() {
            return time;
        }
        public boolean isRandom() {
            return false;
        }
        public String toString() {
            return "BD write wait from " + ack + " til data";
        }
    }

    private final class AckNodeWatcher implements NodeWatcher {
        public void nodeChanged(Node node, long time) {
            final byte val = node.getValue();

            switch (cycle_status) {
              case BEGIN:
              case WAIT_ACK:
                if (val == wantAckState) {
                    synchronized (BufferedNodeBDWriteChannel.this) {
                        cycle_status = WAIT_DATA;
                        DigitalScheduler.get().addEvent(
                                new DataEvent(time + toData));
                    }
                }
                break;
              case WAIT_DATA:
                break;
              case DESTROYED:
                throw new IllegalStateException("Channel " + getName() +
                        " has been destroyed");
              default:
                assert(false);
            }
        }
    }

    /**  Doesn't modify m in any way, but uses its timestamp. **/
    private void setData(Message m, long time) {
        final BigInteger bi = m.getValue();
        for (int i = 0; i < data.length; ++i) {
            final byte val = bi.testBit(i) ? Node.VALUE_1 : Node.VALUE_0;
            data[i].scheduleTime(val, time, getEnablingNode());
        }
    }

    /**  Doesn't modify m in any way, but uses its timestamp. **/
    private void zeroData() {
        for (int i = 0; i < data.length; ++i) {
            data[i].setValueAndEnqueueDependents(Node.VALUE_0);
        }
    }
} // end of final class BufferedNodeBDWriteChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
