/*
 * Copyright 2001, 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/tools/tsim/BufferedNodeBDReadChannel.java#1 $
 * $DateTime: 2014/12/10 04:47:55 $
 * $Author: rliu68 $
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
 * <p> Class representing a BD channel with a
 * <code>BufferedChannel</code> reading from them. </p>
 *
 * <p> Only can be used in one of its roles at once. </p>
 *
 * <p> In cast components of multibundle channels are:
 * <pre>
 * name.C.a
 * name.C.q
 * name.D[]
 * </pre>
 * </p>
 *
 * <p> Expressed as as pseudo-CSP, the hand shaking expansion is:
 * <pre>
 *  function resetNodes() = (L.C.a-);
 *  *[[ L.C.q]; wait(toData); R!L.D; wait(fromData); L.C.a+;
 *    [~L.C.q]; wait(toData); R!L.D; wait(fromData); L.C.a-
 *   ]
 * </pre>
 *
 * @concurrency Synchronization lock protects cycle_status and waitingOnChannel.
 *
 * @author Harry Liu
 * @version $Revision: #1 $ $Date: 2014/12/10 $
 **/

public final class BufferedNodeBDReadChannel extends BufferedNodeBDChannel
    implements ChannelInput, TSimDebug {

    private byte wantReqState;

    /**
     * Accumulator for value from the data rails.  When a data rail is
     * raised, its place value is added to this, and its enable is dropped.
     **/
    private BigInteger dataValue;

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
    public BufferedNodeBDReadChannel(int slack, int toData, int fromData,
                                     int ffLatency, int bbLatency,
                                     int fbLatency, int bfLatency,
                                     int cycleTimeIn, int cycleTimeOut,
                                     String name, int W, boolean startNow) {
        super(slack, toData, fromData, ffLatency, bbLatency, fbLatency,
              bfLatency, cycleTimeIn, cycleTimeOut, name, W, startNow);
    }

    protected void instantiate() {
        reqWatcher = new ReqNodeWatcher();
        req.addWatch(reqWatcher);
        if (resetNode != null) {
            resetNodeWatcher = new ResetNodeWatcher();
            resetNode.addWatch(resetNodeWatcher);
        } else {
            System.err.println("Warning:  No reset node to watch");
        }
        wantReqState = Node.VALUE_1;
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
                moveToken(dataTime);
                waitingOnChannel = false;
                dataValue = null;
                cycle_status = WAIT_REQ;
                wantReqState = invert(wantReqState);
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
                bc.checklessSend(new Message(dataValue, time), time);
            ack.scheduleTime(wantReqState, sendTime + fromData,
                             getEnablingNode());
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
            //    System.out.println ("Warning: Reset called after starting "+
            //                        "channel");
            final byte value = node.getValue();
            if (value == Node.VALUE_0) {
                ack.setValueAndEnqueueDependents(value);
            } else if (value == Node.VALUE_1) {
                cycle_status = WAIT_REQ;
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
                synchronized (BufferedNodeBDReadChannel.this) {
                    if (bc.isReaderBlocked()) setEnablingNode(req);

                    dataValue = BigInteger.ZERO;
                    for (int i = 0; i < data.length; ++i) {
                        final byte val = data[i].getValue();
                        if (val == Node.VALUE_1) {
                            dataValue = dataValue.setBit(i);
                        } else if (val == Node.VALUE_U) {
                            throw new IllegalStateException(
                                "Node " + data[i].getName() +
                                " has undefined value");
                        }
                    }
                    final long t = DigitalScheduler.get().getTime();
                    if (bc.probeSend()) {
                        moveToken(t);
                        cycle_status = WAIT_REQ;
                        wantReqState = invert(wantReqState);
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
            return "BD read wait from " + req + " til data";
        }
    }

    private final class ReqNodeWatcher implements NodeWatcher {
        public void nodeChanged(Node node, long time) {
            final byte val = node.getValue();

            switch (cycle_status) {
              case BEGIN:
              case WAIT_REQ:
                if (val == wantReqState) {
                    synchronized (BufferedNodeBDReadChannel.this) {
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

} // end of final class BufferedNodeBDReadChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
