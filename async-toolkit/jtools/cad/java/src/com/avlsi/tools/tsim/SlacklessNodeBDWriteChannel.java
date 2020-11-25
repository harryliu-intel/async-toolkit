/*
 * Copyright 2001, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/tools/tsim/SlacklessNodeReadChannel.java#1 $
 * $DateTime: 2014/12/10 04:47:55 $
 * $Author: rliu68 $
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

public final class SlacklessNodeBDWriteChannel extends SlacklessNodeBDChannel
    implements ChannelOutput, TSimDebug {

    private byte wantAckState;

    /** Reference to dsim **/
    private DSim sim;

    protected void instantiate() {
        sim = DSim.get();
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

    /**
     * Constructor.
     *
     * @param name Base name of the channel
     * @param W    width of the channel
     **/
    public SlacklessNodeBDWriteChannel(int toData, int fromData, int portOffset,
                                       String name, int W) {
        this(toData, fromData, portOffset, name, W, true);
    }

    public SlacklessNodeBDWriteChannel(int toData, int fromData, int portOffset,
                                       String name, int W, boolean startNow) {
        super(toData, fromData, portOffset, name, W, startNow);
    }

    ///////////////////////  Channel interfaces.
    // Channel output.

    //XXX:  Not sure that this is correct for a slackless
    //channel in all cases
    public long getSendTime() {
        return sim.getTime();
    }

    /**
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    private synchronized void waitForAck() throws InterruptedException {
        if (probeSend()) {
            return;
        }
        sleep_status = WAIT_ACK;
        DigitalScheduler.get().decThreadCount();
        while (!probeSend()) {
            this.wait();
        }
        if (sim.getVerbose())
            System.err.println("SNRC: Done sleeping");
    }

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
        waitForAck();

        long t = Math.max(m.getTime(),sim.getTime());
        setData(m, t);
        wantAckState = invert(wantAckState);
        cycle_status = WAIT_ACK;
        req.scheduleTime(wantAckState, t + fromData,
                         getEnablingNode());
    }

    public boolean probeSend(long time) {
        throw new AssertionError("Correct implementation not known");
    }

    public boolean probeSend() {
        return cycle_status == WAIT_DATA;
    }

    public boolean checklessProbeSend(long time) {
        throw new AssertionError("Correct implementation not known");
    }

    public boolean checklessProbeSend() {
        return probeSend();
    }

    public synchronized void clearWriteWaiter() {
        Debug.assertTrue(sleep_status == SELECT);
        selector = null;
        sleep_status = NONE;
    }

    public synchronized void setWriteWaiter(WaiterInterface w) {
        Debug.assertTrue(sleep_status == NONE);
        selector = w;
        sleep_status = SELECT;
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
                if (ack.getValue() == wantAckState) {
                    waitData(time);
                }
            }
        }
    }

    private synchronized void wakeUp() {
        Debug.assertTrue(sleep_status != NONE);
        DigitalScheduler.get().incThreadCount();
        sleep_status = NONE;
        notify();
    }

    private final class DataEvent implements Event {
        private final long time;
        private int index;
        private DataEvent(final long time) {
            this.time = time;
            this.index = -1;
        }
        public void fire() {
            if (cycle_status == WAIT_ACK) {
                synchronized (SlacklessNodeBDWriteChannel.this) {
                    cycle_status = WAIT_DATA;
                    if (sleep_status == WAIT_ACK) {
                        setEnablingNode(ack);
                        wakeUp();
                    } else if (sleep_status == SELECT) {
                        selector.wakeUp();
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

    private synchronized void waitData(long time) {
        DigitalScheduler.get().addEvent(new DataEvent(time + toData));
    }

    private final class AckNodeWatcher implements NodeWatcher {
        public void nodeChanged(Node node, long time) {
            final byte val = node.getValue();

            switch (cycle_status) {
              case BEGIN:
                break;
              case WAIT_ACK:
                if (val == wantAckState) {
                    waitData(time);
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

} // end of final class SlacklessNodeBDWriteChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
