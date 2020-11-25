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

public final class SlacklessNodeBDReadChannel extends SlacklessNodeBDChannel
    implements ChannelInput, TSimDebug {

    private byte wantReqState;

    /**
     * Accumulator for value from the data rails.
     **/
    private BigInteger dataValue;

    /** Reference to dsim **/
    private DSim sim;

    protected void instantiate() {
        sim = DSim.get();
        reqWatcher = new ReqNodeWatcher();
        req.addWatch(reqWatcher);
        if (resetNode != null) {
            resetNodeWatcher = new ResetNodeWatcher();
            resetNode.addWatch(resetNodeWatcher);
        } else {
            System.err.println("Warning: No reset node to watch");
        }
        wantReqState = Node.VALUE_1;
    }

    /**
     * Constructor.
     *
     * @param name Base name of the channel
     * @param W    width of the channel
     **/
    public SlacklessNodeBDReadChannel(int toData, int fromData, int portOffset,
                                      String name, int W) {
        this(toData, fromData, portOffset, name, W, true);
    }

    public SlacklessNodeBDReadChannel(int toData, int fromData, int portOffset,
                                      String name, int W, boolean startNow) {
        super(toData, fromData, portOffset, name, W, startNow);
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
        return cycle_status == WAIT_ACK;
    }

    public boolean checklessProbeReceive(long time) {
        throw new AssertionError("Correct implementation not known.");
    }

    public boolean checklessProbeReceive() {
        return probeReceive();
    }

    public Message probeValue() {
        if (probeReceive()) {
            return new Message(dataValue, DSim.get().getTime());
        }
        return null;
    }

    /**
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public Message receive(long deviceTime) throws InterruptedException {
        if (deviceTime > sim.getTime()) {
            if (sim.getVerbose()) {
                System.err.println("SNRC: Sleeping for DSim: " + deviceTime +
                                               " > " + sim.getTime());
                new WakeAt(deviceTime, this).sleepTil();
            }
        }
        return receive();
    }

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public synchronized void waitForMessage() throws InterruptedException {
        if (probeReceive()) {
            return;
        }
        sleep_status = WAIT_DATA;
        DigitalScheduler.get().decThreadCount();
        while (!probeReceive()) {
            this.wait();
        }
        if (sim.getVerbose()) 
            System.err.println("SNRC: Done sleeping");
    }

    /** 
     * @todo Undocumented.
     * @throws InterruptedException When?
     **/
    public synchronized Message receive () throws InterruptedException {
        waitForMessage();

        if (sim.getVerbose())
            System.err.println("Got value " + dataValue.toString());

        final long time = sim.getTime();
        ack.scheduleTime(wantReqState, time + fromData, getEnablingNode());
        wantReqState = invert(wantReqState);
        cycle_status = WAIT_REQ;

        return new Message(dataValue, time);
    }

    public synchronized void clearReadWaiter() {
        Debug.assertTrue(sleep_status == SELECT);
        selector = null;
        sleep_status = NONE;
    }

    public synchronized void setReadWaiter(WaiterInterface w) {
        Debug.assertTrue(sleep_status == NONE);
        selector = w;
        sleep_status = SELECT;
    }

    private final class ResetNodeWatcher implements NodeWatcher {
        public void nodeChanged(Node node, long time) {
            assert node == resetNode :
                "ResetNodeWatcher called on non-Reset node " + node;
            if (sleep_status != NONE) {
                System.out.println("Warning: Reset called while sleeping in "+
                                   "channel " + getName());
            }
            final byte value = node.getValue();
            if (value == Node.VALUE_0) {
                ack.setValueAndEnqueueDependents(value);
            } else if (value == Node.VALUE_1) {
                cycle_status = WAIT_REQ;
                if (req.getValue() == wantReqState) {
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
            if (cycle_status == WAIT_DATA) {
                synchronized (SlacklessNodeBDReadChannel.this) {
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

                    cycle_status = WAIT_ACK;
                    if (sleep_status == WAIT_DATA) {
                        setEnablingNode(req);
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
            return "BD read wait from " + req + " til data";
        }
    }

    private synchronized void waitData(long time) {
        cycle_status = WAIT_DATA;
        DigitalScheduler.get().addEvent(new DataEvent(time + toData));
    }

    private final class ReqNodeWatcher implements NodeWatcher {
        public void nodeChanged(Node node, long time) {
            final byte val = node.getValue();

            switch (cycle_status) {
              case BEGIN:
                break;
              case WAIT_REQ:
                if (val == wantReqState) {
                    waitData(time);
                }
                break;
              case WAIT_DATA:
                sim.ui_out("Channel " + getName() + " req changing before ack");
                break;
              case WAIT_ACK:
                sim.ui_out("Channel " + getName() + " req changing before ack");
                break;
              case DESTROYED:
                throw new IllegalStateException("Channel " + getName() +
                        " has been destroyed");
              default:
                assert(false);
            }
        }
    }

} // end of final class SlacklessNodeBDReadChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
