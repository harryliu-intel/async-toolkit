/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/tools/tsim/BufferedNodeChannel.java#1 $
 * $DateTime: 2014/12/10 04:47:55 $
 * $Author: rliu68 $
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.NodeWatcher;

import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DSimUtil;
import java.math.BigInteger;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.text.StringUtil;

/**
 * Bundled data, transition-signaling, asynchronous channel.
 *
 * @author Harry Liu
 * @version $Revision: #1 $ $Date: 2014/12/10 $
 **/

public abstract class BufferedNodeBDChannel implements Statusable, ChannelStatus {
    
    protected final Node[]  data;
    protected       Node    ack;
    protected       Node    req;
    protected final int     toData, fromData;

    protected static final int BEGIN = 0, WAIT_REQ = 1, WAIT_ACK = 2,
                               WAIT_DATA = 3, DESTROYED = 4;
    /**
     * Indicates either that the channel has just begun or has ended,
     * or that it's waiting for data, request, or acknowlodge.
     **/
    protected int cycle_status = BEGIN;

    protected Node resetNode;

    protected boolean connect_status = false; //Not Connected
    
    protected String name;

    /** Time at which data can be sent or read */
    protected long dataTime;

    /**
     * The number of different values this channel can transmit.  If
     * it's an even power of 2, it's equal to 2^bitwidth.  If the
     * channel can transmit any arbitrary BigInteger, this value is
     * -1.  This isn't used in any way yet.  Messages are truncated by
     * the dsim side of the channel, silently.
     **/
    private final BigInteger numPossibleValues;

    protected NodeWatcher resetNodeWatcher;
    protected NodeWatcher reqWatcher;
    protected NodeWatcher ackWatcher;

    /** Internal channel **/
    protected final BufferedChannel bc;

    /** If the internal channel isn't able to handle the waiting dsim state **/
    protected boolean waitingOnChannel = false;

    /** The device the channel end connects to **/
    protected AbstractDevice owner = null;

    /**
     * @throws IllegalSlackException  If slack is nonpositive.
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeBDChannel(int slack, int toData, int fromData,
                                 int ffLatency, int bbLatency,
                                 int fbLatency, int bfLatency,
                                 int cycleTimeIn, int cycleTimeOut,
                                 String name, int W, boolean startNow) {
         connect_status = startNow;
         this.name = name;
         this.toData = toData;
         this.fromData = fromData;
         data = new Node[W];
         numPossibleValues = BigInteger.valueOf(2).pow(W);
         if (startNow) instantiateNM();
         bc = new BufferedChannel(slack,
                                  ffLatency, bbLatency, fbLatency, bfLatency,
                                  cycleTimeIn, cycleTimeOut,
                                  0,  // debug
                                  name + " internal channel",
                                  numPossibleValues);
    }
    
    private Node findNodeOrDie(final String name) {
        final Node node = DSim.get().findNode(name);
        if (node == null) {
            System.err.println("Can't find " + name);
            throw new NoSuchNodeException(name);
        }
        return node;
    }

    /**
     * @throws NoSuchNodeException
     **/
    protected void instantiateNM() {
        resetNode = DSimUtil.getResetNode();
        final String fullname = StringUtil.replaceSubstring(name, "][", ",");
        ack = findNodeOrDie(fullname + ".C.a");
        req = findNodeOrDie(fullname + ".C.q");
        for (int i = 0; i < data.length; ++i) {
            data[i] = findNodeOrDie(fullname + ".D[" + i + "]");
        }
        instantiate();
    }
        
    protected abstract void instantiate();
    
    /**
     * @throws NoSuchNodeException
     **/
    public void bind() {
        if (!connect_status) {
            connect_status = true;
            instantiateNM();
        }
    }

    public String getName() {
        return name;
    }

    public void setName(final String newName) {
        name = newName;
    }

    public BigInteger getNumPossibleValues() {
        return numPossibleValues;
    }

    public long getCycleTime() {
        return bc.getCycleTime();
    }

    /*
     * Used to get rid of this channel.
     *
     * Stops watching nodes, changes state to invalid, etc.
     *
     * Should only be used when resetting the associated circuit.
     */
    public synchronized void destroy() {
        if (reqWatcher != null) {
            req.removeWatch(reqWatcher);
        }

        if (ackWatcher != null) {
            ack.removeWatch(ackWatcher);
        }

        if (resetNodeWatcher != null) {
            resetNode.removeWatch(resetNodeWatcher);
        }

        cycle_status = DESTROYED;
    }

    public void printStatus() {
        bc.printStatus();
    }

    public void setOwner(final AbstractDevice owner) {
        this.owner = owner;
    }

    protected void setEnablingNode(final Node node) {
        if (owner != null) owner.setEnablingNode(node);
    }

    protected Node getEnablingNode() {
        return owner == null ? null : owner.getEnablingNode();
    }

    public int getCapacity() {
        return bc.getCapacity();
    }   

    public int getAvailable() {
        return bc.getAvailable();
    }   

    public String getVerboseStatus() {
        return bc.getVerboseStatus();
    }

    public void incCount() {
        bc.incCount();
    }

    public int getCount() {
        return bc.getCount();
    }

    public static byte invert(final byte state) {
        if (state == Node.VALUE_0) {
            return Node.VALUE_1;
        } else if (state == Node.VALUE_1) {
            return Node.VALUE_0;
        } else {
            return Node.VALUE_U;
        }
    }
} // end of abstract class BufferedNodeBDChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
