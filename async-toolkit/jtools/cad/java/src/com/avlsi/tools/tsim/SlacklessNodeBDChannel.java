// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/tools/tsim/SlacklessNodeChannel.java#1 $
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
 * <p> Base class for <code>SlacklessNodeBD*Channel</code>.
 **/
public abstract class SlacklessNodeBDChannel implements ChannelStatus, Ownable {
    
    protected final Node[]  data;
    protected       Node    ack;
    protected       Node    req;

    protected final int     toData, fromData, portOffset;
    protected static final int BEGIN = 0, NONE = 0, WAIT_REQ = 1, WAIT_ACK = 2,
                               WAIT_DATA = 3, SELECT = 4, DESTROYED = 5;

    protected Node resetNode;

    protected int cycle_status = BEGIN;

    protected int sleep_status = NONE;

    protected boolean connect_status = false; //Not Connected
    
    protected WaiterInterface selector = null;

    protected String name;

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

    /** The device the channel end connects to **/
    protected AbstractDevice owner = null;

    /**
     * Class constructor.
     *
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public SlacklessNodeBDChannel(int toData, int fromData, int portOffset,
                                  String name, int W, boolean startNow) {
        connect_status = startNow;
        this.name = name;
        this.data = new Node[W];
        this.toData = toData;
        this.fromData = fromData;
        this.portOffset = portOffset;
        if (startNow) instantiateNM();
        numPossibleValues = BigInteger.valueOf(2).pow(W);
    }

    public BigInteger getNumPossibleValues() {
        return numPossibleValues;
    }

    /** These are slackless channels, so don't really have cycle times **/
    public long getCycleTime () {
        return 0;
    }
    
    public int getPortOffset() {
        return portOffset;
    }

    /**
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    protected void instantiateNM() {
        resetNode = DSimUtil.getResetNode();
        final String fullname = StringUtil.replaceSubstring(name, "][", ",");
        ack = DSim.get().findOrAddNode(fullname + (data.length == 0 ? "" : ".C") + ".a");
        req = DSim.get().findOrAddNode(fullname + (data.length == 0 ? "" : ".C") + ".q");
        for (int i = 0; i < data.length; ++i) {
            data[i] = DSim.get().findOrAddNode(fullname + ".D[" + i + "]");
        }
        instantiate();
    }
        
    protected abstract void instantiate();
    
    /**
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public void bind() {
        if (!connect_status) {
            connect_status = true;
            instantiateNM();
        }
    }

    @Override
    public String getName() {
        return name;
    }

    public void setName(final String newName) {
        name = newName;
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

        sleep_status = DESTROYED;
    }

    @Override
    public void setOwner(final AbstractDevice owner) {
        this.owner = owner;
    }

    protected void setEnablingNode(final Node node) {
        if (owner != null) owner.setEnablingNode(node);
    }

    protected Node getEnablingNode() {
        return owner == null ? null : owner.getEnablingNode();
    }

    @Override
    public int getCapacity() {
        return 0;
    }   

    @Override
    public int getAvailable() {
        return 0;
    }   

    @Override
    public String getVerboseStatus() {
        return "";
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
} // end of abstract class SlacklessNodeChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
