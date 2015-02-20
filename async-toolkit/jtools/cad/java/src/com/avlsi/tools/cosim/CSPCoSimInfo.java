/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/tools/cosim/CoSimInfo.java#5 $
 * $DateTime: 2002/02/23 11:33:05 $
 * $Author: chrisb $
 */

package com.avlsi.tools.cosim;

import java.math.BigInteger;

import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;

/**
 * Extends CoSimInfo to contain information specific to the csp block.
 * Doesn't do much yet.
 **/
public class CSPCoSimInfo extends CoSimInfo {
    private CellInterface parent = null;

    public CSPCoSimInfo() {
        super();
    }

    public ChannelDictionary createChannels(final String cellName,
                                            final CellInterface cell,
                                            final ChannelFactoryInterface
                                                chanFactory,
                                            final NodeFactoryInterface
                                                nodeFactory) {
        parent = cell;
        final ChannelDictionary cdict =
            super.createChannels(cellName, cell, chanFactory, nodeFactory);
        parent = null;
        return cdict;
    }

    public void addChannelInfo(final String name, final String type,
                               final int slack,
                               final int latency, final int cycle_time,
                               final BigInteger N, final int M,
                               final boolean isArrayed) {
        if (parent == null) {
            super.addChannelInfo(name, type, slack, latency, cycle_time, N, M,
                                 isArrayed);
        } else {
            final ChannelTimingInfo cti =
                DirectiveUtils.getTiming(parent, name);
            //System.err.println("CSP channel: " + parent.getFullyQualifiedType() + " " + name + " " + cti.getSlack() + " " + cti.getLatency() + " " + cti.getCycleTime());
            super.addChannelInfo(name, type, cti, N, M, isArrayed);
        }
    }

    protected boolean usePorts() { return true; }

    public String toString() {
        return "CSPCoSimInfo with input channels: " + inputChannels + " and output channels: " + outputChannels;
    }
}
