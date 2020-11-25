/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/tools/cosim/CoSimInfo.java#5 $
 * $DateTime: 2002/02/23 11:33:05 $
 * $Author: chrisb $
 */

package com.avlsi.tools.cosim;

import java.math.BigInteger;
import java.util.OptionalInt;

import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.ports.PortDefinition;

/**
 * Extends CoSimInfo to contain information specific to the csp block.
 * Doesn't do much yet.
 **/
public class CSPCoSimInfo extends CoSimInfo {
    private CellInterface parent = null;
    private int maxInputOffset = Integer.MIN_VALUE;
    private int minOutputOffset = Integer.MAX_VALUE;

    public CSPCoSimInfo() {
        super();
    }

    @Override
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

    @Override
    public void addChannelInfo(final String name, final String type,
                               final int slack,
                               final int latency, final int cycle_time,
                               final BigInteger N, final int M,
                               final boolean isArrayed,
                               final int dir) {
        if (parent == null) {
            super.addChannelInfo(name, type, slack, latency, cycle_time, N, M,
                                 isArrayed, dir);
        } else {
            final ChannelTimingInfo cti =
                DirectiveUtils.getTiming(parent, BlockInterface.CSP, name, 1, null, dir);
            if (dir == PortDefinition.IN) {
                maxInputOffset = Math.max(maxInputOffset, cti.getCspTime());
            } else if (dir == PortDefinition.OUT) {
                minOutputOffset = Math.min(minOutputOffset, cti.getCspTime());
            }
            //System.err.println("CSP channel: " + parent.getFullyQualifiedType() + " " + name + " " + cti.getSlack() + " " + cti.getLatency() + " " + cti.getCycleTime() + " " + cti.getCspTime() + " " + minCspTime);
            super.addChannelInfo(name, type, cti, N, M, isArrayed, dir);
        }
    }

    @Override
    public OptionalInt getReferenceTime() {
        int t = maxInputOffset == Integer.MIN_VALUE
            ? (minOutputOffset == Integer.MAX_VALUE ? 0 : minOutputOffset)
            : maxInputOffset;
        return OptionalInt.of(t);
    }

    protected boolean usePorts() { return true; }

    public String toString() {
        return "CSPCoSimInfo with input channels: " + inputChannels + " and output channels: " + outputChannels;
    }
}
