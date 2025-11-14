// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim;

import java.math.BigInteger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.OptionalInt;
import java.util.Vector;
import java.util.WeakHashMap;

import com.avlsi.cast.impl.SubscriptSpecInterface;
import com.avlsi.cast2.util.DirectiveUtils.BdcValue;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.tools.dsim.InstanceData;
import com.avlsi.tools.tsim.BufferedChannel;
import com.avlsi.tools.tsim.BufferedNodeBDReadChannel;
import com.avlsi.tools.tsim.BufferedNodeBDWriteChannel;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.MergeDevice;
import com.avlsi.tools.tsim.NodeReadChannel;
import com.avlsi.tools.tsim.NodeWriteChannel;
import com.avlsi.tools.tsim.SlacklessNodeBDReadChannel;
import com.avlsi.tools.tsim.SlacklessNodeBDWriteChannel;
import com.avlsi.tools.tsim.SplitDevice;
import com.avlsi.tools.tsim.Statusable;
import com.avlsi.tools.tsim.WideNode;
import com.avlsi.tools.tsim.WideNodeImpl;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Pair;
import com.avlsi.util.math.BigIntegerUtil;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.text.StringUtil;

import java.util.Collections;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

/**
 * Contains information needed to properly hook up channels, nodes and
 * splits/merges between a behavioral-level block and its production
 * rule surroundings.
 **/
public abstract class CoSimInfo {
    protected final ChannelParameterDict chanParams;
    /** Maps Strings to their InternalChannelInfos. **/
    protected final Map internalChannels;
    /** Contains Strings. **/
    protected final Vector inputChannels;
    /** Contains Strings. **/
    protected final Vector outputChannels;
    /** Contains Strings. **/
    protected final Vector inputNodes;
    /** Contains Strings. **/
    protected final Vector outputNodes;
    /** Possibly wide node ports, any direction. */
    protected final List/*<String>*/ wideNodes;
    /** Input DFT channels. **/
    protected final List/*<String>*/ inputDftChannels;
    /** Output DFT channels. **/
    protected final List/*<String>*/ outputDftChannels;

    protected CoSimInfo() {
        chanParams = new ChannelParameterDict();
        internalChannels = new HashMap();
        inputChannels = new Vector();
        outputChannels = new Vector();
        inputNodes = new Vector();
        outputNodes = new Vector();
        wideNodes = new ArrayList();
        inputDftChannels = new ArrayList();
        outputDftChannels = new ArrayList();
    }

    public void addInternalChannel(final String chanName) {
        internalChannels.put(chanName, new InternalChannelInfo());
    }

    public void addInternalChannel(final String chanName,
                                   int[] timingParams,
                                   SubscriptSpecInterface spec) {
        if (timingParams == null) {
            internalChannels.put(chanName, new InternalChannelInfo(spec));
        } else {
            Debug.assertTrue(timingParams.length == 1,
                         "can't handle anything other than slack yet");
            internalChannels.put(chanName,
                                 new InternalChannelInfo(timingParams[1],
                                                         spec));
        }
    }

    public void addInputChannel(final String chanName) {
        inputChannels.add(chanName);
    }

    public void addOutputChannel(final String chanName) {
        outputChannels.add(chanName);
    }

    /** Returns an iterator of Strings. **/
    public ListIterator getInputChannelNames() {
        return inputChannels.listIterator();
    }

    /** Returns an iterator of Strings. **/
    public ListIterator getOutputChannelNames() {
        return outputChannels.listIterator();
    }

    /** Build array of input channels. **/
    public ChannelInput[] getInputChannelArray(final ChannelDictionary dict) {
	final ChannelInput[] cin = new ChannelInput[inputChannels.size()];
	for (int i = 0; i < cin.length; i++) {
	    cin[i] = dict.getInputChan((String) inputChannels.get(i));
        }

        return cin;
    }

    /** Build array of output channels. **/
    public ChannelOutput[] getOutputChannelArray(final ChannelDictionary dict) {
	final ChannelOutput[] cout = new ChannelOutput[outputChannels.size()];
	for (int i = 0; i < cout.length; i++) {
	    cout[i] = dict.getOutputChan((String) outputChannels.get(i));
	}

        return cout;
    }

    /** Build array of wide nodes. */
    public WideNode[] getWideNodeArray(final ChannelDictionary dict) {
        final WideNode[] result = new WideNode[wideNodes.size()];
        for (int i = 0; i < result.length; i++) {
            result[i] = dict.getWideNode((String) wideNodes.get(i));
            assert result[i] != null : "Cannot find WideNode for " +
                                       wideNodes.get(i);
        }
        return result;
    }

    /**
     * Appends <code>nodeName</code> to list of input nodes.
     **/
    public void addInputNode(final /*@ non_null @*/ String nodeName) {
        inputNodes.add(nodeName);
    }

    /**
     * Appends <code>nodeName</code> to list of output nodes.
     **/
    public void addOutputNode(final /*@ non_null @*/ String nodeName) {
        outputNodes.add(nodeName);
    }

    public void addWideNode(final String nodeName) {
        wideNodes.add(nodeName);
    }

    /** Returns an iterator of Strings. **/
    public /*@ non_null @*/ ListIterator getInputNodeNames() {
        return inputNodes.listIterator();
    }

    /** Returns an iterator of Strings. **/
    public /*@ non_null @*/ ListIterator getOutputNodeNames() {
        return outputNodes.listIterator();
    }

    /**
     * Appends <code>dftName</code> to list of input DFT channels.
     **/
    public void addInputDftChannel(final /*@ non_null @*/ String dftName) {
        inputDftChannels.add(dftName);
    }

    /**
     * Appends <code>dftName</code> to list of output DFT channels.
     **/
    public void addOutputDftChannel(final /*@ non_null @*/ String dftName) {
        outputDftChannels.add(dftName);
    }

    /** Returns an iterator of input DFT channels names. **/
    public /*@ non_null @*/ ListIterator/*<String>*/ getInputDftChannelNames() {
        return inputDftChannels.listIterator();
    }

    /** Returns an iterator of output DFT channel names. **/
    public /*@ non_null @*/ ListIterator getOutputDftChannelNames() {
        return outputDftChannels.listIterator();
    }

    /**
     * Adds info on slack/N/M to the internal ChannelParameterDict.
     **/
    public void addChannelInfo(final String name, final String type,
                               final int slack,
                               final BigInteger N, final int M,
                               final boolean isArrayed) {
        addChannelInfo(name, type, slack, N, M, isArrayed,
                       PortDefinition.NONE);
    }

    public void addChannelInfo(final String name, final String type,
                               final int slack,
                               final BigInteger N, final int M,
                               final boolean isArrayed,
                               final int dir) {
        addChannelInfo(name, type, slack, 1, 4, N, M, isArrayed, dir);
    }

    public void addChannelInfo(final String name, final String type,
                               final int slack,
                               final int latency, final int cycle_time,
                               final BigInteger N, final int M,
                               final boolean isArrayed,
                               final int dir) {
        addChannelInfo(name, type,
            new ChannelTimingInfo() {
                public int getSlack() { return slack; }
                public int getInternalSlack() { return 0; }
                public int getLatency() { return latency; }
                public int getCycleTime() { return cycle_time; }
                public int getDataNeutralEnableLatency() { return 0; }
                public int getDataValidEnableLatency() { return 0; }
                public int getEnableDataLatency() { return 0; }
                public int getCycleTimeIn() { return cycle_time; }
                public int getCycleTimeOut() { return cycle_time; }
                public int getLatencyPerSlack() { return 0; }
            }, N, M, isArrayed, dir);
    }

    public void addChannelInfo(final String name, final String type,
                               final ChannelTimingInfo cti,
                               final BigInteger N, final int M,
                               final boolean isArrayed,
                               final int dir) {
        final String realName = isArrayed && M == 1 ?
            StringUtil.replaceSubstring(name + "[0]", "][", ",") : name;
        chanParams.addChannelParameters(realName, type, cti, N, M);
    }

    public void addNodeInfo(final String name, final int M,
                            final boolean isArrayed, final int direction) {
        chanParams.addNodeParameters(name, M, isArrayed, direction);
    }

    /**
     * Clears all information that could have been set from
     * CellImpl.setCoSimInfoFromPorts().  Internal channels are always
     * explicitly specified.
     **/
    public void clearPortChannels() {
        inputChannels.clear();
        outputChannels.clear();
        chanParams.clearChannels();
        inputNodes.clear();
        outputNodes.clear();
        wideNodes.clear();
        inputDftChannels.clear();
        outputDftChannels.clear();
    }

    /**
     * Returns true if the cosimulation information should be
     * retrieved from the port list rather than the Java block.
     **/
    protected abstract boolean usePorts();

    /**
     * Makes appropriate BufferedChannels and adds them to the
     * dictionary.  Internal channels are both inputs and outputs.
     **/
    private void createInternalChannels(final ChannelDictionary dict) {
        for (final Iterator i=internalChannels.entrySet().iterator();
             i.hasNext();
            ) {
            Map.Entry channelDescription = (Map.Entry) i.next();
            final String name = (String) channelDescription.getKey();
            final InternalChannelInfo info =
                (InternalChannelInfo) channelDescription.getValue();
            info.createInternalChannels(name, dict);
        }
    }

    public static class NodeChannelFactory
        implements ChannelFactoryInterface {
        private final float digitalTau;
        private final String prefix;
        private final CellInterface cell;
        private final AliasedSet localNodes;
        private final InstanceData instData;
        private final CoSimInfo cosimInfo;

        public NodeChannelFactory() {
            this(1.0f);
        }

        public NodeChannelFactory(final float digitalTau) {
            this(digitalTau, null, null, null, null, null);
        }

        public NodeChannelFactory(final float digitalTau,
                                  final String prefix,
                                  final CellInterface cell,
                                  final AliasedSet localNodes,
                                  final InstanceData instData,
                                  final CoSimInfo cosimInfo) {
            this.digitalTau = digitalTau;
            this.prefix = prefix;
            this.cell = cell;
            this.localNodes = localNodes;
            this.instData = instData;
            this.cosimInfo = cosimInfo;
        }

        protected int getSlack(final ChannelTimingInfo cti) {
            return cti.getSlack();
        }

        private int validateBDChannel(final String name, final int slack,
                                      final BigInteger radix,
                                      final int width) {
            if (slack < 0)
                throw new IllegalArgumentException(
                        "Negative slack BD channel " + name + " not supported");
            if (width > 1)
                throw new IllegalArgumentException(
                        "Wide BD channel " + name + " not supported");
            final int radix0 = radix.compareTo(BigInteger.ZERO);
            if (radix0 < 0)
                throw new IllegalArgumentException(
                        "Negative radix BD channel " + name + " not supported");
            return radix0 == 0 ? 0 : radix.bitLength() - 1;
        }

        private int getBDSlack(final String name, int slack) {
            return slack;
        }

        private String stripPrefix(String name) {
            if (prefix.length() > 0 && name.startsWith(prefix)) {
                name = name.substring(prefix.length() + 1);
            }
            return StringUtil.replaceSubstring(name, "][", ",");
        }

        private HierName getAckReq(final String fullname, final String aq,
                                   final int width) {
            final String node = fullname + (width == 0 ? "" : ".C") + "." + aq;
            final HierName hNode =
                HierName.makeHierNameUnchecked(stripPrefix(node), '.');
            return (HierName) localNodes.getCanonicalKey(hNode);
        }

        public ChannelInput makeInputChannel(final String name,
                                             final String type,
                                             final BigInteger radix,
                                             final int width,
                                             final ChannelTimingInfo cti) {
            final int slack = getSlack(cti);
            final int latency = cti.getLatency();
            int ffLatency = slack == 0 ? latency : latency / slack;
            int bfLatency = cti.getEnableDataLatency();
            final int fbNeutral = cti.getDataNeutralEnableLatency();
            final int fbValid = cti.getDataValidEnableLatency();
            int fbLatency = (fbNeutral + fbValid) / 2;
            int bbLatency =
                cti.getCycleTime() - ffLatency - bfLatency - fbLatency;
            if (type.startsWith("standard.channel.e1of")) {
                return new NodeReadChannel(slack,
                                           Math.round(ffLatency * digitalTau),
                                           Math.round(bbLatency * digitalTau),
                                           Math.round(fbNeutral * digitalTau),
                                           Math.round(fbValid * digitalTau),
                                           Math.round(bfLatency * digitalTau),
                                           Math.round(cti.getCycleTimeIn() *
                                                      digitalTau),
                                           Math.round(cti.getCycleTimeOut() *
                                                      digitalTau),
                                           name,
                                           BigIntegerUtil.safeIntValue(radix),
                                           width);
            } else if (type.startsWith("standard.channel.bd")) {
                int bdslack = getBDSlack(name, slack);
                final int W = validateBDChannel(name, bdslack, radix, width);
                float fromData = 1;
                float toData = 200;
                int cspTimeOffset = 0;
                if (instData != null) {
                    toData = 0;
                    final HierName ack = getAckReq(name, "a", width);
                    cspTimeOffset = 
                        cosimInfo.getReferenceTime().orElse(cti.getCspTime()) -
                        cti.getCspTime();
                    assert cspTimeOffset >= 0;
                    fromData = instData.getBDLatency(ack, true) * 100;
                    ffLatency = 0;
                    bbLatency = cti.getCycleTime() - ffLatency;
                    fbLatency = 0;
                    bfLatency = 0;
                }
                if (bdslack == 0) {
                    return new SlacklessNodeBDReadChannel(
                            Math.round(toData * digitalTau),
                            Math.round(fromData * digitalTau),
                            cspTimeOffset,
                            name, W);
                } else {
                    return new BufferedNodeBDReadChannel(
                            bdslack,
                            Math.round(toData * digitalTau),
                            Math.round(fromData * digitalTau),
                            Math.round(ffLatency * digitalTau),
                            Math.round(bbLatency * digitalTau),
                            Math.round(fbLatency * digitalTau),
                            Math.round(bfLatency * digitalTau),
                            Math.round(cti.getCycleTimeIn() * digitalTau),
                            Math.round(cti.getCycleTimeOut() * digitalTau),
                            cspTimeOffset,
                            name, W, true);
                }
            } else {
                throw new IllegalArgumentException("Unknown channel type " + type);
            }
        }

        public ChannelOutput makeOutputChannel(final String name,
                                               final String type,
                                               final BigInteger radix,
                                               final int width,
                                               final ChannelTimingInfo cti) {
            final int slack = getSlack(cti);
            final int latency = cti.getLatency();
            int ffLatency = slack == 0 ? latency : latency / slack;
            int bfLatency = cti.getEnableDataLatency();
            final int fbNeutral = cti.getDataNeutralEnableLatency();
            final int fbValid = cti.getDataValidEnableLatency();
            int fbLatency = (fbNeutral + fbValid) / 2;
            int bbLatency =
                cti.getCycleTime() - ffLatency - bfLatency - fbLatency;
            if (type.startsWith("standard.channel.e1of")) {
                return new NodeWriteChannel(slack,
                                            Math.round(ffLatency * digitalTau),
                                            Math.round(bbLatency * digitalTau),
                                            Math.round(fbLatency * digitalTau),
                                            Math.round(bfLatency * digitalTau),
                                            Math.round(cti.getCycleTimeIn() *
                                                       digitalTau),
                                            Math.round(cti.getCycleTimeOut() *
                                                       digitalTau),
                                            name,
                                            BigIntegerUtil.safeIntValue(radix),
                                            width);
            } else if (type.startsWith("standard.channel.bd")) {
                float fromData = 1;
                float toData = 600;
                int bdslack = getBDSlack(name, slack);
                int cspTimeOffset = 0;
                if (instData != null) {
                    toData = 0;
                    final HierName req = getAckReq(name, "q", width);
                    cspTimeOffset =
                        cti.getCspTime() -
                        cosimInfo.getReferenceTime().orElse(cti.getCspTime());
                    assert cspTimeOffset >= 0;
                    fromData = instData.getBDLatency(req, true) * 100;
                    ffLatency = 0;
                    bbLatency = cti.getCycleTime() - ffLatency;
                    fbLatency = 0;
                    bfLatency = 0;
                }
                final int W = validateBDChannel(name, bdslack, radix, width);
                if (bdslack == 0) {
                    return new SlacklessNodeBDWriteChannel(
                            Math.round(toData * digitalTau),
                            Math.round(fromData * digitalTau),
                            cspTimeOffset,
                            name, W);
                } else {
                    return new BufferedNodeBDWriteChannel(
                            bdslack,
                            Math.round(toData * digitalTau),
                            Math.round(fromData * digitalTau),
                            Math.round(ffLatency * digitalTau),
                            Math.round(bbLatency * digitalTau),
                            Math.round(fbLatency * digitalTau),
                            Math.round(bfLatency * digitalTau),
                            Math.round(cti.getCycleTimeIn() * digitalTau),
                            Math.round(cti.getCycleTimeOut() * digitalTau),
                            cspTimeOffset,
                            name, W, true);
                }
            } else {
                throw new IllegalArgumentException("Unknown channel type " + type);
            }
        }
    }

    private static final class WideNodeFactory implements NodeFactoryInterface {
        public WideNode makeWideNode(String name, int width, int direction,
                                     boolean isArrayed, boolean readOnly) {
            return new WideNodeImpl(name, width, direction, isArrayed,
                                    readOnly);
        }
    }

    /**
     * Makes appropriate NodeRead/WriteChannels and fills the
     * dictionary with them.
     *
     * Assumes that this CoSimInfo belongs to cell.
     **/
    public ChannelDictionary createNodeChannels(final HierName cellName,
                                                final CellInterface cell,
                                                final float digitalTau,
                                                final AliasedSet localNodes,
                                                final InstanceData instData) {
        final String sCellName = cellName.getAsString('.');
        return createNodeChannels(cellName, cell,
                new NodeChannelFactory(digitalTau, sCellName, cell, localNodes,
                                       instData, this));
    }

    public ChannelDictionary createNodeChannels(final HierName cellName,
                                                final CellInterface cell,
                                                final ChannelFactoryInterface channelFactory) {
        final String sCellName = cellName.getAsString('.');
        return createChannels(sCellName, cell, channelFactory,
                              new WideNodeFactory());
    }

    /**
     * Makes appropriate channels using <code>factory</code> and fills the
     * dictionary with them.
     *
     * Assumes that this CoSimInfo belongs to cell.
     * 
     * <pre><jml>
     *   public normal_behavior
     *     requires cell != null;
     *     requires chanFactory != null;
     *     ensures \result != null;
     * </jml></pre>
     **/
    public ChannelDictionary createChannels(final String cellName,
                                            final CellInterface cell,
                                            final ChannelFactoryInterface
                                                chanFactory,
                                            final NodeFactoryInterface
                                                nodeFactory) {
        if (usePorts())
            cell.setCoSimInfoFromPorts(this);

        final ChannelDictionary dict = new ChannelDictionary();

        for (final Iterator i=inputChannels.iterator(); i.hasNext(); ) {
            final String chanSuffix = (String) i.next();
            final String chanName =
                cellName != null ? cellName + '.' + chanSuffix
                                 : chanSuffix;
            final ChannelInput chan =
                chanParams.makeInputChannel(chanSuffix, chanName,
                                            chanFactory);
            dict.addInputChan(chanSuffix, chan);
        }
        
        for (final Iterator i=outputChannels.iterator(); i.hasNext(); ) {
            final String chanSuffix = (String) i.next();
            final String chanName = 
                cellName != null ? cellName + '.' + chanSuffix
                                 : chanSuffix;
            final ChannelOutput chan =
                chanParams.makeOutputChannel(chanSuffix, chanName,
                                             chanFactory);
            dict.addOutputChan(chanSuffix, chan);
        }

        createInternalChannels(dict);

        for (final Iterator i = wideNodes.iterator(); i.hasNext(); ) {
            final String nodeSuffix = (String) i.next();
            final String nodeName = 
                cellName != null ? cellName + '.' + nodeSuffix
                                 : nodeSuffix;
            dict.addWideNode(nodeSuffix,
                             chanParams.makeWideNode(nodeSuffix, nodeName,
                                                     false, nodeFactory));
        }

        return dict;
    }

    /**
     * Handles inputs and outputs by making appropriate NodeChannels,
     * BufferedChannels, and Split/MergeDevices.  The NodeChannels are
     * placed in the channel dictionary.  The Devices are start()ed.
     *
     * Assumes that this CoSimInfo belongs to cell.
     *
     * @param basePrefix The instance name of the cell.
     *
     * <pre><jml>
     *   public normal_behavior
     *     requires coSimInfo != null;
     *     requires cell != null;
     *     ensures \result != null;
     * </jml></pre>
     **/
    public static ChannelDictionary createSplitMerges(
            final CoSimInfo coSimInfo,
            final HierName basePrefix, 
            final CellInterface cell,
            final int cosimSlack,
            final boolean verbose) {
        final HierName digPrefix =
            CoSimParameters.addCoSimDigitalSuffix(basePrefix);
        final ChannelDictionary dict = new ChannelDictionary();
        final NodeChannelFactory chanFactory = new NodeChannelFactory();
        final NodeChannelFactory slackFactory = new NodeChannelFactory() {
            public int getSlack(ChannelTimingInfo cti) {
                return 100;
            }
        };

        final NodeFactoryInterface nodeFactory = new WideNodeFactory();

        if (coSimInfo.usePorts())
            cell.setCoSimInfoFromPorts(coSimInfo);

        try {
            // Inputs need to be split.
            final String splitPrefix = basePrefix.getAsString('.')+".Split.";
            for (final Iterator i=coSimInfo.inputChannels.iterator();
                 i.hasNext(); ) {
                /*  Creates SplitDevice:
                 *                          >|_Java_model_|
                 *                         /
                 *                        / blmChan
                 *   ________  baseChan  /
                 *  |_Parent_|-------->{Split}
                 *                       \ 
                 *                        \ digChan
                 *                         \  _______________
                 *                          >| Digital model |
                 */
                String suffix = (String) i.next();
                final ChannelInput baseChan =
                    coSimInfo.chanParams
                             .makeInputChannel(suffix,
                                             HierName.makeHierName(basePrefix, suffix).getAsString('.'),
                                             chanFactory);
                final ChannelOutput digChan =
                    coSimInfo.chanParams
                             .makeOutputChannel(suffix,
                                             HierName.makeHierName(digPrefix, suffix).getAsString('.'),
                                             slackFactory);
                // TODO: Move the slack into the devices
                final BufferedChannel blmChan =
                    new BufferedChannel(cosimSlack,  // slack
                                        1, 1, 1, 1,  // latencies
                                        0,  // debug
                                        HierName.makeHierName(basePrefix,
                                                              suffix)
                                                .getAsString('.'),
                                        digChan.getNumPossibleValues());
                splitMergeChannels.add(digChan);
                splitMergeChannels.add(blmChan);
                 assert digChan.getNumPossibleValues()
                              .equals(baseChan.getNumPossibleValues());
                assert digChan.getNumPossibleValues()
                              .equals(blmChan.getNumPossibleValues());
                new SplitDevice(splitPrefix + suffix, baseChan,
                                new ChannelOutput[]{digChan, blmChan},
                                !verbose).start();
                dict.addInputChan(suffix, blmChan);
            }

            // Outputs need to be merged.
            final String mergePrefix = basePrefix.getAsString('.')+".Merge.";
            for (final Iterator i=coSimInfo.outputChannels.iterator();
                 i.hasNext(); ) {
                /*  Creates MergeDevice:
                 *                          |_Java_model_|
                 *                         /
                 *                        / blmChan
                 *   ________  baseChan  v  
                 *  |_Parent_|<--------{Merge}
                 *                       ^ 
                 *                        \ digChan
                 *                         \ _______________
                 *                          | Digital model |
                 */

                String suffix = (String) i.next();
                final ChannelOutput baseChan =
                    coSimInfo.chanParams
                             .makeOutputChannel(suffix,
                                             HierName.makeHierName(basePrefix, suffix).getAsString('.'),
                                             chanFactory);
                final ChannelInput digChan =
                    coSimInfo.chanParams
                             .makeInputChannel(suffix,
                                             HierName.makeHierName(digPrefix, suffix).getAsString('.'),
                                             slackFactory);
                // TODO: Move the slack into the devices
                final BufferedChannel blmChan =
                    new BufferedChannel(cosimSlack,  // slack
                                        1, 1, 1, 1,  // latencies
                                        0,  // debug
                                        HierName.makeHierName(basePrefix,
                                                              suffix)
                                                .getAsString('.'),
                                        digChan.getNumPossibleValues());
                splitMergeChannels.add(digChan);
                splitMergeChannels.add(blmChan);
                assert digChan.getNumPossibleValues()
                              .equals(baseChan.getNumPossibleValues());
                assert digChan.getNumPossibleValues()
                              .equals(blmChan.getNumPossibleValues());
                new MergeDevice(mergePrefix + suffix,
                                new ChannelInput[]{digChan, blmChan},
                                baseChan, !verbose).start();
                dict.addOutputChan(suffix, blmChan);
            }

            // handle nodes
            for (final Iterator i = coSimInfo.wideNodes.iterator();
                 i.hasNext(); ) {
                final String nodeSuffix = (String) i.next();
                final String nodeName =
                    HierName.makeHierName(basePrefix, nodeSuffix)
                            .getAsString('.');
                dict.addWideNode(nodeSuffix,
                                 coSimInfo.chanParams
                                          .makeWideNode(nodeSuffix, nodeName,
                                                        true, nodeFactory));
            }
        } catch (InvalidHierNameException e) {
            // The strings stored in inputChannels and outputChannels
            // should have originally been HierName suffixes, so this
            // exception should be impossible to get.
            Debug.assertTrue(false, "shouldn't have gotten this: " + e);
        }

        coSimInfo.createInternalChannels(dict);

        return dict;
    }

    public void createPortNodeLinkages
        (final /*@ non_null @*/ String cellName,
         final /*@ non_null @*/ CellInterface cell,
         final /*@ non_null @*/ NodeLinkageInterface nodeLinker) {

        if (usePorts())
            cell.setCoSimInfoFromPorts(this);

        // input nodes
        for (int i = 0; i < inputNodes.size(); ++i) {
            final String nodeName = (String) inputNodes.get(i);
            nodeLinker.makeInputNode(cellName + '.' + nodeName);
        }

        // output nodes
        for (int i = 0; i < outputNodes.size(); ++i) {
            final String nodeName = (String) outputNodes.get(i);
            nodeLinker.makeOutputNode(cellName + '.' + nodeName);
        }
    }

    /**
     * Assumes that <code>this</code> is empty.
     **/
    protected void refineFrom(final CoSimInfo parentInfo) {
        // Only bother to copy anything if the parent's internals were
        // explicitly set.  If it cribbed from its CellImpl, it will
        // do so again (and possibly more accurately).

        // This also means that information is only shared if the
        // refinement child isn't going to modify it.
        if (! parentInfo.usePorts()) {
            this.chanParams.refineFrom(parentInfo.chanParams);
            this.internalChannels.putAll(parentInfo.internalChannels);
            this.inputChannels.addAll(parentInfo.inputChannels);
            this.outputChannels.addAll(parentInfo.outputChannels);
            this.inputNodes.addAll(parentInfo.inputNodes);
            this.outputNodes.addAll(parentInfo.outputNodes);
            this.wideNodes.addAll(parentInfo.wideNodes);
        }
    }

    public String toString() {
        return "CoSimInfo with input channels: " + inputChannels + " and output channels: " + outputChannels;
    }

    private static final Set splitMergeChannels =
        Collections.synchronizedSet(
            Collections.newSetFromMap(new WeakHashMap()));

    public static void printCosimLeftovers() {
        System.gc();            // eliminate zombies from WeakHashSet
        TreeSet ordered = new TreeSet(new Comparator() {
                public int compare(Object o1, Object o2) {
                    Statusable c1 = (Statusable) o1;
                    Statusable c2 = (Statusable) o2;
                    String s1 =
                        new StringBuffer(c1.getName()).reverse().toString();
                    String s2 =
                        new StringBuffer(c2.getName()).reverse().toString();
                    return s1.compareTo(s2);
                }
            });
        ordered.addAll(splitMergeChannels);
        for (Iterator it = ordered.iterator(); it.hasNext(); ) {
            Statusable s = (Statusable) it.next();
            s.printStatus();
        }
    }

    public OptionalInt getReferenceTime() {
        return OptionalInt.empty();
    }
}
