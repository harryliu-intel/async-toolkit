/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim;

import java.math.BigInteger;
import java.util.Hashtable;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.WideNode;
import com.avlsi.tools.tsim.IllegalSlackException;
import com.avlsi.tools.tsim.NoSuchNodeException;

final class ChannelParameterDict {

    private final class Parameter {
        private final String name;
        private final String type;
        private final ChannelTimingInfo cti;
        private final BigInteger N;         // # of values over a narrow channel
        private final int M;                // # of narrow channels

        public Parameter(final String name, final String type,
                         final ChannelTimingInfo cti,
                         final BigInteger N, final int M) {
            this.name = name;
            this.type = type;
            this.cti = cti;
            this.N = N;
            this.M = M;
        }

        /**
         * @throws IllegalSlackException  If slack is negative.
         * @throws NoSuchNodeException  If a node in the channel could not be
         *   found.
         **/
        public ChannelInput makeInputChannel(
                final String nodenm,
                final ChannelFactoryInterface factory) {
            return factory.makeInputChannel(nodenm, type, N, M, cti);
        }

        /**
         * @throws IllegalSlackException  If slack is negative.
         * @throws NoSuchNodeException  If a node in the channel could not be
         *   found.
         **/
          public ChannelOutput makeOutputChannel(
                final String nodenm,
                final ChannelFactoryInterface factory) {
            return factory.makeOutputChannel(nodenm, type, N, M, cti);
        }
    }

    private final Hashtable channelParameters;
    private final Hashtable nodeParameters;

    public ChannelParameterDict() {
        channelParameters = new Hashtable();
        nodeParameters = new Hashtable();
    }

    public void addChannelParameters(final String name, final String type,
                                     final ChannelTimingInfo cti,
                                     final BigInteger N, final int M) {
        channelParameters.put(name, new Parameter(name, type, cti, N, M));
    }

    public void addChannelParameters(final ChannelParameterDict params,
                           final String name) {
        final Parameter par = 
            (Parameter) params.channelParameters.get(name);
        channelParameters.put(name, par);
    }

    public void addNodeParameters(final String name, final int M,
                                  final boolean isArrayed,
                                  final int direction) {
        nodeParameters.put(name, new int[] { M, direction, isArrayed ? 1 : 0 });
    }

    public void clearChannels() {
        channelParameters.clear();
    }

    public ChannelInput makeInputChannel
        (final String chname, final String nodenm,
         final ChannelFactoryInterface factory) {
        final Parameter par = 
            (Parameter)channelParameters.get(chname);
        return par.makeInputChannel(nodenm, factory);
    }

    public ChannelOutput makeOutputChannel
        (final String chname, final String nodenm,
         final ChannelFactoryInterface factory) {
        final Parameter par = 
            (Parameter)channelParameters.get(chname);
        return par.makeOutputChannel(nodenm, factory);
    }

    public WideNode makeWideNode(final String portName, final String fullName,
                                 final boolean cosim,
                                 final NodeFactoryInterface factory) {
        final int[] params = (int[]) nodeParameters.get(portName);
        return factory.makeWideNode(fullName, params[0], params[1],
                                    params[2] == 1, cosim);
    }

    /** Should only be called from CoSimInfo.refineFrom(). **/
    public void refineFrom(final ChannelParameterDict parentParams) {
        this.channelParameters.putAll(parentParams.channelParameters);
        this.nodeParameters.putAll(parentParams.nodeParameters);
    }
}
