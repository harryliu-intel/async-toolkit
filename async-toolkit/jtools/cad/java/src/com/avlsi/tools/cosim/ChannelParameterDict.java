/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim;

import java.util.Hashtable;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.WideNode;
import com.avlsi.tools.tsim.IllegalSlackException;
import com.avlsi.tools.tsim.NoSuchNodeException;

final class ChannelParameterDict {

    private final class Parameter {
        private final String name;
        private final ChannelTimingInfo cti;
        private final int N;                // wires per enable (e1ofN)
        private final int M;                // # e1ofN bundles per channel

        public Parameter(final String name, final ChannelTimingInfo cti,
                         final int N, final int M) {
            this.name = name;
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
            return factory.makeInputChannel(nodenm, N, M, cti);
        }

        /**
         * @throws IllegalSlackException  If slack is negative.
         * @throws NoSuchNodeException  If a node in the channel could not be
         *   found.
         **/
          public ChannelOutput makeOutputChannel(
                final String nodenm,
                final ChannelFactoryInterface factory) {
            return factory.makeOutputChannel(nodenm, N, M, cti);
        }
    }

    private final Hashtable channelParameters;
    private final Hashtable nodeParameters;

    public ChannelParameterDict() {
        channelParameters = new Hashtable();
        nodeParameters = new Hashtable();
    }

    public void addChannelParameters(final String name,
                                     final ChannelTimingInfo cti,
                                     final int N, final int M) {
        channelParameters.put(name, new Parameter(name, cti, N, M));
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
