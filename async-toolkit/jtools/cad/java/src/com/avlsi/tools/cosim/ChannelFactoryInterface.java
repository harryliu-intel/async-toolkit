/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.cosim;

import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;

/**
 * Factory to create input and output channels.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public interface ChannelFactoryInterface {

    /**
     * Creates an input channel.
     * 
     * <pre><jml>
     *   public normal_behavior
     *     requires radix >= 2;
     *     requires width >= 1;
     *     requires slack >= 0;
     *     requires latency >= 0;
     *     requires cycle_time >= 0;
     * </jml></pre>
     **/
    ChannelInput makeInputChannel(String name,
                                  int radix,
                                  int width,
                                  ChannelTimingInfo cti);

    /**
     * Creates an output channel.
     * 
     * <pre><jml>
     *   public normal_behavior
     *     requires radix >= 2;
     *     requires width >= 1;
     *     requires slack >= 0;
     *     requires latency >= 0;
     *     requires cycle_time >= 0;
     * </jml></pre>
     **/
    ChannelOutput makeOutputChannel(String name,
                                    int radix,
                                    int width,
                                    ChannelTimingInfo cti);
}
