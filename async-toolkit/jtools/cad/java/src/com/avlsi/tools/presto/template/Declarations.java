/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.template;

import com.avlsi.tools.presto.ChannelName;
import com.avlsi.tools.presto.complete.Node;

public interface Declarations {
    /* These correspond to "basic channel types" in standard/channel.cast,
     * although we don't actually use most of them. */
    String CH1of   = "1of";
    String CH_1of  = "_1of";
    String CH_a1of = "_a1of";
    String CHa1of  = "a1of";
    String CH_e1of = "_e1of";
    String CHe1of  = "e1of";
    String CHev1of = "ev1of";

    /**
     * @param c     the channel to declare
     * @param type  should be one of the channel types above (trying to
     *              pretend this is an abstract data type, even though
     *              it's really just a string)
     */
    void declareChannel(ChannelName c, String type);

    /**
     * @param n  the node to declare
     */
    void declareNode(Node n);
}
