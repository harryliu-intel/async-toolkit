/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2002 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id: $
 */

package com.avlsi.fast;


import java.util.Iterator;

import com.avlsi.file.common.HierName;
import com.avlsi.util.container.Alias;

/**
 * Class to represent one of the nets inside a channel.  Does not
 * represent complete connectivity of said net, only its connectivity
 * inside that channel.  In the cast of the cast fragment "e1of2 L;
 * node foo = L.e; L.0 = L.e;", the net for L.0 would only have the
 * two names "0" and "d[0]", not "e" or "foo".
 *
 * Immutable.
 **/

public class ChannelNet {
    /**
     * List of alternate names (as HierNames without parents), in a
     * useful form for parent classes.
     **/
    private final Alias names;

    /** Only constructor, used by ChannelTemplate.execute(). **/
    public ChannelNet(final Alias names) {
        this.names = names;
    }

    /**
     * Returns the canonical name of this net, based only on the
     * information known by this channel (see example in class-level
     * comment)
     **/
    public HierName getCanonicalName() {
        return (HierName) names.getRootAlias().getKey();
    }

    /** Returns an iterator of all the HierNames stored in this ChannelNet **/
    public Iterator getNames() {
        return names.getAliasedKeys();
    }
}
