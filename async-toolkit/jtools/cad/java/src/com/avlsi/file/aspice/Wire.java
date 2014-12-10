/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.file.aspice;

import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

/**
 * Class to represent a wire in an .aspice files.
 * @design jmr XXX this class is stupid, remove it pronto.
 *   Namespace should be tracked using AliasedStringMap.  Duh!
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Wire {
    /**
     * Name of nodes being connected.
     **/
    private final Set nodeNames = new TreeSet();

    /**
     * Class constructor.  Makes a wire connecting an empty set of nodes.
     **/
    public Wire()
    {
    }

    /**
     * Adds the wire to the set of equivalent wires.
     **/
    public void addNode(final String nodeName) {
        nodeNames.add(nodeName);
    }

    /**
     * returns an Iterator of the connected node names.
     **/
    public Iterator getNodeNames() {
        return nodeNames.iterator();
    }

    /**
     * Returns the number of connected nodes.
     **/
    public int getNumNodes() {
        return nodeNames.size();
    }

    /**
     * return string suitable for inclusion in an aspice file.
     **/
    public String getAspiceString() {
        final StringBuffer sb = new StringBuffer();

        sb.append("wire ("); 

        int n = 0;
        for (final Iterator i = nodeNames.iterator(); i.hasNext(); ++n) {
            final String nodeName = (String) i.next();

            if (n > 0)
                sb.append(',');

            sb.append(nodeName);
        }

        sb.append(");");

        return sb.toString();
    }

    public String toString() {
        return getAspiceString();
    }

}
