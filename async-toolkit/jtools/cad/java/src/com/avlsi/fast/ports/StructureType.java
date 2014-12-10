/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.fast.ports;

import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

/**
 * Class representing structured types in port list (a channel which
 * contains other channels and therefore can't be represented by a
 * ChannelInput or a ChannelOutput).
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public final class StructureType implements PortTypeInterface {
    private List<PortDefinition> subPortsList;
    /**
     * Fully qualified name of this structured type; maybe <code>null</code>.
     **/
    private String tag;

    public StructureType () {
        subPortsList = new ArrayList<PortDefinition>();
        tag = null;
    }

    public StructureType (Iterator<PortDefinition> i) {
        this(i, null);
    }

    public StructureType (Iterator<PortDefinition> i, String tag) {
        subPortsList = new ArrayList<PortDefinition>();
        while (i.hasNext()) {
            PortDefinition p = (PortDefinition) i.next();
            subPortsList.add (p);
        }
        this.tag = tag;
    }

    public void add (PortDefinition p) {
        subPortsList.add (p);
    }

    public Iterator<PortDefinition> iterator () {
        return subPortsList.iterator();
    }

    /**
     * Returns the fully qualified name of this structured type, or
     * <code>null</code> if it doesn't have a name.
     **/
    public String getTag() {
        return tag;
    }

    public String toString() {
        String result = "{";
        for (Iterator<PortDefinition> i = iterator(); i.hasNext(); ) {
            PortDefinition p = i.next();
            result = result + p.toSubPortString() + ';';
        }
        result = result + "}";

        return result;
    }
}
