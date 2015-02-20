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

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

/**
 * Class representing a simple channel type (a channel which contains
 * only nodes, and can be represented by a ChannelInput or
 * ChannelOutput.  These are e1ofNs and e1ofN[M]s.)
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class ChannelType implements PortTypeInterface {

    /** CAST Type name for the channel, not null. **/
    private final String typeName;

    /**
     * Width of channel, the number of <code>typeName</code>'s comprising
     * channel.  <code> width &gt; 0</code>
     **/
    private final int width;

    /**
     * Tells if this is an arrayed ChannelType.  This is used by SubtypeOutput
     * to differentiate e1of4[1] from e1of4 which both have width of 1, but are
     * incompatible for refinement.
     **/
    private final boolean arrayed;

    /**
     * Describes ports for this channel.
     **/
    private final Collection/*<PortDefinition>*/ subPortsList;

    /**
     * Number of values that can be sent over the narrow channel
     **/
    private final BigInteger numValues;

    /**
     * Class constructor.
     *
     * @param typeName  CAST type name, not null
     **/
    public ChannelType(final Iterator/*<PortDefinition>*/ ports,
                       final String typeName,
                       final BigInteger numValues) {
        this(ports, typeName, 1, numValues, false);
    }

    /**
     * Class constructor.  Use this constructor to represent a type that would
     * require a bracket in CAST, e.g., e1of4[1].
     *
     * @param typeName  CAST type name, not null
     * @param width  number of <code>typeName</code>'s comprising channel,
     *     <code> width &gt; 0</code>.
     **/
    public ChannelType(final Iterator/*<PortDefinition>*/ ports,
                       final String typeName,
                       final int width,
                       final BigInteger numValues) {
        this(ports, typeName, width, numValues, true);
    }

    private ChannelType(final Iterator/*<PortDefinition>*/ ports,
                        final String typeName,
                        final int width, final BigInteger numValues,
                        final boolean arrayed) {
        assert ports != null && typeName != null && width > 0;
        this.typeName = typeName;
        this.width = width;
        this.numValues = numValues;
        this.arrayed = arrayed;
        this.subPortsList = new ArrayList/*<PortDefinition>*/();
        while (ports.hasNext()) add((PortDefinition) ports.next());
    }

    /**
     * Returns the CAST type name.
     *
     * @return channel's CAST type name, not null
     **/
    public String getTypeName() {
        return typeName;
    }

    /**
     * Returns the channel's width, ie number of <code>typeName</code>'s
     * comprising the channel.
     *
     * @return channel's width
     **/
    public int getWidth() {
        return width;
    }

    /**
     * Returns the number of values that can be sent over the narrow channel.
     *
     * @return number of values can be sent
     **/
    public BigInteger getNumValues() {
        return numValues;
    }

    public boolean isArrayed() {
        return arrayed;
    }

    public String toString() {
        return typeName + (width > 1 ? "*" + width : "");
    }

    private void add(PortDefinition p) {
        subPortsList.add(p);
    }

    public Iterator/*<PortDefinition>*/ iterator() {
        return subPortsList.iterator();
    }
}
