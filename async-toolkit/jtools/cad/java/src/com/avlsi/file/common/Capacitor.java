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

package com.avlsi.file.common;

import com.avlsi.util.text.NumberFormatter;
import com.avlsi.circuit.CapacitorInterface;
/**
 * Class to represent a capacitor in an aspice or ext file.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Capacitor implements CapacitorInterface{
    /**
     * Name of source node.
     **/
    private final HierName source;

    /**
     * Name of drain node.
     **/
    private final HierName drain;

    /**
     * Capacitance in farads
     **/
    private final double cap;

   /**
     * Class constructor. Ensures that source is lexicographically 
     * less than or equal to drain, in order to canonicalize.
     * @param source name of source node
     * @param drain name of drain node
     * @param cap capacitance in farads
     **/
    public Capacitor(final HierName source,
                     final HierName drain,
                     final double cap)
    {
        // ensure that source <= drain, lexicographically
        if (source.compareTo(drain) <= 0) {
            this.source = source;
            this.drain = drain;
        } else {
            this.source = drain;
            this.drain = source;
        }

        this.cap = cap;
    }

    /** Not used, returns null **/
    public HierName getName() {
        return null;
    }

    /**
     * Get name of source node.
     * @return name of source node
     **/
    public HierName getSource() {
        return source;
    }

    /**
     * Get name of drain node.
     * @return name of drain node
     **/
    public HierName getDrain() {
        return drain;
    }

    /**
     * Get the capacitance
     * @return capacitance in farads.
     **/
    public double getCap() {
        return cap;
    }

    /** Duplicate yes, but for compatibility we'll deal **/
    public double getCapacitance() {
        return cap;
    }

    /**
     * return string suitable for inclusion in an aspice file.
     **/
    public String getAspiceString() {
        return "cap ("
            + getSource().getAspiceString() + ","
            + getDrain().getAspiceString() + ") ("
            + NumberFormatter.format(getCap()) + ");";
    }

    /**
     *
     **/
    public String toString() {
        return getClass().getName() + " ("
            + getSource().toString() + ","
            + getDrain().toString() + ") ("
            + getCap() + ");";
    }
}
