/*
 * Copyright 2008 Fulcrum Microsystems. All rights reserved.
 *
 * $Id$
 *
 * Created on Dec 24, 2008
 * Author zloh
 */

package com.fulcrummicro.hw.verification.chip.mgmt.registers;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Container class for each linear bus stop
 * @author zloh
 *
 */
public class RegisterBusStop implements Comparable<RegisterBusStop> {

    /**
     * @param name
     */
    public RegisterBusStop( String name) {
        this.name = name;
    }

    /** bus stop name, identifies a set of bus stops sharing the same
     * MGMT_IN and MGMT_OUT */
    String name;

    /** stride if this is an arrayed bus stop, if arrayed then name is the
     * base constant name and an offset is computed from the stride to
     * get the bus stop id */
    int stride = 0;

    /** register bases within this bus stop */
    SortedSet<RegisterBase> bases = new TreeSet<RegisterBase>();

    /* (non-Javadoc)
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    public int compareTo(RegisterBusStop r) {
        return this.name.compareTo(r.name);
    }

    public String getName() {
        return this.name;
    }

    /**
     *
     * @return sorted set of register bases
     */
    public Collection<RegisterBase> getRegisterBases() {
        return this.bases;
    }

    public int getStride() {
        return this.stride;
    }

}
