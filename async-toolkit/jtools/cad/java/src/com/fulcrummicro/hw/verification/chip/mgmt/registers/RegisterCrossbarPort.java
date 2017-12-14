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
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import com.fulcrummicro.util.misc.Utility;

/**
 * Container class for crossbar ports
 * @author zloh
 *
 */
public class RegisterCrossbarPort implements Comparable<RegisterCrossbarPort>{

    /**
     * @param name
     */
    public RegisterCrossbarPort( String name) {
        this.name = name;
    }

    /** crossbar port address from crossbarPort tag in XML*/
    String name;

    /** non-zero stride if arrayed crossbar ports, offset will be added to name */
    int stride;

    /** bases within this crossbar port */
    private SortedSet<RegisterBase> bases = new TreeSet<RegisterBase>();

    /** map from bus stop name to bus stop */
    Map<String,RegisterBusStop> busStopMap = new HashMap<String, RegisterBusStop>();

    public boolean hasBusStops() {
        Set<String> keys = busStopMap.keySet();
        if (keys.contains("BSN_NULL")) {
            return keys.size() > 1;
        } else return keys.size() > 0;
    }

    public Collection<RegisterBusStop> getBusStops() {
        return this.busStopMap.values();
    }

    /** @return name used for CSP attribute cell */
    public String getBusStopCspAttributeName() {
        return getBusStopCspRawName() + "Functions";
    }

    public String getBusStopCspRawName() {
        String n = Utility.underscoreToCamelCase(name, true);
        n = n.replaceFirst("Cbp", "");//remove Cbp prefix
        return n;
    }

    public String getName() {
        return this.name;
    }

    public void addRegisterBase(RegisterBase base) {
        this.bases.add(base);
    }

    /**
     *
     * @return sorted set of register bases
     */
    public SortedSet<RegisterBase> getRegisterBases() {
        return this.bases;
    }

    /* (non-Javadoc)
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    public int compareTo(RegisterCrossbarPort o) {
        return this.bases.first().compareTo(o.bases.first());
//        return this.name.compareTo(o.name);
    }

    /**
     * Returns the total number of registers (counting all arrayed
     * registers as '1' regardless of their size and dimensionality).
     **/
    public int getNumRegisters() {
        int num = 0;
        for (RegisterBase b : bases) {
            num += b.getRegisters().size();
        }
        return num;
    }

    /**
     * Over all registers in all bases, returns the maximum number of
     * atomic words per register entry.
     **/
    public int getMaxEntriesAtomic() {
        int maxIdxA = 1;
        for (RegisterBase b : bases) {
            for (Register r : b.getRegisters()) {
                maxIdxA = Math.max(maxIdxA, r.getAtomicWidth());
            }
        }
        return maxIdxA;
    }

    /**
     * Over all registers in all bases, returns the maximum INDEX_(idx)
     * entries.
     **/
    public int getMaxEntries(int idx) {
        int maxIdx = 1;
        for (RegisterBase b : bases) {
            for (Register r : b.getRegisters()) {
                maxIdx = Math.max(maxIdx, idx==0 ? r.getNumEntries() :
                                          idx==1 ? r.getNumEntries1() :
                                          idx==2 ? r.getNumEntries2() : 0);
            }
        }
        return maxIdx;
    }

    public int getStride() {
        return this.stride;
    }

}
