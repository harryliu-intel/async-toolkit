/*
 * Copyright 2002-2009 Fulcrum Microsystems.  All rights reserved.
 * Unauthorized disclosure is prohibited.
 *
 * $Id$
 */

package com.fulcrummicro.hw.verification.util.gen;

import java.util.Map;
import java.util.TreeMap;

public class Granary {

    private static Granary defaultGranary = new Granary();

    private Map<String, Long> seedTable = new TreeMap<String, Long>();

    private boolean isUnused = true;

    private long masterSeed = -1L;
    
    /**
     * Creates a new granary. The granary is initialized using the
     * {@literal FV_GRANARY_MASTER_SEED} environment variable if it has been
     * set, or the current system time otherwise.
     */
    public Granary() {
        this(-1L);
    }

    /**
     * Creates a new granary using the specified master seed.
     * 
     * @param masterSeed
     *            is the master seed to initialize the granary with. If set to
     *            -1, the granary is initialized using the
     *            {@literal FV_GRANARY_MASTER_SEED} environment variable, if it
     *            has been set, or the current system time otherwise.
     */
    public Granary(long masterSeed) {
        if (masterSeed == -1L) {
            String env = System.getenv("FV_GRANARY_MASTER_SEED");
            if (env != null) {
                this.masterSeed = Long.valueOf(env);
            } else {
                this.masterSeed = System.currentTimeMillis();
            }
        } else {
            this.masterSeed = masterSeed;
        }
    }

    /**
     * Retrieves the default {@code Granary} instance.
     * 
     * @return the default {@code Granary} instance.
     */
    public static Granary getDefault() {
        assert defaultGranary != null;
        return defaultGranary;
    }

    /**
     * Sets the default {@code Granary} instance.
     * 
     * @param granary
     *            is the {@code Granary} instance that is to be used as the new
     *            default granary.
     */
    public static Granary setDefault(Granary granary) {
        if (granary == null) {
            throw new NullPointerException("Tried to set null default Granary");
        }
        if (!defaultGranary.isUnused()) {
            throw new IllegalStateException("Tried to change default Granary instance "
                                            + "after it was used");
        }
        defaultGranary = granary;
        return granary;
    }
    
    public boolean isUnused() {
        return isUnused;
    }

    protected long makeNewSeed(String name) {
        return (name+masterSeed).hashCode()+masterSeed;
    }

    protected long getSeed(String name, long fallback, boolean useFallback) {
        isUnused = false;
        if (seedTable.containsKey(name)) {
            return seedTable.get(name).longValue();
        } else {
            Long newSeed = new Long(useFallback ? fallback : makeNewSeed(name));
            seedTable.put(name, newSeed);
            return newSeed.longValue();
        }
    }

    /** Returns the specified seed from the table (or creates a new
     * one, enters it in the table, and returns it if none exists).
     * New seeds are based on the current time, and are almost
     * certainly different between separate invocations of the same
     * program. **/
    public long getSeed(String name) {
        return getSeed(name, 0, false);
    }

    /** Like getSeed(name), but use the specified fallback value if
     * the seed is not in the table. **/
    public long getSeed(String name, long fallback) {
        return getSeed(name, fallback, true);
    }

    public long getMasterSeed() {
        return masterSeed;
    }
}