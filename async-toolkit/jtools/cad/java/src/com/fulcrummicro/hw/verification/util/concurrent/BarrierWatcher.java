/**
 * INTEL TOP SECRET
 * Copyright 2011 - 2013 Intel Corporation
 * All Rights Reserved.
 */

package com.fulcrummicro.hw.verification.util.concurrent;

/**
 * @author mhesseli
 */
public interface BarrierWatcher<T extends Comparable<T>> {

    /**
     * Informs the barrier watcher that all group members have reached the
     * barrier.
     *
     * @param b
     *            is the barrier that has been reached.
     */
    public void barrierEvent(Barrier<T> b);

    /**
     * Returns the name of the barrier watcher.
     * <p/>
     * <strong>Note</strong>: The name of each barrier watcher should be unique
     * during the lifetime of the application.
     *
     * @return the name of the barrier watcher.
     */
    public String getName();

}