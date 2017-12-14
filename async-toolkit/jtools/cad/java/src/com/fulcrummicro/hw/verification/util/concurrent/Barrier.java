/**
 * INTEL TOP SECRET
 * Copyright 2011 - 2014 Intel Corporation
 * All Rights Reserved.
 */

package com.fulcrummicro.hw.verification.util.concurrent;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;


/**
 * A barrier which blocks the caller until all group members have called it.
 * <p>
 * An example of how the {@code Barrier} class can be used is:
 *
 * <pre>
 *  private static final barrier = new Barrier();
 *  public barrierEvent(Barrier b) {
 *      this.actionCompleted(null);
 *  }
 *  public execute() {
 *      barrier.add(this);
 *      ...
 *      if (!barrier.post(this))
 *          this.waitForAction();
 *      ...
 *  }
 * </pre>
 *
 * @author mhesseli
 */
public class Barrier<T extends Comparable<T>> {

    /**************************************************************************
     * Types
     **************************************************************************/

    /**
     * A wrapper for the associated object to ensure that the value put into the
     * {@link Barrier#watchers} concurrent map is non-null and the object order
     * of the set returned by {@link Barrier#getAssociatedObject()} is identical
     * from one function call to another function call.
     */
    private static class Value<T extends Comparable<T>> implements Comparable<Value<T>> {

        public final BarrierWatcher<T> owner;

        public T value;

        public Value(BarrierWatcher<T> owner, T value) {
            this.owner = owner;
            this.value = value;
        }

        public int compareTo(Value<T> o) {
            int cmp;

            cmp = this.owner.getName().compareTo(o.owner.getName());
            if (cmp != 0) {
                return cmp;
            } else if (this.value == null) {
                return o.value == null ? 0 : -1;
            } else {
                return o.value == null ? 1 : this.value.compareTo(o.value);
            }
        }

    }

    private static class WatcherComparator<T extends Comparable<T>> implements Comparator<BarrierWatcher<T>> {

        public int compare(BarrierWatcher<T> o1, BarrierWatcher<T> o2) {
            return o1.getName().compareTo(o2.getName());
        }

    }

    /**************************************************************************
     * Private Variables
     **************************************************************************/

    private final java.util.concurrent.Semaphore lock;

    private final SortedMap<BarrierWatcher<T>, Value<T>> watchers;

    private int count;

    /**
     * Boolean indicating whether the user is allowed to call the
     * {@link #getAssociatedObject()} method. Set to <tt>false</tt> until all
     * group members have reached the barrier and the set of associated objects
     * has become consistent.
     */
    private boolean completed = false;

    /**************************************************************************
     * Public Methods
     **************************************************************************/

    /**
     * Initializes a newly created {@code Barrier} object.
     */
    public Barrier() {
        this.count = 0;
        this.lock = new java.util.concurrent.Semaphore(1);
        this.watchers = new TreeMap<BarrierWatcher<T>, Value<T>>(new WatcherComparator<T>());
    }

    /**
     * Adds a new group member to this barrier.
     *
     * @param watcher
     *            is the group member to add.
     * @throws IllegalArgumentException
     *             if the set of watchers already contains a watcher with a name
     *             identical to the name of the specified watcher.
     */
    public void add(BarrierWatcher<T> watcher) {
        String msg;

        this.acquire();
        try {
            if (this.watchers.containsKey(watcher)) {
                msg = String.format("\"%s\" not unique", watcher.getName());
                throw new IllegalArgumentException(msg);
            }
            this.watchers.put(watcher, new Value<T>(watcher, null));
        } catch (RuntimeException e) {
            throw e;
        } finally {
            this.release();
        }
    }

    /**
     * Returns the set of objects associated with the current barrier. The
     * object order in the returned set is identical from one function call to
     * another function call, but is different from one execution of an
     * application to another execution of an application.
     *
     * @return the set of object associated with the current barrier.
     * @throws IllegalStateException
     *             if one or more group members have not yet reached the
     *             barrier.
     */
    public List<T> getAssociatedObject() {
        List<T> l;
        List<Value<T>> values;

        this.acquire();
        try {
            if (!this.completed) {
                throw new IllegalStateException();
            }
            // Ensure that the object order in the returned set is identical
            // from one function call to another function call.
            values = new ArrayList<Value<T>>(this.watchers.values());
            Collections.sort(values);
            l = new ArrayList<T>();
            for (Value<T> o : values) {
                l.add(o.value);
            }
            return l;
        } catch (RuntimeException e) {
            throw e;
        } finally {
            this.release();
        }
    }

    /**
     * Increments the value of the barrier. If the caller was the last group
     * member to reach the barrier, notifies all group members except the caller
     * and resets the value of the barrier to 0.
     *
     * @param caller
     *            is the group member incrementing the value of the barrier.
     * @return <tt>true</tt> if all watchers have reached the barrier,
     *         <tt>false</tt> otherwise.
     */
    public boolean post(BarrierWatcher<T> caller) {
        return this.post(caller, null);
    }

    /**
     * Increments the value of the barrier. If the caller was the last group
     * member to reach the barrier, notifies all group members except the caller
     * and resets the value of the barrier to 0.
     *
     * @param caller
     *            is the group member incrementing the value of the barrier.
     * @param associatedObject
     *            is an arbitrary object that is broadcast to all group members
     *            once all members have reached the barrier.
     * @return <tt>true</tt> if all group members have reached the barrier,
     *         <tt>false</tt> otherwise.
     */
    public boolean post(BarrierWatcher<T> caller, T associatedObject) {
        String msg;

        this.acquire();
        try {
            if (!this.watchers.containsKey(caller)) {
                msg = String.format("\"%s\" not a group member", caller.getName());
                throw new IllegalArgumentException(msg);
            }
            this.watchers.get(caller).value = associatedObject;
            this.completed = ++this.count == this.watchers.size();
            if (this.completed) {
                for (BarrierWatcher<T> watcher : this.watchers.keySet()) {
                    if (!watcher.equals(caller)) {
                        watcher.barrierEvent(this);
                    }
                }
                this.count = 0;
            }
            return this.completed;
        } catch (RuntimeException e) {
            throw e;
        } finally {
            this.release();
        }
    }

    /**
     * Remove a group member from this barrier.
     *
     * @param watcher
     *            is the group member to remove.
     */
    public void remove(BarrierWatcher<T> watcher) {
        this.acquire();
        try {
            this.watchers.remove(watcher);
        } catch (RuntimeException e) {
            throw e;
        } finally {
            this.release();
        }
    }

    /**************************************************************************
     * Protected Methods
     **************************************************************************/

    protected void acquire() {
        try {
            this.lock.acquire();
        } catch (InterruptedException e) { }
    }

    protected void release() {
        this.lock.release();
    }

}
