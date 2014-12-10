/*
 * Copyright 2000-2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.avlsi.tools.dsim.Event;

/**
 * Adds a private event queue to each thread.  DigitalScheduler fires an event,
 * all threads that wakes up adds new events to their per-thread event queue.
 * When all threads are blocked, the DigitalScheduler inserts all events in the
 * per-thread queues into the central event queue in a deterministic order
 * (provided by sorting on id), then clears the per-thread queues.
 **/
public class AbstractDeviceThread extends Thread implements Comparable<AbstractDeviceThread> {
    private final int id;
    private boolean processed = false;
    private ArrayList<Event> events = new ArrayList<Event>();
    private final List<Event> unmodifiableEvents =
        Collections.unmodifiableList(events);

    public AbstractDeviceThread(final int id, final Runnable target) {
        super(target);
        this.id = id;
    }

    public AbstractDeviceThread(final int id, final Runnable target,
                                final String name) {
        super(target, name);
        this.id = id;
    }

    public void addEvent(final Event e) {
        events.add(e);
    }

    public List<Event> getEvents() {
        return unmodifiableEvents;
    }

    public void clearEvents() {
        events.clear();
    }

    public boolean getProcessed() {
        return processed;
    }

    public void setProcessed(final boolean processed) {
        this.processed = processed;
    }

    public int compareTo(final AbstractDeviceThread o) {
        return id - o.id;
    }
}
