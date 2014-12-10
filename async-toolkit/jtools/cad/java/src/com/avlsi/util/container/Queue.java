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

package com.avlsi.util.container;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.NoSuchElementException;

/**
 * Queue class.  Supports null elements.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class Queue {
    private final LinkedList ll;

    /**
     * Class constructor.  Creates an empty queue.
     **/
    public Queue() {
        ll = new LinkedList();
    }

    /**
     * Class constructor.  Adds all of the elements in 
     * <code>c</code> to queue in the order returned by the 
     * collection's iterator.
     **/
    public Queue(final Collection c) {
        this();
        pushAll(c);
    }

    /**
     * Add an object to the back of the queue.
     **/
    public void push(final Object o) {
        ll.addLast(o);
    }

    /**
     * Add all of the elements of the collection to the back of the queue,
     * in the order returned by the collection's iterator.
     **/
    public void pushAll(final Collection c) {
        for (final Iterator i = c.iterator(); i.hasNext(); )
            push(i.next());
    }

    /**
     * Remove the object at the front of the queue, and return it.
     **/
    public Object pop() {
        return ll.removeFirst();
    }

    /**
     * Return the object at the front of the queue, without removing it.
     **/
    public Object front() {
        return ll.getFirst();
    }

    /**
     * Return the object at the back of the queue, without removing it.
     **/
    public Object back() {
        return ll.getLast();
    }

    /**
     * Tells whether the queue is empty.
     **/
    public boolean isEmpty() {
        return size() == 0;
    }

    /**
     * Returns the number of elements in the queue.
     **/
    public int size() {
        return ll.size();
    }

    /**
     * Returns true if the other object is a Queue, and all the elements 
     * are equal to those in this queue.
     **/
    public boolean equals(final Object o) {
        return (o instanceof Queue && equals((Queue) o));
    }

    /**
     * Returns true if all the elements are equal to those in this queue.
     **/
    public boolean equals(final Queue q) {
        return ll.equals(q.ll);
    }
}
