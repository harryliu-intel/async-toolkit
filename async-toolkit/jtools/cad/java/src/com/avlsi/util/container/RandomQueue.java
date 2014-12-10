/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:cin:sw=4:expandtab
/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.container;

import  com.avlsi.util.debug.Debug;

import java.util.NoSuchElementException;
import java.util.Vector;
import java.util.Random;

/**
 * RandomQueue class. 
 * returns a random element from the queue
 * 
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/
public class RandomQueue {
    /** Actual vector */
    private Vector victor = new Vector();
    /** number of elements */
    private int elementCount = 0;
    /** looked at front of queue */
    private Object top = null;
    /** Random number source. */
    private final Random r;

    /**
     * Class constructor.  Creates an empty random queue.
     **/
    public RandomQueue() {
        r = new Random();
    }

    /**
     * Class constructor.  Creates an empty random queue whose random
     * number generator is seeded with <code>seed</code>.
     **/
    public RandomQueue(long seed) {
        r = new Random(seed);
    }

    /**
     * Class constructor.  Creates an empty random queue whose random
     * number generator is seeded with <code>seed</code>.
     **/
    public RandomQueue(Random r) {
        this.r = r;
    }

    /**
     * Add an object to the queue.
     **/
    public void enqueue(final Comparable c) {
	victor.add(c);
	elementCount++;
    }

    private void exchangeAndPopRandom() {
        Debug.assertTrue(top==null);
        int randIndex = r.nextInt(elementCount);
        exchange(randIndex,elementCount-1);
        top = victor.elementAt(elementCount-1);
        victor.removeElementAt(elementCount-1);
    }
    /**
     * Remove the object at the front of the queue, and return it.
     **/
    public Object next() {
        Object rv;
        if (elementCount <=0) throw new NoSuchElementException();
        if (top == null) {
            exchangeAndPopRandom();
        }
        rv = top;
        top = null;
        elementCount--;
        return rv;
    }

    /**
     * Return the object at the front of the queue, without removing it.
     **/
    public Object front() {
	if (elementCount <=0) throw new NoSuchElementException();
        if (top == null) {
            exchangeAndPopRandom();
        }
        return top;
    }

    /**
     * Tells whether the queue is empty.
     **/
    public boolean isEmpty() {
        return elementCount == 0;
    }

    /**
     * Returns the number of elements in the queue.
     **/
    public int size() {
        return elementCount;
    }

    private void exchange(int i, int j) {
        Debug.assertTrue(i < elementCount);
        Debug.assertTrue(j < elementCount);
	Object swap = victor.elementAt(i);
	victor.setElementAt(victor.elementAt(j), i);
	victor.setElementAt(swap, j);
    }
}
