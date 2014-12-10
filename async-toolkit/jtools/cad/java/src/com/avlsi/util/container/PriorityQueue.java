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

import com.avlsi.util.debug.Debug;

import java.util.NoSuchElementException;
import java.util.Vector;

/**
 * PriorityQueue class. 
 * Will not handle duplicates well
 * <b>Smaller</b> ranks are more important.
 * Only stores <code>Comparable</code>
 * 
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/
public class PriorityQueue {
    /** Actual heap */
    private Vector heap = new Vector();
    /** size of heap */
    private int elementCount = 0;

    /**
     * Class constructor.  Creates an empty priority queue.
     **/
    public PriorityQueue() {
    }

    /**
     * Add an object to the queue.  O(log N)
     **/
    public void enqueue(final Comparable c) {
	heap.add(c);
	elementCount++;
        reheap(elementCount-1);
    }

    /**
     * Remove the object at the front of the queue, and return it. O(log N)
     **/
    public Comparable next() {
        Comparable rv;
	if (elementCount <=0) throw new NoSuchElementException();
        rv = remove(0);
	return rv;
    }

    /**
     * Return the object at the front of the queue, without removing it.
     * O(1)
     **/
    public Comparable front() {
	if (elementCount <=0) throw new NoSuchElementException();
	return (Comparable) heap.elementAt(0);
    }

    /**
     * Tells whether the queue is empty.
     * O(1)
     **/
    public boolean isEmpty() {
        return elementCount == 0;
    }

    /**
     * Returns the number of elements in the queue.
     * O(1)
     **/
    public int size() {
        return elementCount;
    }

    // return index of left subchild of node with index i
    private static final int left(int i) {
        return i*2 + 1;
    }

    // return index of right subchild of node with index i
    private static final int right(int i) {
        return i*2 + 2;
    }

    // return index of parent of node with index i
    private static final int parent(int i) {
        return (i-1)/2;
    }

    /**
     * Rerank Comparable <code>c</code>.
     * Use after changing its priority.
     * O(n) -- Use with care!
     * Might want to keep and ignore duplicates instead.
     **/
    public void rerank(Comparable c) {
        int index = heap.indexOf(c);
        if (index < 0) {
            throw new NoSuchElementException ();
        }
        reheap(index);
    }

    /**
     * Remove Comparable <code>c</code>.
     * If an event is later invalidated, you can use
     * this to take it out of the queue.  O(n), so it
     * might be better to have some sort of "superseded/invalid" flag.
     **/
    public void remove(Comparable c) {
        int index = heap.indexOf(c);
        if (index < 0) {
            throw new NoSuchElementException ();
        }
        remove(index);
    }


    private Comparable remove(int index) {
        Comparable rv;
	int lastIndex = elementCount-1;
        exchange(index, lastIndex);
        rv = (Comparable) heap.elementAt(lastIndex);
        heap.removeElementAt(lastIndex);
        elementCount--;
        reheap(index);
        return rv;
    }

    // Preserve invariant of children larger than parent.
    // Exact same algorithm as digital_sim.c
    // Pre-condition: everything but element i is in proper place.
    // i.e. (parent(i) > i) xor ((i > left(i)) | (i > right(i))),
    // or some combination.
    private void reheap(int i) {
        // if smaller than parent, pull up until no longer true.
        while (true) {
            int p=parent(i);
            if ((i>0) && compare(i,p) < 0) {
                exchange(i,p);
                i = p;
            } else {
                break;
            }
        }

        // if larger than child, pull down.  Must promote smaller
        // of two children, otherwise invariant violated.
        // more complicated than pull-up section because must check existence.
        while (true) {
            int l=left(i), r=right(i);
            int s;
            boolean l_smaller, r_smaller;
            l_smaller = (l < elementCount) && (compare(l,i) < 0);
            r_smaller = (r < elementCount) && (compare(r,i) < 0);
            if (!l_smaller && !r_smaller) { break; }
            else if (l_smaller && !r_smaller) {
                s = l;
            } else if (r_smaller && !l_smaller) {
                s = r;
            } else if (compare(l, r) < 0) {
                s = l;
            } else {
                s = r;
            }

            exchange(i,s);
            i = s;
        }
    }

    private int compare (int i, int j) {
        return ((Comparable) heap.elementAt(i)).compareTo(heap.elementAt(j));
    }

    private void exchange(int i, int j) {
        Debug.assertTrue(i < elementCount);
        Debug.assertTrue(j < elementCount);
	Comparable swap = (Comparable) heap.elementAt(i);
	heap.setElementAt(heap.elementAt(j), i);
	heap.setElementAt(swap, j);
    }

    protected void checkHeapInvariant() {
        Debug.assertTrue(elementCount == heap.size());
        for (int i = 1; i < elementCount; i++) {
                Debug.assertTrue(compare(parent(i),i) < 0, "Heap constrant violated:" + heap.elementAt(i) + " < " + heap.elementAt(parent(i)));
        }
    }
}
