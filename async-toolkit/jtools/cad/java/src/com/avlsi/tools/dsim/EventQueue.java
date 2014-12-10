/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import com.avlsi.util.debug.Debug;
import com.avlsi.tools.dsim.Node;

import java.util.NoSuchElementException;
import java.util.ArrayList;

/**
 * <p> A queue of <code>Event</code> objects.  <em>Smaller</em> ranks are more
 * important. </p>
 *
 * @bug Will not handle duplicates well.
 *
 * @todo Write postconditions for reheap() and remove().
 * @todo Improve postconditions for left(), right(), parent().
 * @todo Add real class invariant.
 * @todo Write public contracts.
 * 
 * @author Aaron Denney
 * @version $Revision$ $Date$
 **/

public class EventQueue implements EventQueueInterface {

    // Attributes

    /*@ private
      @   invariant heap != null;
      @ private
      @   invariant elementCount >= 0;
      @ private
      @   invariant class_invariant();
      @*/

    /** 
     * The actual data-structure that holds the queue's elements as a heap. 
     **/
    private ArrayList heap = new ArrayList();

    /** 
     * The size of the heap, thus the queue. 
     **/
    private int elementCount = 0;

    // Constructors

    /** Class constructor.  Creates an empty priority queue. **/

    public EventQueue() { }

    // Public Methods

    // =================================================================
    // EventQueueInterface implementation.

    // Documented in interface.

    /**
     * @time-complexity O(size()) * O(ArrayList.get())
     **/

    public String pendingList() {
        String ret = "";
        for (int i = 0; i < heap.size(); i++) {
            Event cur = (Event)heap.get(i);
            if (cur != null) {
                if (cur instanceof Node) {
                    ret = ret + ((Node)cur).getEventString() + "\n";
                } else {
                    ret = ret + "Generic Event at "+cur.getTime();
                }
            }
        }
        return ret;
    }

    // Documented in interface.

    /**
     * @time-complexity O(log size()) 
     **/

    public final void enqueue(Event e) {
        heap.add(e);
        e.setIndex(heap.size()-1);
        elementCount++;
        reheap(elementCount-1);
    }

    // Documented in interface.

    /** 
     * @throws NoSuchElementException
     *         If the event queue is empty.
     * @time-complexity O(log size()) 
     **/

    public final Event next() {
        if (elementCount == 0 ) { throw new NoSuchElementException(); }
        return remove(0);
    }

    // Documented in interface.

    /** 
     * @throws NoSuchElementException
     *         If the event queue is empty.
     * @time-complexity O(1)
     **/

    public final Event front() {
        if (elementCount == 0) { throw new NoSuchElementException(); }
        return (Event)heap.get(0);
    }

    // Documented in interface.

    /**
     * @time-complexity O(1) 
     **/

    public final boolean isEmpty() { return elementCount == 0; }

    // Documented in interface.

    /**
     * @time-complexity O(1) 
     **/

    public final int size() { return elementCount; }

    // Documented in interface.

    /**
     * @throws NoSuchElementException
     *         If the event is not in the queue.
     * @time-complexity O(log n)
     * @bug Conditional on exception should be a disjunct, not a conjunct.
     **/

    public final void remove(Event e) {
        int index = e.getIndex(); //heap.indexOf(c);
        if (index < 0 && heap.get(index) != e) { throw new NoSuchElementException (); }
        remove(index);
    }

    /**
     * Rerank event 'e'.  Use after changing its priority.  Use with care!
     *
     * @param e the event ot rerank.
     * @exception NoSuchElementException if the event is not in the queue.
     * @time-complexity O(n)
     * @review Might want to keep and ignore duplicates instead.
     * @bug This method is erroneous if 'e' has a valid index but actually isn't
     * in the heap.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires e != null;
     *   ensures size() == \old(size());
     * also
     * private exceptional_behavior
     *   requires (Event)heap.get(e.getIndex()) != e;
     *   signals (NoSuchElementException nsee);
     * also
     * public exceptional_behavior
     *   requires e.getIndex() < 0;
     *   signals (NoSuchElementException nsee);
     * </jml></pre>
     **/

    public final void rerank(Event e) {
        int index = e.getIndex(); //heap.indexOf(e);
        if (index < 0) { throw new NoSuchElementException (); }
        reheap(index);
    }

    // end of EventQueueInterface implementation.
    // =================================================================

    // Protected Methods

    /**
     * Copies the event 'e' into index 'i' in the queue.
     *
     * @param i the index in which to copy the event.
     * @param e the event to copy.
     *
     * <pre><jml>
     * protected normal_behavior
     *   requires i < size();
     *   requires e != null;
     *   ensures size() == \old(size());
     * also
     * private normal_behavior
     *   ensures (Event)heap.get(i) == e;
     * </jml></pre>
     **/

    protected final void copyInto(int i, Event e) {
        Debug.assertTrue(i < elementCount);
        heap.set(i, e);
        e.setIndex(i);
    }

    /**
     * @return the event at index 'i'
     * @param i the index of the event to get.
     *
     * <pre><jml>
     * protected normal_behavior
     *   requires i < size();
     *   ensures size() == \old(size());
     * </jml></pre>
     **/

    protected final /*@ pure @*/ Event get(int i) { return (Event) heap.get(i); }

    /**
     * Class invariant: implementation heap is total ordered from children to
     * parents.
     **/

    protected final /*@ pure @*/ boolean class_invariant() {
        if (elementCount != heap.size())
            return false;
        for (int i = 1; i < elementCount; i++) {
            if (compare(parent(i),i) > 0)
                return false;
        }
        return true;
    }


    // Private Methods

    /**
     * @return index of left subchild of the node with index 'i'.
     *
// If this method weren't static, we would do:
//   * <pre><jml>
//   * private normal_behavior
//   *   requires i < size();
//   *   ensures \result < size();
//   *   ensures size() == \old(size());
//   * </jml></pre>
     **/

    private static final /*@ pure @*/ int left(int i) { return i*2 + 1; }

    /**
     * @return index of right subchild of the node with index 'i'.
     *
// If this method weren't static, we would do:
//   * <pre><jml>
//   * private normal_behavior
//   *   requires i < size();
//   *   ensures \result < size();
//   *   ensures size() == \old(size());
//   * </jml></pre>
     **/

    private static final /*@ pure @*/ int right(int i) { return i*2 + 2; }

    /**
     * @return index of the parent of node with index 'i'.
     *
// If this method weren't static, we would do:
//   * <pre><jml>
//   * private normal_behavior
//   *   requires i < size();
//   *   ensures \result < size();
//   *   ensures size() == \old(size());
//   * </jml></pre>
     **/

    private static final /*@ pure @*/ int parent(int i) { return (i-1)/2; }

    /**
     * Remove the event at index 'i'.
     *
     * @param i the index of the event to remove from the queue.
     *
     * <pre><jml>
     * private normal_behavior
     *   requires i < size();
     *   ensures size() == \old(size()) - 1;
     * </jml></pre>
     **/

    private final Event remove(int i) {
        int lastIndex = elementCount-1;
        exchange(i, lastIndex);
        Event rv = (Event) heap.remove(lastIndex);
        elementCount--;
        //if node was not the last node in the array, reheap that
        //node to a valid spot
        if (i != lastIndex) reheap(i);
        rv.setIndex(-1);
        return rv;
    }

    /**
     * Reheap the queue's implementation heap.
     *
     * @param i the index of the event to reheap.
     * @design Exact same algorithm as digital_sim.c
     * @requires Pre-condition: everything but element i is in proper place.
     * i.e. (parent(i) > i) xor ((i > left(i)) | (i > right(i))),
     * or some combination.
     * @ensures Preserve invariant of children larger than parent.
     *
     * <pre><jml>
     * private normal_behavior
     *   requires i < size();
     *   ensures size() == \old(size());
     * </jml></pre>
     **/

    private final void reheap(int i) {
        // if smaller than parent, pull up until no longer true.
        while (true) {
            int p=parent(i);
            if ((i>0) && compare(i,p) < 0) {
                exchange(i,p);
                i = p;
            } else { break; }
        }

        // if larger than child, pull down.  Must promote smaller of two
        // children, otherwise invariant violated.  more complicated than
        // pull-up section because must check existence.
        while (true) {
            int l=left(i), r=right(i);
            int s;
            boolean l_smaller, r_smaller;
            l_smaller = (l < elementCount) && (compare(l,i) < 0);
            r_smaller = (r < elementCount) && (compare(r,i) < 0);
            if (!l_smaller && !r_smaller) { break; }
            else if (l_smaller && !r_smaller) { s = l; }
            else if (r_smaller && !l_smaller) { s = r; }
            else if (compare(l, r) < 0) { s = l; }
            else { s = r; }
            exchange(i,s);
            i = s;
        }
    }

    /**
     * Compare two events in the heap indexed by 'i' and 'j'.
     *
     * @return the difference in the times of two two events; [i] - [j].
     * @param i the first index to look up and compare.
     * @param j the second index to look up and compare.
     *
     * <pre><jml>
     * private normal_behavior
     *   requires i < size();
     *   requires j < size();
     *   ensures size() == \old(size());
     * </jml></pre>
     **/

    private final /*@ pure @*/ long compare(int i, int j) {
        return ((Event)heap.get(i)).getTime() - ((Event)heap.get(j)).getTime();
    }

    /**
     * Exchange the events at indices 'i' and 'j'.
     *
     * @param i the index of the first event to exchange.
     * @param j the index of the second event to exchange.
     *
     * <pre><jml>
     * private normal_behavior
     *   requires i < size();
     *   requires j < size();
     *   ensures (Event)heap.get(i) == (Event)heap.get(\old(j));
     *   ensures (Event)heap.get(j) == (Event)heap.get(\old(i));
     *   ensures size() == \old(size());
     * </jml></pre>
     **/

    private final void exchange(int i, int j) {
        Debug.assertTrue(i < elementCount);
        Debug.assertTrue(j < elementCount);
        Event swapi = (Event)heap.get(i);
        Event swapj = (Event)heap.get(j);
        heap.set(i, swapj);
        heap.set(j, swapi);
        swapi.setIndex(j);
        swapj.setIndex(i);
    }

} // end of class EventQueue

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
