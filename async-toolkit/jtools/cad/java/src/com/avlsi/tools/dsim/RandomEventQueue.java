/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:cin:sw=4:expandtab

package com.avlsi.tools.dsim;

import com.avlsi.util.debug.Debug;
import com.avlsi.tools.dsim.Node;

import java.util.ArrayList;
import java.util.NoSuchElementException;
import java.util.List;
import java.util.Random;

/**
 * <p> A queue of events that fire at random. </p>
 * 
 * @author tim
 * @author Aaron Denney
 * @version $Revision$ $Date$
 **/

public class RandomEventQueue implements EventQueueInterface {

    // Attributes

    /** Actual vector */
    private final List<Event> victor;
    /** number of elements */
    private int elementCount = 0;
    /** looked at front of queue */
    private Event top = null;
    /** Random number source. */
    private final Random r;

    // Constructors

    /** Class constructor.  Creates an empty random queue. **/
    public RandomEventQueue() { this(new Random()); }

    /** 
     * Class constructor.  Creates an empty random queue with random number
     * generator <code>r</code>.
     * @param r the random number gererator to use for this queue.
     **/
    public RandomEventQueue(Random r) { this(r, new ArrayList<Event>()); }

    /** 
     * Class constructor.  Creates an empty random queue with random number
     * generator <code>r</code>.
     * @param r the random number gererator to use for this queue.
     * @param l the list used to store events
     **/
    public RandomEventQueue(Random r, List<Event> l) {
        this.r = r;
        this.victor = l;
    }

    // Public Methods

    /** Returns a string list of all the queued Events. Mainly for debugging. **/
    public String pendingList() {
        String ret = "";
        for (int i=0; i<victor.size(); i++) {
            Event cur = victor.get(i);
            if (cur!=null) {
                if (cur instanceof Node) {
                    ret = ret + ((Node)cur).getEventString() + "\n";
                } else {
                    ret = ret + "Generic Random Event at "+cur.getTime();
                }
            }
        }
        return ret;
    }
    /** Add an object to the queue. **/
    public void enqueue(final Event e) {
        victor.add(e);
        e.setIndex(victor.size()-1);
        elementCount++;
    }

    /**
     * Moves a randomly selected Event to the back of the queue via 
     * <code>exchange</code>, such that the vector only has to shrink at the
     * end.
     *  
     * @throws NoSuchElementException
     **/
    private void exchangeAndPopRandom() {
        Debug.assertTrue(top==null);
        if (elementCount <=0) { throw new NoSuchElementException(); }
        int randIndex = r.nextInt(elementCount);
        exchange(randIndex,elementCount-1);
        top = victor.remove(elementCount-1);
        top.setIndex(victor.size());
    }

    /**
     * Remove the object at the front of the queue, and return it.
     * 
     * @throws NoSuchElementException
     **/
    public Event next() {
        if (top == null) { exchangeAndPopRandom(); }
        Event rv = top;
        top.setIndex(-1);
        top = null;
        elementCount--;
        return rv;
    }

    /** 
     * Return the object at the front of the queue, without removing it.
     * 
     * @throws NoSuchElementException
     **/
    public Event front() {
        if (top == null) { exchangeAndPopRandom(); }
        return top;
    }

    /** Remove a specified event from the queue. */
    public void remove(Event e) {
        int index = e.getIndex(), len = victor.size()-1;
        if (top==e) { next(); }
        else if (index>=0 && victor.get(index)==e) {
            exchange(index, len);
            victor.remove(len); 
            e.setIndex(-1); 
            elementCount--;
        } else { throw new NoSuchElementException("Event not in RandomEventQueue");  }
    }

    /** Tells whether the queue is empty. **/
    public boolean isEmpty() { return elementCount == 0; }

    /** Returns the number of elements in the queue. **/
    public int size() { return elementCount; }

    // Private Methods

    /** Swaps two events in the queue and updates their index. **/
    private void exchange(int i, int j) {
        Debug.assertTrue(i < elementCount);
        Debug.assertTrue(j < elementCount);
        Event swapi = victor.get(i);
        Event swapj = victor.get(j);
        victor.set(i, swapj);
        victor.set(j, swapi);
        swapi.setIndex(j);
        swapj.setIndex(i);
    }

} // end of class RandomEventQueue

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */

