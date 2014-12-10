/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.dsim;

import java.util.NoSuchElementException;

/**
 * A queue of events.
 *
 * @author Dan Daly
 * @version $Date$
 **/

public interface EventQueueInterface {

    /** 
     * @return a list of pending events as a string.
     **/

    /*@ pure @*/ String pendingList();

    /** 
     * Add an event 'e' to the queue. 
     *
     * @param e the event to add.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires e != null;
     *   ensures size() == \old(size()) + 1;
     * </jml></pre>
     **/

    void enqueue(Event e);

    /** 
     * @return remove the object at the front of the queue, and return it.
     * @exception NoSuchElementException if the queue is empty.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires size() > 0;
     *   ensures size() == \old(size()) - 1;
     * also
     * public exceptional_behavior
     *   requires size() == 0;
     *   signals (NoSuchElementException nsee);
     * </jml></pre>
     **/

    Event next();

    /** 
     * @return the object at the front of the queue, without removing it. 
     * @exception NoSuchElementException if the queue is empty.
     *
     * <pre><jml>
     * public exceptional_behavior
     *   requires size() == 0;
     *   signals (NoSuchElementException nsee);
     * </jml></pre>
     **/

    /*@ pure @*/ Event front();

    /**
     * @return a flag indicating whether the queue is empty or not.
     **/

    /*@ pure @*/ boolean isEmpty();
    
    /**
     * @return the size of the queue.
     * @time-complexity O(1)
     *
     * <pre><jml>
     * public normal_behavior
     *   ensures \result >= 0;
     * </jml></pre>
     **/

    /*@ pure @*/ int size();

    /**
     * Remove event 'e' from the queue.
     *
     * @param e the event to remove.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires e != null;
     *   ensures size() == \old(size()) - 1;
// There is no variable heap... This is an interface.
//   * also
//   * public exceptional_behavior
//   *   requires (Event)heap.get(e.getIndex()) != e;
//   *   signals (NoSuchElementException nsee);
     * also
     * public exceptional_behavior
     *   requires e.getIndex() < 0;
     *   signals (NoSuchElementException nsee);
     * </jml></pre>
     **/

    void remove(Event e);

} // end of interface EventQueueInterface

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */


