/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

/**
 * The interface that the DigitalScheduler interacts with 
 *
 * @author tim
 * @author Dan Daly
 * @version $Revision$ $Date$
 **/

public interface Event {

    /** 
     * Called by scheduler when it is time for this event to act. 
     **/
    void fire() ;

    /** 
     * Courtesy storage (but required) for the <code>DigitalScheduler</code>. 
     * @param _index the index associated with this event in the queue.
     * @see DigitalScheduler
     **/
    void setIndex(int _index) ;

    /** 
     * @return the previously stored value (via <code>setIndex</code>).
     * Defaults to -1, which indicates that the event is not currently queued.
     **/
    /*@ pure @*/ int getIndex() ;

    /**
     * @return the time at which the event should fire, for non-random events.
     **/
    /*@ pure @*/ long getTime();

    /** 
     * @return a true value indicates that the event should fire at a random
     * unspecified time.
     **/
    /*@ pure @*/ boolean isRandom();

} // end of interface Event

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */

