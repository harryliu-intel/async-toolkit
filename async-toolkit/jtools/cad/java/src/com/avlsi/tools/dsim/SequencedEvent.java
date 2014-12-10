/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.dsim;

/**
 * @todo Undocumented.
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 **/

public interface SequencedEvent extends Event {

    /** 
     * @return true if this event has another event scheduled to come after it
     * at the same time.
     **/
    /*@ pure @*/ boolean hasNextEvent();

    /** 
     * @return the next event scheduled at this time.  The ordering of events
     * scheduled at the same time is determined by the order in which they were
     * added to the scheduler.
     **/
    SequencedEvent getNextEvent();

    /**
     * @param nextEvent the event to follow this one in sequence time.
     **/
    void setNextEvent(SequencedEvent nextEvent);

} // end of interface SequencedEvent

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */

