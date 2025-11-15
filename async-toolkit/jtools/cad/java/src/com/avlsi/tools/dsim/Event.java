// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
     * @return the sub-time at which the event should fire, for non-random events.
     **/
    /*@ pure @*/ default int getSubTime() {
        return 0;
    }

    /** 
     * @return a true value indicates that the event should fire at a random
     * unspecified time.
     **/
    /*@ pure @*/ boolean isRandom();

    static long compare(final long ta, final long sta,
                        final long tb, final long stb) {
        long delta = ta - tb;
        if (delta == 0) {
            delta = sta - stb;
        }
        return delta;
    }

    static long compare(final long ta, final long sta, final Event b) {
        return compare(ta, sta, b.getTime(), b.getSubTime());
    }

    static long compare(final Event a, final long tb, final long stb) {
        return compare(a.getTime(), a.getSubTime(), tb, stb);
    }

    static long compare(final Event a, final Event b) {
        return compare(a.getTime(), a.getSubTime(),
                       b.getTime(), b.getSubTime());
    }

} // end of interface Event

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */

