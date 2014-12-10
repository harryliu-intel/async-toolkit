/*
 * Copyright 2000 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.Event;
import com.avlsi.util.debug.Debug;

/**
 * <p> Class for an <code>Event</code> that indicates to TSim to exit. </p>
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 26 July 2002.
 **/

public class ExitEvent implements Event {

    // Attributes

    private long time;
    private int index = -1;

    // Constructors

    public ExitEvent(long time) {
        this.time = time;
    }

    // Public Methods

    public long getTime() { return time; }
    public void setIndex(int _index) { index = _index; }
    public int getIndex() { return index; }

    // unimplemented methods
    public void fire() { Debug.unimplemented(); }
    public boolean isRandom() { 
        Debug.unimplemented();
        return false;
    }

} // end of class ExitEvent

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
