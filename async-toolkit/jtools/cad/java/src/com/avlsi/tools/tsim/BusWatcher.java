/*
 * Copyright 2000 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.tsim;

import java.util.EventListener;

/**
 * <p> Interface for watching for events on a bus. </p>
 *
 * @author Dan Daly
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @see SharedBus
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public interface BusWatcher extends EventListener {
    
    /** 
     * Called when any of the pins on the <code>SharedBus</code> change.
     *
     * @pre bus != null
     * @pre time >= 0
     **/

    /*@ public normal_behavior
      @   requires bus != null;
      @   requires time >= 0;
      @*/

    void busChanged(SharedBus bus, long time);

} // end of interface BusWatcher

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
