/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

/**
 * <p> Interface for watching for clock events. </p>
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public interface ClockWatcher extends BusWatcher {

    /** Called on a rising edge **/
    void risingEdge(SharedBus bus, long time);

    /** Called on a falling edge **/
    void fallingEdge(SharedBus bus, long time);

} // end of interface ClockWatcher

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */


