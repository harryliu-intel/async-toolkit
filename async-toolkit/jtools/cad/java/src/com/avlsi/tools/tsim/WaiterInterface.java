/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

/**
 * <p> Interface for objects which may be waiting on a <code>Waitable</code>.
 * Used by channels: if a channel is completely empty or completely full the
 * receiving/sending object may be waiting for a token to arrive/leave before it
 * can continue.  When the channel does change state it needs to notify that
 * object. </p>
 *
 * @author Patrick Pelletier
 * @version $Revision$ $Date$
 **/

public interface WaiterInterface {

    /**
     * @todo Undocumented.
     **/

    void wakeUp();

} // end of interface WaiterInterface

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
