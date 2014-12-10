/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.csp2java.runtime;

/**
 * Programs generated with csp2java can call these methods to
 * notify something else (ie csp2tt) of their progress.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public interface CspSnoopingInterface {

    /**
     * Called when the go method starts.
     **/
    void onStart();

    /**
     * Called immediately before executing the first statement in the
     * outermost loop every time it is reached.
     **/
    void onOutermostLoopStart();

    /**
     * Called immediately before the go method returns.
     **/
    void onEnd();

    /**
     * Called when a deadlock would have occurred.
     **/
    void onDeadlock(String message, String whereAmI);

    /**
     * Called when arbitration would have been required.
     **/
    void onArbitrate(String message, String whereAmI);

    /**
     * Called on out of bounds array access.
     **/
    void onOutOfBoundsAccess(String message, String whereAmI);
}
