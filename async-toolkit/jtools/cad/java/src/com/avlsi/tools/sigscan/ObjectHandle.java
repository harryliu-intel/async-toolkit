/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.sigscan;

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

abstract class ObjectHandle {

    protected final long handle;
    /**
     * Constructor.
     **/
    public ObjectHandle(long handle) {

        this.handle = handle;
    }

    public abstract void finalize();

    public long getHandle() { return handle; }
}

