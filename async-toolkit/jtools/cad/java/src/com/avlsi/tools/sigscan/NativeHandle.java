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
 * Class for holding native handles.  The native memory
 * will be freed when this object is garbage collected,
 * but the native object's destructor won't be called.  You'll
 * need a special JNI call if you want to do that.
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class NativeHandle extends ObjectHandle{

    /**
     * Constructor.
     **/
    public NativeHandle(long handle) { super(handle); }

    public void finalize() { 
        if (handle != 0) {
            //System.out.println("Freeing Handle "+handle);
            //Sigscan.freeHandle(this);
        }
    } 
}
