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

import com.avlsi.util.debug.Debug;

/**
 * Class for wrapping native handles to transactions
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class Transaction {

    private NativeHandle handle=null;

    /** The Fiber this Transaction is on**/
    private final TransactionFiber fiber;
    
    public Transaction(TransactionFiber fiber) {
        this.fiber = fiber;
    }

    /**
     * Constructor.
     **/
    public Transaction(TransactionFiber fiber, long handle) {
        this.fiber = fiber;
        this.handle = new NativeHandle(handle);
    }

    public long getHandle() { return (handle == null)?0:handle.getHandle(); }

    public TransactionFiber getFiber() { return fiber; }
    
    public void setHandle(long handle) {
        Debug.assertTrue(this.handle == null);
        this.handle = new NativeHandle(handle);
    }
}

