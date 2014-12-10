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
 * Class for building end transaction events
 *
 * @author Dan Daly
 * @version $Date$
 **/

class EndTranEvent extends LogEvent{

    protected final Sigscan sigscan;
    
    protected final TransactionFiber fiber;

    protected final Transaction tran;
    
    /**
     * Constructor.
     **/
    EndTranEvent(Sigscan sigscan, TransactionFiber fiber, long time) {
        this(sigscan, fiber, null, time);
    }
    
    public EndTranEvent(Sigscan sigscan, TransactionFiber fiber,
                        Transaction tran, long time) {
        super(time);
        this.fiber = fiber;
        this.sigscan = sigscan;
        this.tran = tran;
    }

    protected void actionPerformed() { 
        /*try {
            sigscan.convertAndSetTime(time);
        } catch (SigscanException e) { fiber.setTimeError(e); }
        */
        if (tran == null) sigscan.endLastTransaction(fiber);
        else              sigscan.endTransaction(fiber, tran);
        //if (notifyFlag) { fiber.doNotify(); }
    }

    public String toString() { 
        return "EndTranEvent on fiber "+fiber.getFullname();
    }

}

