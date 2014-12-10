/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.sigscan;

/**
 * Class for adding link events to the scheduler
 *
 * @author Dan Daly
 * @version $Date$
 **/

class LinkEvent extends LogEvent {
    public static final int PRED = -1;
    public static final int SUCC = 1;
    public static final int BOTH = 0;
    
    private final Sigscan sigscan;

    private final Transaction tran1, tran2;

    private final int relationship;
    
    /**
     * Constructor.
     **/
    LinkEvent(Sigscan sigscan, Transaction tran1, Transaction tran2,
                     int relationship, long time) {
        super(time);
        this.sigscan = sigscan;
        this.tran1 = tran1;
        this.tran2 = tran2;
        this.relationship = relationship;
    }

    protected void actionPerformed() {
        switch (relationship) {
          case PRED:    sigscan.linkPredecessor(tran1, tran2);
                        break;
          case SUCC:    sigscan.linkSuccessor(tran1, tran2);
                        break;
          default:      sigscan.link(tran1, tran2);
                        break;
        }
    }
}

