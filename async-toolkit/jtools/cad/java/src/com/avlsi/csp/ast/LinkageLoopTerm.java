/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

import com.avlsi.util.exception.AssertionFailure;

/**
 *
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class LinkageLoopTerm extends AbstractLoop
    implements LinkageTermInterface {

    private final LinkageTermInterface term;

    public LinkageLoopTerm(final String indexVar,
            final Range range,
            final LinkageTermInterface term) {
        // pass -1 as loop type, it doesn't matter
        super(indexVar, range, -1);
        this.term = term;
    }

    public LinkageTermInterface getTerm() {
        return term;
    }

    public int getSeparator() {
        throw new AssertionFailure();
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitLinkageLoopTerm(this);
    }
}
