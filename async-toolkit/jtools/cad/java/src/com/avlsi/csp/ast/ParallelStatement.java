/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.ast;

/**
 * Parallel statements.  Represents <code>s1 || s2</code> or
 * <code>s1, s2</code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class ParallelStatement extends AbstractCompositeStatement {

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitParallelStatement(this);
    }
}
