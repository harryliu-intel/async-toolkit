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
 * Deterministic repetition statements.
 * Ie <code>*[e1 -&gt; s1 [] e2 -&gt; s2 [] ...] </code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class DeterministicRepetitionStatement
    extends AbstractRepetitionStatement {
    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitDeterministicRepetitionStatement(this);
    }
}
