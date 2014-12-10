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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Abstract base class for sequential and parallel statements.
 * Keeps a list of statements.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public abstract class AbstractCompositeStatement
    extends AbstractASTNode
    implements StatementInterface {

    /** List of {@link StatementInterface}s.  May not be null.  **/
    private final List<StatementInterface> statements;

    /**
     * Class constructor.  Creates a composite statement without
     * any statements in its statement list.
     **/
    public AbstractCompositeStatement() {
        statements = new ArrayList<StatementInterface>();
    }

    /**
     *  Adds a statement to the tail of the statement list.
     * 
     * @param stmt  statement to add to the tail of the list,
     *     may not be null
     **/
    public void addStatement(final StatementInterface stmt) {
        statements.add(stmt);
    }

    /**
     * Returns an iterator through the statements in the statement list.
     * Will not return null.
     * 
     * @return Iterator of {@link StatementInterface}s, not null
     **/
    public Iterator<StatementInterface> getStatements() {
        return statements.iterator();
    }
}
