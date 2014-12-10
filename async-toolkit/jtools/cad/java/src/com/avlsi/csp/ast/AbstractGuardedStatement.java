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
 * Abstract base class for deterministic and non-deterministic selection
 * and repetition statements.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public abstract class AbstractGuardedStatement
    extends AbstractASTNode
    implements StatementInterface {

    /**
     * List of {@link GuardedCommandInterface}s, for the guard and action.  
     * May not be null.
     **/
    private final List<GuardedCommandInterface> guardedCommands;

    /**
     * Statement for <code>else</code> case.  May be null if there is no
     * <code>else</code>.
     **/
    private StatementInterface elseStatement;

    /**
     * Class constructor.  Constructs a guarded statement with
     * an empty guarded command list and no <code>else</code> statement.
     **/
    public AbstractGuardedStatement() {
        guardedCommands = new ArrayList<GuardedCommandInterface>();
        elseStatement = null;
    }

    /**
     * Returns an Iterator over guard expressions and action statements.
     *
     * @return Iterator of {@link GuardedCommandInterface}s, not null
     **/
    public Iterator<GuardedCommandInterface> getGuardedCommands() {
        return guardedCommands.iterator();
    }

    /**
     * Appends a guard and action to the list.
     *
     * @param guardedCommand  guarded command to append to list,
     *     may not be null
     **/
    public void addGuardedCommand(
            final GuardedCommandInterface guardedCommand) {
        guardedCommands.add(guardedCommand);
    }

    /**
     * Returns statement corresponding to <code>else</code> guard.
     *
     * @return action statement for <code>else</code> guard or null if there
     *     is none
     **/
    public StatementInterface getElseStatement() {
        return elseStatement;
    }

    /**
     * Sets <code>else</code> statement to <code>elseStatement</code>,
     * replacing any existing elseStatement.
     *
     * @param elseStatement  action statement for <code>else</code> guard,
     *     not null.
     **/
    public void addElseStatement(final StatementInterface elseStatement) {
        this.elseStatement = elseStatement;
    }
}
