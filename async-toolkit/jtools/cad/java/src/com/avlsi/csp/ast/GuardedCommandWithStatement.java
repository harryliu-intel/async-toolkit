/*
 * Copyright 2008 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

/**
 * A type-safe pair of guard and command with additional statements that are
 * needed to evaluate guard.  The guard is an expression, but to simplify
 * translation, we may want to pull out function calls, assign function return
 * values to temporary variables, then use the temporary variables in place of
 * the original function calls.  This rewriting procedure would generate
 * <code>VarStatement</code>s as well as <code>AssignmentStatement</code>s,
 * which are not expressions.
 *
 * This class is not instantiated by the parser because there is no
 * corresponding CSP construct.  It exists to help
 * <code>FunctionPreprocessor</code> and <code>VerilogEmitter</code>.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public final class GuardedCommandWithStatement extends GuardedCommand {
    /** Additional statement needed to evaluate guard. **/
    private final StatementInterface guardStatement;

    /**
     * Class constructor.
     *
     * @param guard  Guard expression, not null
     * @param linkageTerms  Linkage terms for linked arbitration, may be null
     * @param command  Action command, not null
     * @param guardStmt  Additional statement needed to evaluate guard,
     *                   not null
     **/
    public GuardedCommandWithStatement(final ExpressionInterface guard,
                                       final LinkageTerms linkageTerms,
                                       final StatementInterface command,
                                       final StatementInterface guardStmt) {
        super(guard, linkageTerms, command);
        this.guardStatement = guardStmt;
    }

    /**
     * Returns statement needed to evaluate guard.
     *
     * @return  statement needed to evaluate guard, not null
     **/
    public StatementInterface getGuardStatement() {
        return guardStatement;
    }
}
