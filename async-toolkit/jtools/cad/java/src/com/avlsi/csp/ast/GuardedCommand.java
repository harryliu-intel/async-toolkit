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
 * A type-safe pair of guard and command.
 * Represents <code>expr -&gt; stmt</code> portion of repetition or
 * selection statement.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class GuardedCommand 
    extends AbstractASTNode
    implements GuardedCommandInterface {

    /** Guard expression, not null.  **/
    private final ExpressionInterface guard;

    /** Node linkage terms for linked arbitration.  May be null.  **/
    private final LinkageTerms linkageTerms;

    /** Command statement, not null.  **/
    private final StatementInterface command;

    /**
     * Class constructor.
     *
     * @param guard  Guard expression, not null
     * @param linkageTerms  Linkage terms for linked arbitration,
     *     may be null
     * @param command  Action command, not null
     **/
    public GuardedCommand(final ExpressionInterface guard,
            final LinkageTerms linkageTerms,
            final StatementInterface command) {
        this.guard = guard;
        this.linkageTerms = linkageTerms;
        this.command = command;
    }

    /**
     * Class constructor.
     *
     * @param guard  Guard expression, not null
     * @param command  Action command, not null
     **/
    public GuardedCommand(final ExpressionInterface guard,
            final StatementInterface command) {
        this(guard, null, command);
    }

    /**
     * Returns guard expression.
     *
     * @return  guard expression, not null
     **/
    public ExpressionInterface getGuard() {
        return guard;
    }

    /**
     * Returns node linkage terms for linked arbitration.
     *
     * @return  node linkage terms, may be null
     **/
    public LinkageTerms getLinkageTerms() {
        return linkageTerms;
    }

    /**
     * Returns action statement to be exectued if guard expression is
     * true.
     *
     * @return action statement, not null
     **/
    public StatementInterface getCommand() {
        return command;
    }
}
