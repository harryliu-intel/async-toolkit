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
 * Increment or decrement statement.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class IncDecStatement
    extends AbstractASTNode
    implements StatementInterface {

    /**
     * Expression to increment or decrement.  May not be null.
     **/
    private final ExpressionInterface expr;

    /**
     * <code>true</code> if increment, <code>false</code> if decrement.
     **/
    private final boolean inc;

    /**
     * Class constructor.
     *
     * @param expr expression to increment or decrement.  Not null.
     * @param inc <code>true</code> if increment, <code>false</code> if
     * decrement.
     **/
    public IncDecStatement(final ExpressionInterface expr, final boolean inc) {
        this.expr = expr;
        this.inc = inc;
    }

    /**
     * Returns the expression to increment or decrement.
     *
     * @return expression to modify, not null
     **/
    public ExpressionInterface getExpression() {
        return expr;
    }

    /**
     * Return <code>true</code> if increment, <code>false</code> if decrement.
     **/
    public boolean isIncrement() {
        return inc;
    }

    /**
     * Represent the increment and decrement as an assignment statement.
     **/
    public AssignmentStatement getAssignmentStatement() {
        return (AssignmentStatement)
            new AssignmentStatement(expr, new IntegerExpression(1),
                inc ? AssignmentStatement.ADD
                    : AssignmentStatement.SUBTRACT).epr(this);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitIncDecStatement(this);
    }
}
