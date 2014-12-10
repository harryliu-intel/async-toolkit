/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.csp.ast;

/**
 * Loop statements, <code>&lt; sep i : M..N : stmt &gt;</code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class LoopStatement extends AbstractLoop
    implements StatementInterface {

    /** Sequential composition of loop bodies constant.  **/
    public static final int SEQUENTIAL = 0;

    /** Parallel composition of loop bodies constant.  **/
    public static final int PARALLEL = 1;

    /** Loop body statement, not null.  **/
    private final StatementInterface stmt;

    /**
     * Class constructor.
     *
     * @param indexVar loop index variable, not null
     * @param range minimum and maximum of range for loop index, not null
     * @param separator  separator code, one of {@see #SEQUENTIAL},
     *     {@see #PARALLEL}
     * @param stmt loop body statement
     **/
    public LoopStatement(final String indexVar,
            final Range range,
            final int separator,
            final StatementInterface stmt) {
        super(indexVar, range, separator);
        this.stmt = stmt;
    }

    /**
     * Returns body statement.
     *
     * @return body statement, not null
     **/
    public StatementInterface getStatement() {
        return stmt;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitLoopStatement(this);
    }
}
