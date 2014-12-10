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
 * Abstract base class for loop expressions or statements,
 * <code>&lt; sep i : M..N : expr_or_stmt &gt;</code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class AbstractLoop
    extends AbstractASTNode {
    /** Loop index variable. May not be null. **/
    private final IdentifierExpression indexVar;

    /** Range of variable for loop execution. May not be null. **/
    private final Range range;

    /**
     * Indicates operator that is used to combine loop bodies.
     * Only given meaning by {@link LoopStatement} or
     * {@link LoopExpression}.
     **/
    private final int separator;

    /**
     * Class constructor.
     *
     * @param indexVar loop index variable, not null
     * @param range minimum and maximum of range for loop index, not null
     * @param separator  separator code
     **/
    public AbstractLoop(final String indexVar, final Range range,
            final int separator) {
        this.indexVar = new IdentifierExpression(indexVar);
        this.range = range;
        this.separator = separator;
    }

    /**
     * Returns the loop index variable as a string.
     *
     * @return loop index variable, not null
     **/
    public String getIndexVar() {
        return indexVar.getIdentifier();
    }

    /**
     * Returns the loop index variable as an <code>IdentifierExpression</code>.
     *
     * @return loop index variable, not null
     **/
    public IdentifierExpression getIndexVarExpression() {
        return indexVar;
    }

    /**
     * Returns the range for the loop index.
     *
     * @return range for loop index, not null
     **/
    public Range getRange() {
        return range;
    }

    /**
     * Returns the separator for loop bodies.  Only given meaning by
     * {@link LoopStatement} or {@link LoopExpression}.
     *
     * @return separator code
     **/
    public int getSeparator() {
        return separator;
    }
}
