/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.prs;

import com.avlsi.cell.CellInterface;

/**
 * Exception thrown when the production rule cannot be synthesized.
 *
 * @author Aaron Denney
 * @version $Date$
 **/
public final class UnimplementableProductionRuleException extends Exception {
    /**
     * The production rule that cannot be implemented.
     **/
    public final ProductionRule rule;
    /**
     * The cell the production rule is in.  Helps to find the problem.
     **/
    public final CellInterface  cell;
    /**
     * Constructor.
     * @param p The production rule that cannot be implemented.  May not be null.
     * @param c The cell the production rule is located in.  May be null.
     **/
    public UnimplementableProductionRuleException(ProductionRule p, CellInterface c) {
        super("Production rule " + p + (c==null ? "" : " in cell " + c.getFullyQualifiedType()) + " is not implementable.");
        rule = p;
        cell = c;
    }
}

